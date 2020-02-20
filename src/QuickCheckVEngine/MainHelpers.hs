--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018-2020 Alexandre Joannou
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- This software was partly developed by the University of Cambridge
-- Computer Laboratory as part of the Partially-Ordered Event-Triggered
-- Systems (POETS) project, funded by EPSRC grant EP/N031768/1.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
--

{-# LANGUAGE ScopedTypeVariables #-}

module QuickCheckVEngine.MainHelpers (
  readDataFile
, genInstrServer
, prop
) where

import Numeric
import System.IO
import System.IO.Unsafe
import System.Timeout
import Data.IORef
import Data.Maybe
import Data.Binary
import Network.Socket
import Network.Socket.ByteString.Lazy
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as BS

import qualified InstrCodec
import RISCV hiding (and)
import QuickCheckVEngine.RVFI_DII
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils

-- | Turns a file representation of some data into a 'TestCase' that initializes
--   memory with that data
readDataFile :: FilePath -> IO TestCase
readDataFile inFile = do
  handle <- openFile inFile ReadMode
  contents <- hGetContents handle
  testCase <- generate $ readData (lines contents)
  putStrLn $ show (testCaseInstCount testCase)
  return testCase
  where readData ss = genTemplateUnsized $
             (writeData addr ws)
          <> (li32 1 0x80000000)
          <> (Single $ InstrCodec.encode jalr 0 1 0)
          where (addr:ws) = map (fst . head . readHex . head . words) ss

-- | Retrieve an instruction from 'Socket' to an external instruction server
genInstrServer :: Socket -> Gen Integer
genInstrServer sckt = do
  seed :: Word32 <- arbitraryBoundedRandom
  -- This should be safe so long as the server returns the same instruction when
  -- given the same seed.
  return $ unsafePerformIO $ do sendAll sckt (encode seed)
                                (decode . BS.reverse) <$> recv sckt 4

-- | The core QuickCheck property sending the 'TestCase' to the tested RISC-V
--   implementations as 'DII_Packet's and checking the returned 'RVFI_Packet's
--   for equivalence
prop :: Socket -> Socket -> IORef Bool -> (TestCase -> IO ())
     -> ArchDesc -> Int -> Bool -> Gen TestCase
     -> Property
prop socA socB alive onFail arch timeoutDelay doLog gen =
  forAllShrink gen shrink mkProp
  where mkProp testCase = whenFail (onFail testCase) (doProp testCase)
        doProp testCase = monadicIO $ run $ do
          let instTrace = map diiInstruction $ fromTestCase testCase
          let instTraceTerminated = instTrace ++ [diiEnd]
          currentlyAlive <- readIORef alive
          if currentlyAlive then do
            result <- try $ do
              -- Send to implementations
              sendDIITrace socA instTraceTerminated
              when doLog $ putStrLn "Done sending instructions to implementation A"
              sendDIITrace socB instTraceTerminated
              when doLog $ putStrLn "Done sending instructions to implementation B"
              -- Receive from implementations
              m_modTrace <- timeout timeoutDelay $ recvRVFITrace socA doLog
              m_impTrace <- timeout timeoutDelay $ recvRVFITrace socB doLog
              --
              let diffStrings = zipWith (rvfiShowCheck $ has_xlen_64 arch) (fromMaybe (error "broken modtrace") m_modTrace) (fromMaybe (error "broken imptrace") m_impTrace)
              when doLog $ mapM_ putStrLn diffStrings
              return (m_modTrace, m_impTrace)
            case result of
              Right (Just modTrace, Just impTrace) ->
                return $ property $ and (zipWith (rvfiCheck (has_xlen_64 arch)) modTrace impTrace)
              Right (a, b) -> do
                writeIORef alive False
                when (isNothing a) $ putStrLn "Error: implementation A timeout. Forcing all future tests to report 'SUCCESS'"
                when (isNothing b) $ putStrLn "Error: implementation B timeout. Forcing all future tests to report 'SUCCESS'"
                return $ property False
              Left (SomeException e) -> do
                writeIORef alive False
                putStrLn "Error: exception on IO with implementations. Forcing all future tests to report 'SUCCESS'"
                return $ property False
          else do
            when doLog $ putStrLn "Warning: reporting success since implementations not running"
            return $ property True -- We don't want to shrink once one of the implementations has died, so always return that the property is true
