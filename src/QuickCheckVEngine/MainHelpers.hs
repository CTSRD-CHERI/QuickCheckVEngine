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
, doRVFIDII
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
                                (decode . BS.reverse) <$> Network.Socket.ByteString.Lazy.recv sckt 4

-- | Helper that zips two lists of potentially unequal lengths, padding the
--   shorter with a default element.
zipWithPadding :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithPadding a b f = go
  where
    go []     []     = []
    go []     (y:ys) = f a y : go [] ys
    go (x:xs) []     = f x b : go xs []
    go (x:xs) (y:ys) = f x y : go xs ys

-- | The core QuickCheck property sending the 'TestCase' to the tested RISC-V
--   implementations as 'DII_Packet's and checking the returned 'RVFI_Packet's
--   for equivalence. It receives among other things a callback function
--   'TestCase -> IO ()' to be performed on failure that takes in the reduced
--   'TestCase' which caused the failure
prop :: (Socket, Int) -> (Socket, Int) -> IORef Bool -> (TestCase -> IO ())
     -> ArchDesc -> Int -> Bool -> Gen TestCase
     -> Property
prop scktA scktB alive onFail arch delay doLog gen =
  forAllShrink gen shrink mkProp
  where mkProp testCase = whenFail (onFail testCase) (doProp testCase)
        doProp testCase = monadicIO $ run $ do
          let (rawInsts, asserts) = fromTestCase testCase
          let instTrace = map diiInstruction rawInsts
          let insts = instTrace ++ [diiEnd]
          currentlyAlive <- readIORef alive
          if currentlyAlive then do
            m_traces <- doRVFIDII scktA scktB alive delay doLog insts
            case m_traces of
              Just (traceA, traceB) -> do
                let diff = zipWithPadding rvfiEmptyHaltPacket rvfiEmptyHaltPacket
                                          (rvfiCheckAndShow $ has_xlen_64 arch)
                                          traceA traceB
                when doLog $ mapM_ (putStrLn . snd) diff
                let implAAsserts = asserts (init traceA)
                let implBAsserts = asserts (init traceB)
                mapM_ (\s -> putStrLn ("Impl A failed assert: " ++ s)) implAAsserts
                mapM_ (\s -> putStrLn ("Impl B failed assert: " ++ s)) implBAsserts
                return $ property $ (and (map fst diff)) && (null implAAsserts) && (null implBAsserts)
              _ -> return $ property False
          -- We don't want to shrink once one of the implementations has died,
          -- so always return that the property is true
          else do putStrLn "Warning: reporting success since implementations not running"
                  return $ property True

-- | Send a sequence of instructions ('[DII_Packet]') to the implementations
--   running behind the two provided 'Sockets's and recieve their respective
--   RVFI traces ('[RVFI_Packet]'). If all went well, return
--   'Just (traceA, traceB)', otherwise 'Nothing' and sets the provided
--   'IORef Bool' for alive to 'False' indicating that further interaction with
--   the implementations is futile
doRVFIDII :: (Socket, Int) -> (Socket, Int) -> IORef Bool -> Int -> Bool -> [DII_Packet]
          -> IO (Maybe ([RVFI_Packet], [RVFI_Packet]))
doRVFIDII (scktA, traceVerA) (scktB, traceVerB) alive delay doLog insts = do
  currentlyAlive <- readIORef alive
  if currentlyAlive then do
    result <- try $ do
      -- Send to implementations
      sendDIITrace scktA insts
      when doLog $ putStrLn "Done sending instructions to implementation A"
      sendDIITrace scktB insts
      when doLog $ putStrLn "Done sending instructions to implementation B"
      -- Receive from implementations
      m_traceA <- timeout delay $ recvRVFITrace (scktA, traceVerA) doLog
      when doLog $ putStrLn "Done receiving reports from implementation A"
      m_traceB <- timeout delay $ recvRVFITrace (scktB, traceVerB) doLog
      when doLog $ putStrLn "Done receiving reports from implementation B"
      --
      return (m_traceA, m_traceB)
    case result of
      Right (Just traceA, Just traceB) -> return $ Just (traceA, traceB)
      Right (a, b) -> do
        writeIORef alive False
        when (isNothing a) $ putStrLn "Error: implementation A timeout. Forcing all future tests to report 'SUCCESS'"
        when (isNothing b) $ putStrLn "Error: implementation B timeout. Forcing all future tests to report 'SUCCESS'"
        return Nothing
      Left (SomeException e) -> do
        writeIORef alive False
        putStrLn "Error: exception on IO with implementations. Forcing all future tests to report 'SUCCESS'"
        return Nothing
  else do
     putStrLn "Warning: doRVFIDII called when both implementations are not alive"
     return Nothing
