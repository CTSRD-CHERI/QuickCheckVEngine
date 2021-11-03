--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018-2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
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
import Control.Exception (try, SomeException(..))
import qualified Data.ByteString.Lazy as BS

import qualified InstrCodec
import RISCV hiding (and)
import QuickCheckVEngine.RVFI_DII
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils

-- | Turns a file representation of some data into a 'Test' that initializes
--   memory with that data.
--   Format:
--   0xADDR0 0xDATA 0xDATA 0xDATA...
--   0xADDR1 0xDATA 0xDATA 0xDATA...
--   Example:
--   80000000 13050000 ef008014 13051000 ef000014
--   80000010 13052000 ef008013 13053000 ef000013
readDataFile :: FilePath -> IO (Test Integer)
readDataFile inFile = do
  handle <- openFile inFile ReadMode
  contents <- hGetContents handle
  test <- generate $ readData (lines contents)
  putStrLn $ show (length test)
  return test
  where readData ss = genTest $
          sequenceTemplate (map (\(addr:ws) -> writeData addr ws) write_args)
          <> (li64 1 0x80000000)
          <> (inst $ jalr 0 1 0)
          where write_args = map (map (fst . head . readHex) . words) ss

-- | Retrieve an instruction from 'Socket' to an external instruction server
genInstrServer :: Socket -> Gen Integer
genInstrServer sckt = do
  seed :: Word32 <- arbitraryBoundedRandom
  -- This should be safe so long as the server returns the same instruction when
  -- given the same seed.
  return $ unsafePerformIO $ do sendAll sckt (encode seed)
                                (decode . BS.reverse) <$> Network.Socket.ByteString.Lazy.recv sckt 4

-- | Helper that zips three lists of potentially unequal lengths, padding the
--   shorter with a default element.
zipWithPadding :: a -> b -> c -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWithPadding a b c f = go
  where
    go []     []     []     = []
    go []     []     (z:zs) = f a b z : go [] [] zs
    go []     (y:ys) []     = f a y c : go [] ys []
    go []     (y:ys) (z:zs) = f a y z : go [] ys zs
    go (x:xs) []     []     = f x b c : go xs [] []
    go (x:xs) []     (z:zs) = f x b z : go xs [] zs
    go (x:xs) (y:ys) []     = f x y c : go xs ys []
    go (x:xs) (y:ys) (z:zs) = f x y z : go xs ys zs

-- | The core QuickCheck property sending the 'Test' to the tested RISC-V
--   implementations as 'DII_Packet's and checking the returned 'RVFI_Packet's
--   for equivalence. It receives among other things a callback function
--   'Test -> IO ()' to be performed on failure that takes in the reduced
--   'Test' which caused the failure
prop :: RvfiDiiConnection -> RvfiDiiConnection -> IORef Bool -> (Test Integer -> IO ())
     -> ArchDesc -> Int -> Int -> Bool -> Gen (Test Integer) -> Property
prop connA connB alive onFail arch delay verbosity ignoreAsserts gen =
  forAllShrink gen shrink mkProp
  where mkProp test = whenFail (onFail test) (doProp test)
        doProp test = monadicIO $ run $ do
          let (rawInsts, asserts) = fromTestCase testCase
          let instTrace = map diiInstruction (map fst rawInsts)
          let insts = instTrace ++ [diiEnd]
          currentlyAlive <- readIORef alive
          if currentlyAlive then do
            m_traces <- doRVFIDII connA connB alive delay verbosity insts
            case m_traces of
              Just (traceA, traceB) -> do
                let diff = zipWithPadding rvfiEmptyHaltPacket rvfiEmptyHaltPacket Nothing
                                          (rvfiCheckAndShow $ has_xlen_64 arch)
                                          traceA traceB (if ignoreAsserts then [] else map snd rawInsts)
                when (verbosity > 1) $ mapM_ (putStrLn . snd) diff
                let implAAsserts = asserts (init traceA)
                let implBAsserts = asserts (init traceB)
                mapM_ (\s -> putStrLn ("Impl A failed assert: " ++ s)) implAAsserts
                mapM_ (\s -> putStrLn ("Impl B failed assert: " ++ s)) implBAsserts
                return $ property $ (and (map fst diff)) && (ignoreAsserts || ((null implAAsserts) && (null implBAsserts)))
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
doRVFIDII :: RvfiDiiConnection -> RvfiDiiConnection -> IORef Bool -> Int
          -> Int -> [DII_Packet] -> IO (Maybe ([RVFI_Packet], [RVFI_Packet]))
doRVFIDII connA connB alive delay verbosity insts = do
  currentlyAlive <- readIORef alive
  if currentlyAlive then do
    result <- try $ do
      let doLog = verbosity > 1
      -- Send to implementations
      sendDIITrace connA insts
      when doLog $ putStrLn "Done sending instructions to implementation A"
      sendDIITrace connB insts
      when doLog $ putStrLn "Done sending instructions to implementation B"
      -- Receive from implementations
      m_traceA <- timeout delay $ recvRVFITrace connA verbosity
      when doLog $ putStrLn "Done receiving reports from implementation A"
      m_traceB <- timeout delay $ recvRVFITrace connB verbosity
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
        putStrLn ("Error: exception on IO with implementations. Forcing all future tests to report 'SUCCESS': " ++ show e)
        return Nothing
  else do
     putStrLn "Warning: doRVFIDII called when both implementations are not alive"
     return Nothing
