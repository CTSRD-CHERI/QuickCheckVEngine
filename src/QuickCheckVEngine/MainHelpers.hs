{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
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
, runImpls
, wrapTest
, showTraceInput
, showAnnotatedTrace
, prop
) where

import Numeric
import System.IO
import System.IO.Unsafe
import System.Timeout
import Data.IORef
import Data.Maybe
import Data.Binary
import Data.Time.Clock
import Network.Socket
import Network.Socket.ByteString.Lazy
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad
import Control.Exception (try, SomeException(..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set

import qualified InstrCodec
import RISCV hiding (and, or)
import QuickCheckVEngine.RVFI_DII
import QuickCheckVEngine.Test
import qualified QuickCheckVEngine.Template as T
import QuickCheckVEngine.Templates.Utils.General

instance Show DII_Packet where
  show (DII_End _) = "# Test end"
  show (DII_Instruction _ x) = show $ MkInstruction (toInteger x)
  show _ = ""

instance {-# OVERLAPPING #-} Show (Test TestResult) where
  show = showTraceInput

showTraceInput t = show ((\(x, _, _) -> x) <$> t)

showAnnotatedTrace singleImp arch verbosity t = showTestWithComments t (\(x, _, _) -> show x) (\(_, a, b) -> Just . unlines . (("# " ++) <$>) . lines . (\(a, b) -> b) $ rvfiCheckAndShow True singleImp (has_xlen_64 arch) verbosity a b [])

bypassShrink :: ShrinkStrategy
bypassShrink = sequenceShrink f'
  where f' :: Test TestResult -> Test TestResult -> [Test TestResult]
        f' a b = foldr f [] a
          where f (DII_Instruction _ x, _, _) = ((if is_bypass then ((a <>) <$> (singleShrink (s (def0 m_rd_x) (def0 m_rs1_x)) b)) else []) ++)
                  where (is_bypass, _, m_rs1_x, m_rd_x, _) = rv_extract . MkInstruction . toInteger $ x
                        s old new (DII_Instruction t i, ra, rb) = single <$>
                             [ (DII_Instruction t . fromInteger . unMkInstruction $ reencode_i (def0 m_rs2_i) new (def0 m_rd_i), ra, rb)
                             | maybe False (== old) m_rs1_i ]
                          ++ [ (DII_Instruction t . fromInteger . unMkInstruction $ reencode_i new (def0 m_rs1_i) (def0 m_rd_i), ra, rb)
                             | maybe False (== old) m_rs2_i ]
                          where (_, m_rs2_i, m_rs1_i, m_rd_i, reencode_i) = rv_extract . MkInstruction . toInteger $ i
                        s _ _ _ = []
                        def0 (Just x) = x
                        def0 Nothing = 0
                f _ = id

instShrink :: ShrinkStrategy
instShrink = singleShrink f'
  where f' (diiPkt, a, b) = wrap <$> rv_shrink (MkInstruction . toInteger . dii_insn $ diiPkt)
          where wrap (MkInstruction x) = single (diiPkt { dii_insn = fromInteger x }, a, b)

-- | Turns a file representation of some data into a 'Test' that initializes
--   memory with that data.
--   Format:
--   0xADDR0 0xDATA 0xDATA 0xDATA...
--   0xADDR1 0xDATA 0xDATA 0xDATA...
--   Example:
--   80000000 13050000 ef008014 13051000 ef000014
--   80000010 13052000 ef008013 13053000 ef000013
readDataFile :: T.TestParams -> FilePath -> IO (Test Instruction)
readDataFile params inFile = do
  handle <- openFile inFile ReadMode
  contents <- hGetContents handle
  test <- generate $ readData (lines contents)
  putStrLn $ show (length test)
  return test
  where readData ss = T.genTest params $
          mconcat (map (\(addr:ws) -> writeData addr ws) write_args)
          <> (li64 1 0x80000000)
          <> (T.inst $ jalr 0 1 0)
          where write_args = map (map (fst . head . readHex) . words) ss

-- | Retrieve an instruction from 'Socket' to an external instruction server
genInstrServer :: Socket -> Gen Integer
genInstrServer sckt = do
  seed :: Word32 <- arbitraryBoundedRandom
  -- This should be safe so long as the server returns the same instruction when
  -- given the same seed.
  return $ unsafePerformIO $ do sendAll sckt (encode seed)
                                (decode . BS.reverse) <$> Network.Socket.ByteString.Lazy.recv sckt 4

wrapTest :: Test Instruction -> Test TestResult
wrapTest = (<> single (diiEnd, Nothing, Nothing))
         . (flip shrinkStrategy defaultShrink)
         . (flip shrinkStrategy instShrink)
         . (flip shrinkStrategy bypassShrink)
         . addShrinkScopes
         . balance
         . removeEmpties
         . (f <$>)
  where f (MkInstruction i) = (diiInstruction i, Nothing, Nothing)

runImpls :: RvfiDiiConnection         -- ^ Implementation A connection
         -> Maybe RvfiDiiConnection   -- ^ Implementation B connection
         -> IORef Bool                -- ^ Implementations still alive?
         -> Int                       -- ^ RVFI-DII delay
         -> Int                       -- ^ Verbosity
         -> Maybe FilePath            -- ^ Optional save directory for failed tests
         -> Test TestResult           -- ^ Test to run
         -> (Test TestResult -> IO a) -- ^ Callback: report from implementations
         -> (Test DII_Packet -> IO a) -- ^ Callback: first implementation disconnect
         -> (Test DII_Packet -> IO a) -- ^ Callback: already disconnected implementations
         -> IO a
runImpls connA m_connB alive delay verbosity saveDir test onTrace onFirstDeath onSubsequentDeaths = do
  let instTrace = (\(x, _, _) -> x) <$> test
  currentlyAlive <- readIORef alive
  if currentlyAlive then do
    m_trace <- doRVFIDII connA m_connB alive delay verbosity saveDir test
    case m_trace of
      Just trace -> onTrace trace
      _ -> onFirstDeath instTrace
  else onSubsequentDeaths instTrace

data TestWithSeen = MkTestWithSeen { test :: Test TestResult, seen :: Set.Set (Test TestResult) }

instance Show TestWithSeen where
   show = show . test

-- | The core QuickCheck property sending the 'Test' to the tested RISC-V
--   implementations as 'DII_Packet's and checking the returned 'RVFI_Packet's
--   for equivalence. It receives among other things a callback function
--   'Test -> IO ()' to be performed on failure that takes in the reduced
--   'Test' which caused the failure
prop :: RvfiDiiConnection             -- ^ Implementation A connection
     -> Maybe RvfiDiiConnection       -- ^ Implementation B connection
     -> IORef Bool                    -- ^ Implementations still alive?
     -> (Test TestResult -> IO ())    -- ^ Callback on falsification
     -> ArchDesc                      -- ^ Archictecture description
     -> Int                           -- ^ RVFI-DII Delay
     -> Int                           -- ^ Verbosity
     -> Maybe FilePath                -- ^ Optional save directory for failed tests
     -> Bool                          -- ^ Ignore embedded asserts in tests
     -> Bool                          -- ^ Strict RVFI response comparison
     -> Gen (Test TestResult)         -- ^ Test generator
     -> Property
prop connA m_connB alive onFail arch delay verbosity saveDir ignoreAsserts strict gen =
  forAllShrink genDedup shrinkTestDedup mkPropDedup
  where mkPropDedup t = mkProp (test t)
        genDedup = (\t -> MkTestWithSeen t (Set.singleton t)) <$> gen
        shrinkTestDedup t = map (\t' -> MkTestWithSeen t' (Set.insert t' (seen t))) (filter (flip Set.notMember (seen t)) (shrinkTest (test t)))
        mkProp test = whenFail (onFail test) (doProp test)
        doProp test = monadicIO $ run $ runImpls connA m_connB alive delay verbosity saveDir test onTrace onFirstDeath onSubsequentDeaths
        colourGreen = "\ESC[32m"
        colourRed = "\ESC[31m"
        colourEnd = "\ESC[0m"
        colourise (b, s) = (b, (if b then colourGreen else colourRed) ++ s ++ colourEnd)
        diffFunc asserts (DII_Instruction _ _, a, b) = colourise $ rvfiCheckAndShow strict (isNothing m_connB) (has_xlen_64 arch) verbosity a b asserts
        diffFunc _ (DII_End _, _, _) = (True, "Test end")
        diffFunc _ _ = (True, "")
        handleAsserts (ReportAssert False s, _) = do putStrLn $ "Failed assert: " ++ s
                                                     return True
        handleAsserts                         _ = return False
        onTrace trace = do
          let diff = mapWithAssertLastVal diffFunc trace
          when (verbosity > 1) $ mapM_ (putStrLn . snd) diff
          assertsFailed <- forM (gatherReports $ runAssertCompounds trace) handleAsserts
          return $ property $ and (fst <$> diff) && (ignoreAsserts || not(or assertsFailed))
        onFirstDeath _ = return $ property False
        -- We don't want to shrink once one of the implementations has died,
        -- so always return that the property is true
        onSubsequentDeaths _ = do
          return $ property Discard

-- | Send a sequence of instructions ('[DII_Packet]') to the implementations
--   running behind the two provided 'Sockets's and recieve their respective
--   RVFI traces ('[RVFI_Packet]'). If all went well, return
--   'Just (traceA, traceB)', otherwise 'Nothing' and sets the provided
--   'IORef Bool' for alive to 'False' indicating that further interaction with
--   the implementations is futile
doRVFIDII :: RvfiDiiConnection        -- ^ Implementation A connection
          -> Maybe RvfiDiiConnection  -- ^ Implementation B connection
          -> IORef Bool               -- ^ Implementations still alive?
          -> Int                      -- ^ RVFI-DII delay
          -> Int                      -- ^ Verbosity
          -> Maybe FilePath           -- ^ Optional save directory for failed tests
          -> Test TestResult          -- ^ Input instruction sequence
          -> IO (Maybe (Test TestResult))
doRVFIDII connA m_connB alive delay verbosity saveDir test = do
  let instTrace = (\(x, _, _) -> x) <$> test
  let insts = instTrace
  currentlyAlive <- readIORef alive
  if currentlyAlive then do
    result <- try $ do
      let doLog = verbosity > 1
      let emptyTrace = fmap (flip (,) Nothing)
      -- Send to implementations
      let send name conn = do sendDIITrace conn insts
                              when doLog $ putStrLn $ "Done sending instructions to " ++ name
      send "implementation A" connA
      maybe (pure ()) (send "implementation B") m_connB
      -- Receive from implementations
      let receive name base conn = do res <- timeout delay $ recvRVFITrace conn verbosity base
                                      when doLog $ putStrLn $ "Done receiving reports from " ++ name
                                      return res
      m_traceA <- receive "implementation A" insts connA
      let traceA = fromMaybe (emptyTrace insts) m_traceA
      m_traceAB <- maybe (return . Just $ emptyTrace traceA) (receive "implementation B" traceA) m_connB
      --
      when (isNothing m_traceA || isNothing m_traceAB) $ writeIORef alive False
      when (isNothing m_traceA) $ putStrLn "Error: implementation A timeout. Discarding all future tests."
      when (isNothing m_traceAB) $ putStrLn "Error: implementation B timeout. Discarding all future tests."
      --
      case saveDir of
        Nothing -> do return ()
        Just dir -> do
          t <- getCurrentTime
          let tstamp = [if x == ' ' then '_' else if x == ':' then '-' else x | x <- show t]
          let prelude = "# Automatically generated test case\n"
          writeFile (dir ++ "/random-test-" ++ tstamp ++ ".S") (prelude ++ (show test))
      --
      return $ fromMaybe (emptyTrace traceA) m_traceAB
    case result of
      Right t -> return $ Just $ (\((x,y),z) -> (x,y,z)) <$> t
      Left (SomeException e) -> do
        writeIORef alive False
        putStrLn ("Error: exception on IO with implementations. Discarding all future tests: " ++ show e)
        return Nothing
  else do
     putStrLn "Warning: doRVFIDII called when both implementations are not alive"
     return Nothing
