--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018-2020 Alexandre Joannou
-- Copyright (c) 2018-2020 Peter Rugg
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

module Main (main) where

import System.Exit
import System.Environment
import System.FilePath.Find
import System.Console.GetOpt
import Data.IORef
import Data.Maybe
import Data.Time.Clock
import Control.Monad
import Network.Socket
import Test.QuickCheck

import RISCV hiding (or)
import InstrCodec
import QuickCheckVEngine.MainHelpers
import QuickCheckVEngine.RVFI_DII
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import QuickCheckVEngine.Templates.GenAll
import QuickCheckVEngine.Templates.GenArithmetic
import QuickCheckVEngine.Templates.GenMemory
import QuickCheckVEngine.Templates.GenCSRs
import QuickCheckVEngine.Templates.RandomTest
import QuickCheckVEngine.Templates.GenControlFlow
import QuickCheckVEngine.Templates.GenMulDiv
import QuickCheckVEngine.Templates.GenCompressed
import QuickCheckVEngine.Templates.GenAtomics
import QuickCheckVEngine.Templates.GenFP
import QuickCheckVEngine.Templates.GenCHERI

-- command line arguments
--------------------------------------------------------------------------------
data Options = Options
    { optVerbosity   :: Int
    , nTests         :: Int
    , impAPort       :: String
    , impAIP         :: String
    , impBPort       :: String
    , impBIP         :: String
    , instTraceFile  :: Maybe FilePath
    , instDirectory  :: Maybe FilePath
    , memoryInitFile :: Maybe FilePath
    , arch           :: ArchDesc
    , instrPort      :: Maybe String
    , saveDir        :: Maybe FilePath
    , timeoutDelay   :: Int
    , testLen        :: Int
    , optShrink      :: Bool
    , optSave        :: Bool
    } deriving Show

defaultOptions = Options
    { optVerbosity   = 1
    , nTests         = 100
    , impAPort       = "5000"
    , impAIP         = "127.0.0.1"
    , impBPort       = "5001"
    , impBIP         = "127.0.0.1"
    , instTraceFile  = Nothing
    , instDirectory  = Nothing
    , memoryInitFile = Nothing
    , arch           = archDesc_rv32i
    , instrPort      = Nothing
    , saveDir        = Nothing
    , timeoutDelay   = 6000000000 -- 60 seconds
    , testLen        = 2048
    , optShrink      = True
    , optSave        = True
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['v']     ["verbose"]
      (ReqArg (\ f opts -> opts { optVerbosity = read f }) "VERB")
        "Set verbosity level"
  , Option ['n']     ["number-of-tests"]
      (ReqArg (\ f opts -> opts { nTests = read f }) "NUMTESTS")
        "Specify NUMTESTS the number of tests to run"
  , Option ['a']     ["implementation-A-port"]
      (ReqArg (\ f opts -> opts { impAPort = f }) "PORT")
        "Specify which PORT to use for implementation A"
  , Option ['A']     ["implementation-A-ip"]
      (ReqArg (\ f opts -> opts { impAIP = f }) "IP")
        "Specify which IP to use for implementation A"
  , Option ['b']     ["implementation-B-port"]
      (ReqArg (\ f opts -> opts { impBPort = f }) "PORT")
        "Specify which PORT to use for implementation B"
  , Option ['B']     ["implementation-B-ip"]
      (ReqArg (\ f opts -> opts { impBIP = f }) "IP")
        "Specify which IP to use for implementation B"
  , Option ['t']     ["trace-file"]
      (ReqArg (\ f opts -> opts { instTraceFile = Just f }) "PATH")
        "Specify PATH a trace file to use as the instruction trace to replay"
  , Option ['d']     ["trace-directory"]
      (ReqArg (\ f opts -> opts { instDirectory = Just f }) "PATH")
        "Specify PATH a directory which contains trace files to replay"
  , Option ['m']     ["memory-init-file"]
      (ReqArg (\ f opts -> opts { memoryInitFile = Just f }) "PATH")
        "Specify PATH a processed objdump file (e.g. 8000f420 2ed632d8 36da3adc 3edec2c0 c6c2cac4) to use as the initial contents of memory"
  , Option ['r']     ["architecture"]
      (ReqArg (\ f opts -> opts { arch = fromString f }) "ARCHITECTURE")
        "Specify ARCHITECTURE to be verified (e.g. rv32i)"
  , Option ['i']     ["instruction generator port"]
      (ReqArg (\ f opts -> opts { instrPort = Just f }) "PORT")
        "Connect to an external instruction generator on PORT"
  , Option ['s']     ["save-dir"]
      (ReqArg (\ f opts -> opts { saveDir = Just f }) "PATH")
        "Keep running, saving any new failures to files"
  , Option ['T']     ["timeout"]
      (ReqArg (\ f opts -> opts { timeoutDelay = read f }) "TIMEOUT")
        "Timeout after TIMEOUT microseconds of A or B not responding"
  , Option ['L']     ["test-length"]
      (ReqArg (\ f opts -> opts { testLen = read f }) "TEST-LENGTH")
        "Generate tests up to TEST-LENGTH instructions long"
  , Option ['S']     ["disable-shrink"]
      (NoArg (\ opts -> opts { optShrink = False }))
        "Disable shrinking of failed tests"
  , Option []        ["no-save"]
      (NoArg (\ opts -> opts { optSave = False }))
        "Don't offer to save failed counter-examples"
  ]

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: QCVEngine [OPTION...] files..."

--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ do
  -- parse command line arguments
  rawArgs <- getArgs
  (flags, leftover) <- commandOpts rawArgs
  when (optVerbosity flags > 1) $ print flags
  let archDesc = arch flags
  -- initialize model and implementation sockets
  addrA <- resolve (impAIP flags) (impAPort flags)
  addrB <- resolve (impBIP flags) (impBPort flags)
  socA <- open "implementation-A" addrA
  socB <- open "implementation-B" addrB
  sendDIIPacket socA diiEnd
  _ <- recvRVFITrace socA False
  sendDIIPacket socB diiEnd
  _ <- recvRVFITrace socB False
  addrInstr <- mapM (resolve "127.0.0.1") (instrPort flags)
  instrSoc <- mapM (open "instruction-generator-port") addrInstr
  --
  alive <- newIORef True -- Cleared when either implementation times out, since they will may not be able to respond to future queries
  let checkSingle tc verbosity doShrink len onFail = do
        quickCheckWithResult (Args Nothing 1 1 len (verbosity > 0) (if doShrink then 1000 else 0))
                             (prop socA socB alive onFail archDesc (timeoutDelay flags) (verbosity > 1) (return tc))
  let check_mcause_on_trap tc traceA traceB =
        if or (map rvfiIsTrap traceA) || or (map rvfiIsTrap traceB)
           then tc <> TC [TS False [encode csrrs 0x342 0 1, encode csrrs 0x343 0 1, encode csrrs 0xbc0 0 1]]
           else tc
  let saveOnFail tc tcTrans = do
        let insts = (map diiInstruction $ fromTestCase tc) ++ [diiEnd]
        m_traces <- doRVFIDII socA socB alive (timeoutDelay flags) False insts
        let (traceA, traceB) = fromMaybe (error "unexpected doRVFIDII failure")
                                         m_traces
        writeFile "last_failure.S" ("# last failing test case:\n" ++ show tc)
        when (optVerbosity flags > 0) $
          do putStrLn "Replaying shrunk failed test case:"
             let tcNew = tcTrans tc traceA traceB
             checkSingle tcNew 2 False (testLen flags) (const $ return ())
             return ()
        when (optSave flags) $
          do case (saveDir flags) of
               Nothing -> do
                 putStrLn "Save this trace (give file name or leave empty to ignore)?"
                 fileName <- getLine
                 when (not $ null fileName) $ do
                   putStrLn "One-line description?"
                   comment <- getLine
                   writeFile (fileName ++ ".S")
                             ("# " ++ comment ++ "\n" ++ show tc)
               Just dir -> do
                 t <- getCurrentTime
                 let tstamp = [if x == ' ' then '_' else x | x <- (show t)]
                 writeFile (dir ++ "/failure-" ++ tstamp ++ ".S")
                           ("# Automatically generated failing test case" ++ "\n" ++ show tc)
  let checkTrapAndSave tc = saveOnFail tc check_mcause_on_trap
  let checkResult = if (optVerbosity flags > 1)
                    then verboseCheckWithResult
                    else quickCheckWithResult
  let checkGen gen remainingTests = do
        res <- checkResult (Args Nothing remainingTests 1 (testLen flags) (optVerbosity flags > 0) (if optShrink flags then 1000 else 0))
                           (prop socA socB alive checkTrapAndSave archDesc (timeoutDelay flags) (optVerbosity flags > 1) gen)
        case res of Failure {} -> return 1
                    _          -> return 0
  let checkFile (memoryInitFile :: Maybe FilePath) (skipped :: Int) (fileName :: FilePath)
            | skipped == 0 = do putStrLn $ "Reading trace from " ++ fileName
                                trace <- read <$> readFile fileName
                                initTrace <- case (memoryInitFile) of
                                    Just memInit -> do putStrLn $ "Reading memory initialisation from file " ++ memInit
                                                       readDataFile memInit
                                    Nothing -> return mempty
                                res <- checkSingle (initTrace <> trace) (optVerbosity flags) (optShrink flags) (testLen flags) checkTrapAndSave
                                case res of Failure {} -> putStrLn "Failure."
                                            _          -> putStrLn "No Failure."
                                isAlive <- readIORef alive
                                return $ if isAlive then 0 else 1
            | otherwise = return (skipped + 1)
  --
  failuresRef <- newIORef 0
  let doCheck a b = do res <- checkGen a b
                       modifyIORef failuresRef ((+) res)
  case (instTraceFile flags) of
    Just fileName -> do
      void $ checkFile (memoryInitFile flags) 0 fileName
    Nothing -> do
      case (instDirectory flags) of
        Just directory -> do
          fileNames <- System.FilePath.Find.find always (extension ==? ".S") directory
          skipped <- foldM (checkFile Nothing) 0 fileNames
          when (skipped > 1) $ putStrLn $ "Warning: skipped " ++ show (skipped - 1) ++ " tests due to dead implementations"
        Nothing -> do
          case instrSoc of
            Nothing -> do
              when (has_i archDesc) $
                do putStrLn "rv32 I Arithmetic Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_arithmetic) (nTests flags)
                   putStrLn "rv32 I Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_memory) (nTests flags)
                   putStrLn "rv32 I Control Flow Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_controlflow) (nTests flags)
                   putStrLn "rv32 I Cache Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_cache) (nTests flags)
              when (has_i archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 I Arithmetic Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_arithmetic) (nTests flags)
                   putStrLn "rv64 I Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_memory) (nTests flags)
                   putStrLn "rv64 I Cache Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_cache) (nTests flags)
                   -- Note: no rv64 specific control flow instructions
              when (has_m archDesc) $
                do putStrLn "rv32 M extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_m) (nTests flags)
              when (has_m archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 M extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_m) (nTests flags)
              when (has_a archDesc) $
                do putStrLn "rv32 A extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_a) (nTests flags)
                   putStrLn "rv32 A Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_a_memory) (nTests flags)
              when (has_a archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 A extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_a) (nTests flags)
                   putStrLn "rv64 A Memory Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_a_memory) (nTests flags)
              when (has_c archDesc) $
                do putStrLn "rv C extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv_c) (nTests flags)
              when (has_f archDesc) $
                do putStrLn "rv32 F extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_f) (nTests flags)
              when (has_f archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 F extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_f) (nTests flags)
              when (has_d archDesc) $
                do putStrLn "rv32 D extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_d) (nTests flags)
              when (has_d archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 D extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_d) (nTests flags)
              when (has_icsr archDesc) $
                do putStrLn "rv32 Zicsr extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_zicsr) (nTests flags)
              when (has_ifencei archDesc) $
                do putStrLn "rv32 Zifencei extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv32_i_zifencei_memory) (nTests flags)
              when (has_ifencei archDesc && has_xlen_64 archDesc) $
                do putStrLn "rv64 Zifencei extension Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_i_zifencei_memory) (nTests flags)
              when (has_s archDesc) $
                do putStrLn "PTE template:"
                   doCheck (genTemplate $ gen_pte) (nTests flags)
              when (has_cheri archDesc) $
                do putStrLn "Xcheri extension Capability Inspection Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERIinspection) (nTests flags)
                   putStrLn "Xcheri extension Capability Arithmetic Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERIarithmetic) (nTests flags)
                   putStrLn "Xcheri extension Capability Miscellaneous Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERImisc) (nTests flags)
                   putStrLn "Xcheri extension Capability Control Flow Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd genCHERIcontrol) (nTests flags)
                   putStrLn "Xcheri extensions Cache Verification:"
                   doCheck (genTemplate $ repeatTemplateTillEnd gen_rv64_Xcheri_cache) (nTests flags)
                   putStrLn "Xcheri extension buildCap Template:"
                   doCheck (genTemplate $ repeatTemplateTillEnd $ buildCapTest archDesc) (nTests flags)
                   putStrLn "Xcheri extension Random Template:"
                   doCheck (genTemplate $ randomCHERITest archDesc) (nTests flags)

              putStrLn "All Verification:"
              doCheck (genTemplate $ genAll archDesc) (nTests flags)
              putStrLn "Random Template:"
              doCheck (genTemplate $ randomTest archDesc) (nTests flags)
            Just sock -> do
              doCheck (liftM toTestCase $ listOf $ liftM (\x -> TS False x) $ listOf $ genInstrServer sock) (nTests flags)
  --
  close socA
  close socB
  --
  failures <- readIORef failuresRef
  if failures == 0 then exitSuccess else exitWith $ ExitFailure failures
  --
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open dest addr = do
        putStrLn ("connecting to " ++ dest ++ " ...")
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
