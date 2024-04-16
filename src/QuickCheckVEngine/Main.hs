--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018-2021 Alexandre Joannou
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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Text.Regex.TDFA

import RISCV hiding (or)
import InstrCodec
import QuickCheckVEngine.MainHelpers
import QuickCheckVEngine.RVFI_DII
import qualified QuickCheckVEngine.Template as T
import QuickCheckVEngine.Test
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
import QuickCheckVEngine.Templates.GenHPM
import QuickCheckVEngine.Templates.GenTransExec

-- command line arguments
--------------------------------------------------------------------------------
data Options = Options
    { optVerbosity     :: Int
    , nTests           :: Int
    , impAPort         :: String
    , impAIP           :: String
    , impBPort         :: String
    , impBIP           :: String
    , instTraceFile    :: Maybe FilePath
    , instDirectory    :: Maybe FilePath
    , memoryInitFile   :: Maybe FilePath
    , arch             :: ArchDesc
    , testIncludeRegex :: Maybe String
    , testExcludeRegex :: Maybe String
    , instrPort        :: Maybe String
    , saveDir          :: Maybe FilePath
    , timeoutDelay     :: Int
    , testLen          :: Int
    , optSingleImp     :: Bool
    , optShrink        :: Bool
    , optPedantic      :: Bool
    , optSave          :: Bool
    , optContinueOnFail:: Bool
    , optIgnoreAsserts :: Bool
    , csrIncludeRegex  :: Maybe String
    , csrExcludeRegex  :: Maybe String
    } deriving Show

defaultOptions :: Options
defaultOptions = Options
    { optVerbosity     = 1
    , nTests           = 100
    , impAPort         = "5000"
    , impAIP           = "127.0.0.1"
    , impBPort         = "5001"
    , impBIP           = "127.0.0.1"
    , instTraceFile    = Nothing
    , instDirectory    = Nothing
    , memoryInitFile   = Nothing
    , arch             = archDesc_rv32i
    , testIncludeRegex = Nothing
    , testExcludeRegex = Nothing
    , instrPort        = Nothing
    , saveDir          = Nothing
    , timeoutDelay     = 6000000000 -- 60 seconds
    , testLen          = 2048
    , optShrink        = True
    , optPedantic      = True
    , optSave          = True
    , optContinueOnFail= False
    , optIgnoreAsserts = False
    , optSingleImp     = False
    , csrIncludeRegex = Nothing
    , csrExcludeRegex = Nothing
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
  , Option ['I']     ["test-include-regex"]
      (ReqArg (\ f opts -> opts { testIncludeRegex = Just f }) "REGEX")
        "Specify REGEX to include only a subset of tests"
  , Option ['x']     ["test-exclude-regex"]
      (ReqArg (\ f opts -> opts { testExcludeRegex = Just f }) "REGEX")
        "Specify REGEX to exclude a subset of tests"
  , Option ['i']     ["instruction-generator-port"]
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
  , Option ['R']     ["relaxed-comparison"]
      (NoArg (\ opts -> opts { optPedantic = False }))
        "Only compare key RVFI fields"
  , Option ['S']     ["disable-shrink"]
      (NoArg (\ opts -> opts { optShrink = False }))
        "Disable shrinking of failed tests"
  , Option []        ["no-save"]
      (NoArg (\ opts -> opts { optSave = False }))
        "Don't offer to save failed counter-examples"
  , Option []        ["continue-on-fail"]
      (NoArg (\ opts -> opts { optContinueOnFail = True }))
        "Keep running tests after failure to find multiple failures"
  , Option []        ["ignore-asserts"]
      (NoArg (\ opts -> opts { optIgnoreAsserts = True }))
        "Don't fail tests if assertions fail"
  , Option []        ["single-implementation"]
      (NoArg (\ opts -> opts { optSingleImp = True }))
        "Run with only implementation A, testing asserts only (if enabled)"
  , Option []     ["csr-include-regex"]
      (ReqArg (\ f opts -> opts { csrIncludeRegex = Just f }) "REGEX")
        "Specify REGEX to test only a subset of CSRs"
  , Option []     ["csr-exclude-regex"]
      (ReqArg (\ f opts -> opts { csrExcludeRegex = Just f }) "REGEX")
        "Specify REGEX to exclude a subset of CSRs from tests"
  ]

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: QCVEngine [OPTION...] files..."

allTests :: [(String, String, ArchDesc -> Bool, T.Template)]
allTests = [
             ("arith",      "Arithmetic Verification",                                const True,                               T.repeatTillEnd gen_rv32_i_arithmetic)
           -- CSC: Capability Speculation Constraint
           -- BSC: Branching Speculation Constraint
           -- TSC: Translation Speculation Constraint
           , ("csc_data",   "Data CSC Verification",                                  andPs [has_cheri, has_icsr, has_ihpm],              gen_csc_data_verify)
           , ("bsc_cond_1", "BSC Condition 1 Verification",                           andPs [has_cheri, has_icsr, has_ihpm, has_xlen_64], gen_bsc_cond_1_verify)
           , ("bsc_jumps",  "BSC Jumps Verification",                                 andPs [has_cheri, has_icsr, has_ihpm, has_xlen_64], gen_bsc_jumps_verify)
           , ("bsc_excps",  "BSC Exceptions Verification",                            andPs [has_cheri, has_icsr, has_ihpm, has_xlen_64], gen_bsc_exceptions_verify)
           , ("tsc",        "TSC Verification",                                       andPs [has_s, has_icsr, has_ihpm, has_xlen_64],     gen_tsc_verify)
           , ("csc_inst",   "Instruction CSC Verification",                           andPs [has_cheri, has_icsr, has_ihpm, has_xlen_64], gen_csc_inst_verify)
           , ("cclear",     "CClear Verification",                                    andPs [has_cheri, has_xlen_64],                     gen_simple_cclear)
           , ("fpclear",    "FPClear Verification",                                   andPs [has_cheri, has_xlen_64, has_d],              gen_simple_fpclear)
           , ("mem",        "Memory Verification",                                    const True,                               T.repeatTillEnd gen_rv32_i_memory)
           , ("control",    "Control Flow Verification",                              const True,                               T.repeatTillEnd gen_rv32_i_controlflow)
           , ("cache",      "Cache Verification",                                     const True,                               T.repeatTillEnd gen_rv32_i_cache)
           , ("arith64",    "RV64 Arithmetic Verification",                           has_xlen_64,                              T.repeatTillEnd gen_rv64_i_arithmetic)
           , ("mem64",      "RV64 Memory Verification",                               has_xlen_64,                              T.repeatTillEnd gen_rv64_i_memory)
           , ("cache64",    "RV64 Cache Verification",                                has_xlen_64,                              T.repeatTillEnd gen_rv64_i_cache)
           -- Note: no rv64 specific control flow instructions
           , ("muldiv",     "M Extension Verification",                               has_m,                                    T.repeatTillEnd gen_rv32_m)
           , ("muldiv64",   "RV64 M Extension Verification",                          andPs [has_m, has_xlen_64],               T.repeatTillEnd gen_rv64_m)
           , ("atomic",     "A Extension Verification",                               has_a,                                    T.repeatTillEnd gen_rv32_a)
           , ("memAmo",     "AMO Memory Verification",                                has_a,                                    T.repeatTillEnd gen_rv32_i_a_memory)
           , ("atomic64",   "RV64 A Extension Verification",                          andPs [has_a, has_xlen_64],               T.repeatTillEnd gen_rv64_a)
           , ("capatomic",  "Xcheri A Extension Verification",                        andPs [has_a, has_xlen_64],               T.repeatTillEnd gen_cheri_a)
           , ("memAmo64",   "RV64 AMO Memory Verification",                           andPs [has_a, has_xlen_64],               T.repeatTillEnd gen_rv64_i_a_memory)
           , ("compressed", "C Extension Verification",                               has_c,                                    T.repeatTillEnd gen_rv_c)
           , ("float",      "F Extension Verification",                               has_f,                                    T.repeatTillEnd gen_rv32_f)
           , ("float64",    "RV64 F Extension Verification",                          andPs [has_f, has_xlen_64],               T.repeatTillEnd gen_rv64_f)
           , ("double",     "D Extension Verification",                               has_d,                                    T.repeatTillEnd gen_rv32_d)
           , ("double64",   "RV64 D Extension Verification",                          andPs [has_d, has_xlen_64],               T.repeatTillEnd gen_rv64_d)
           , ("csr",        "Zicsr Extension Verification",                           has_icsr,                                 T.repeatTillEnd gen_rv32_i_zicsr)
           , ("fencei",     "Zifencei Extension Verification",                        has_ifencei,                              T.repeatTillEnd gen_rv32_i_zifencei_memory)
           , ("fencei64",   "RV64 Zifencei Extension Verification",                   andPs [has_ifencei, has_xlen_64],         T.repeatTillEnd gen_rv64_i_zifencei_memory)
           , ("pte",        "PTE Verification",                                       has_s,                                    T.repeatN 2 $ T.uniform [gen_pte_perms, gen_pte_trans])
           , ("hpm",        "HPM Verification",                                       andPs [has_icsr, has_ihpm],               T.repeatTillEnd genHPM)
           , ("capinspect", "Xcheri Extension Capability Inspection Verification",    has_cheri,                                T.repeatTillEnd genCHERIinspection)
           , ("caparith",   "Xcheri Extension Capability Arithmetic Verification",    has_cheri,                                T.repeatTillEnd genCHERIarithmetic)
           , ("capmisc",    "Xcheri Extension Capability Miscellaneous Verification", has_cheri,                                T.repeatTillEnd genCHERImisc)
           , ("capcontrol", "Xcheri Extension Capability Control Flow Verification",  has_cheri,                                T.repeatTillEnd genCHERIcontrol)
           , ("capcache",   "Xcheri Extension Cache Verification",                    has_cheri,                                T.repeatTillEnd gen_rv64_Xcheri_cache)
           , ("capdecode",  "Xcheri Extension Capability Decode Template",            has_cheri,                                T.repeatTillEnd capDecodeTest)
           , ("cloadtags",  "Xcheri Extension CLoadTags Template",                    andPs [has_cheri, not . has_nocloadtags], T.repeatTillEnd cLoadTagsTest)
           , ("caprandom",  "Xcheri Extension Random Template",                       has_cheri,                                randomCHERITest)
           , ("caprvcrandom", "Xcheri RVC Extension Random Template",                 andPs [has_cheri, has_c],                 randomCHERIRVCTest)
           , ("all",        "All Verification",                                       const True,                               genAll)
           , ("random",     "Random Template",                                        const True,                               randomTest)
           ]
  where andPs = foldl (\k p -> (\x -> p x && k x)) (const True)

--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ do
  -- parse command line arguments
  rawArgs <- getArgs
  (flags, _) <- commandOpts rawArgs
  when (optVerbosity flags > 1) $ print flags
  let checkRegex incReg excReg str = (str =~ (fromMaybe ".*" incReg)) && (not $ str =~ (fromMaybe "a^" excReg))
  let archDesc = arch flags
  let csrFilter idx = checkRegex (csrIncludeRegex flags) (csrExcludeRegex flags) (fromMaybe "reserved" $ csrs_nameFromIndex idx)
  let testParams = T.TestParams { T.archDesc  = archDesc
                                , T.csrFilter = csrFilter }
  -- initialize model and implementation sockets
  implA <- rvfiDiiOpen (impAIP flags) (impAPort flags) (optVerbosity flags) "implementation-A"
  m_implB <- if optSingleImp flags then return Nothing else Just <$> rvfiDiiOpen (impBIP flags) (impBPort flags) (optVerbosity flags) "implementation-B"

  addrInstr <- mapM (resolve "127.0.0.1") (instrPort flags)
  instrSoc <- mapM (open "instruction-generator-port") addrInstr
  --
  alive <- newIORef True -- Cleared when either implementation times out, since they will may not be able to respond to future queries
  let checkSingle :: Test TestResult -> Int -> Bool -> Int -> (Test TestResult -> IO ()) -> IO Result
      checkSingle test verbosity doShrink len onFail = do
        quickCheckWithResult (Args Nothing 1 1 len (verbosity > 0) (if doShrink then 1000 else 0))
                             (prop implA m_implB alive onFail archDesc (timeoutDelay flags) verbosity (optIgnoreAsserts flags) (optPedantic flags) (return test))
  let check_mcause_on_trap :: Test TestResult -> Test TestResult
      check_mcause_on_trap (trace :: Test TestResult) = if or (hasTrap <$> trace) then (filterTest p trace) <> wrapTest testSuffix else trace
        where hasTrap (_, a, b) = maybe False rvfiIsTrap a || maybe False rvfiIsTrap b
              testSuffix = noShrink $ singleSeq [ csrrs 1 (unsafe_csrs_indexFromName "mcause") 0
                                                , csrrs 1 (unsafe_csrs_indexFromName "mtval" ) 0
                                                , csrrs 1 (unsafe_csrs_indexFromName "mccsr" ) 0 ]
              p (DII_End _, _, _) = False
              p _ = True
  let askAndSave sourceFile contents m_trace testTrans = do
        writeFile "last_failure.S" ("# last failing test case:\n" ++ contents)
        case m_trace of Just trace | optVerbosity flags > 0 -> do
                          putStrLn "Replaying shrunk failed test case:"
                          checkSingle (testTrans trace) 2 False (testLen flags) (const $ return ())
                          return ()
                        _ -> return ()
        when (optSave flags) $ do
          case saveDir flags of
            Nothing -> do
              putStrLn "Save this trace (give file name or leave empty to ignore)?"
              fileName <- getLine
              when (not $ null fileName) $ do
                putStrLn "One-line description?"
                comment <- getLine
                writeFile (fileName ++ ".S")
                          ("# " ++ comment ++ "\n" ++ contents)
            Just dir -> do
              t <- getCurrentTime
              let tstamp = [if x == ' ' then '_' else if x == ':' then '-' else x | x <- show t]
              let prelude = case sourceFile of
                              Just name -> "# Generated from input file: " ++ show name ++ "\n"
                              Nothing   -> "# Automatically generated failing test case\n"
              writeFile (dir ++ "/failure-" ++ tstamp ++ ".S") (prelude ++ contents)
  let saveOnFail :: Maybe FilePath -> Test TestResult -> (Test TestResult -> Test TestResult) -> IO ()
      saveOnFail sourceFile test testTrans = runImpls implA m_implB alive (timeoutDelay flags) 0 test onTrace onDeath onDeath
        where onDeath test = do putStrLn "Failure rerunning test"
                                askAndSave sourceFile (show test) Nothing testTrans
              onTrace trace = askAndSave sourceFile (showAnnotatedTrace (isNothing m_implB) archDesc trace) (Just trace) testTrans
  let checkTrapAndSave sourceFile test = saveOnFail sourceFile test (check_mcause_on_trap :: Test TestResult -> Test TestResult)
  let checkResult = if optVerbosity flags > 1 then verboseCheckWithResult else quickCheckWithResult
  let checkGen gen remainingTests =
        checkResult (Args Nothing remainingTests 1 (testLen flags) (optVerbosity flags > 0) (if optShrink flags then 1000 else 0))
                    (prop implA m_implB alive (checkTrapAndSave Nothing) archDesc (timeoutDelay flags) (optVerbosity flags) (optIgnoreAsserts flags) (optPedantic flags) gen)
  failuresRef <- newIORef 0
  let checkFile (memoryInitFile :: Maybe FilePath) (skipped :: Int) (fileName :: FilePath)
        | skipped == 0 = do putStrLn $ "Reading trace from " ++ fileName
                            trace <- read <$> readFile fileName
                            initTrace <- case memoryInitFile of
                              Just memInit -> do putStrLn $ "Reading memory initialisation from file " ++ memInit
                                                 readDataFile testParams memInit
                              Nothing -> return mempty
                            res <- checkSingle (wrapTest $ initTrace <> trace) (optVerbosity flags) (optShrink flags) (testLen flags) (checkTrapAndSave (Just fileName))
                            case res of Failure {} -> do putStrLn "Failure."
                                                         modifyIORef failuresRef ((+) 1)
                                                         when (not (optContinueOnFail flags)) $ writeIORef alive False
                                        _          -> putStrLn "No Failure."
                            isAlive <- readIORef alive
                            return $ if isAlive then 0 else 1
        | otherwise = return $ skipped + 1
  --
  let doCheck a b = do res <- checkGen a b
                       case res of Failure {} -> modifyIORef failuresRef ((+) 1)
                                   _ -> return ()
                       return res
  case instTraceFile flags of
    Just fileName -> do
      void $ checkFile (memoryInitFile flags) 0 fileName
    Nothing -> do
      case instDirectory flags of
        Just directory -> do
          fileNames <- System.FilePath.Find.find always (extension ==? ".S") directory
          skipped <- foldM (checkFile Nothing) 0 fileNames
          when (skipped > 1) $ putStrLn $ "Warning: skipped " ++ show (skipped - 1) ++ " tests"
        Nothing -> do
          case instrSoc of
            Nothing -> do let tests = [ template | template@(label,_,_,_) <- allTests
                                      , checkRegex (testIncludeRegex flags) (testExcludeRegex flags) label ]
                          when (null tests) $ putStrLn "Warning: no tests selected"
                          mapM_ attemptTest tests
              where attemptTest (label, description, archReqs, template) =
                      if archReqs archDesc then do
                        putStrLn $ label ++ " -- " ++ description ++ ":"
                        (if optContinueOnFail flags then repeatTillTarget else (\f t -> f t >> return ())) ((numTests <$>) . (doCheck (wrapTest <$> (T.genTest testParams template)))) (nTests flags)
                      else
                        putStrLn $ "Warning: skipping " ++ label ++ " since architecture requirements not met"
                    repeatTillTarget f t = if t <= 0 then return () else f t >>= (\x -> repeatTillTarget f (t - x))
            Just sock -> do
              doCheck (liftM (wrapTest . singleSeq . (MkInstruction <$>)) $ listOf (genInstrServer sock)) (nTests flags)
              return ()
  --
  rvfiDiiClose implA
  maybe (pure ()) rvfiDiiClose m_implB
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
      sock <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      putStrLn ("connected to " ++ dest ++ " ...")
      return sock
    rvfiDiiOpen ip port verb name = do
      addr <- resolve ip port
      soc <- open name addr
      traceVer <- rvfiNegotiateVersion soc name verb
      return $ RvfiDiiConnection soc traceVer name
    rvfiDiiClose (RvfiDiiConnection sock _ _) = close sock
