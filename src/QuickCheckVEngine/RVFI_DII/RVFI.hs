--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018-2020 Alexandre Joannou
-- Copyright (c) 2020 Alex Richardson
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

-- |
--    Module      : QuickCheckVEngine.RVFI_DII.RVFI
--    Description : RISC-V Formal Interface
--
--    The 'QuickCheckVEngine.RVFI_DII.RVFI' module defines the RISC-V RVFI
--    interface as introduced
--    <https://github.com/SymbioticEDA/riscv-formal/blob/master/docs/rvfi.md here>
module QuickCheckVEngine.RVFI_DII.RVFI (
  RVFI_Packet (..)
, rvfiEmptyHaltPacket
, rvfiGetFromString
, rvfiIsHalt
, rvfiHaltVersion
, rvfiIsTrap
, rvfiCheck
, rvfiCheckAndShow
, rvfiCheckMagicBytes
, rvfiReadDataPacketWithMagic
, rvfiReadV1Response
, rvfiReadV2Response
, rvfi_rd_wdata_or_zero
) where

import Basement.Numerical.Number (toNatural)
import qualified Basement.Types.Word256
import Control.Monad
import Control.Monad.Zip (mzipWith)
import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Bits
import qualified Data.Bits.Bitwise as BW
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Builder (lazyByteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Int
import Data.Maybe
import RISCV hiding (and)
import RISCV.Helpers (privString, xlenString)
import Text.Printf

-- | Type synonym for a RISCV register index
type RV_RegIdx = Word8

-- | Type synonym for a RISCV XLEN-bit word
type RV_WordXLEN = Word64

-- * Definition of the RISC-V Formal Interface

-- | The 'RVFI_Packet' type captures (a subset of) the RISC-V Formal Interface
data RVFI_Packet = RVFI_Packet {
-- The trace version used to decode this packet. This is used to ignore parts
-- of fields that are only present in version 2 (or newer).
  rvfi_trace_version :: Int
-- Metadata
, rvfi_valid :: Word8
, rvfi_order :: Word64
, rvfi_insn  :: Word64
, rvfi_trap  :: Word8
, rvfi_halt  :: Word8
, rvfi_intr  :: Word8
, rvfi_mode  :: Maybe RISCV.PrivMode
, rvfi_ixl   :: Maybe RISCV.XLen
-- Program Counter
, rvfi_pc_rdata :: RV_WordXLEN
, rvfi_pc_wdata :: RV_WordXLEN
-- Integer Register Read/Write
, rvfi_int_data :: Maybe RVFI_IntData
-- Memory Access
, rvfi_mem_data :: Maybe RVFI_MemAccessData
-- Further TODOs in the RVFI specification:
-- Control and Status Registers (CSRs)
-- Modelling of Floating-Point State
-- Handling of Speculative Execution
-- Modelling of Virtual Memory
-- Modelling of Atomic Memory Operations
-- Skipping instructions
}

rvfiGetFromString :: [Char] -> Maybe (RVFI_Packet -> Integer)
rvfiGetFromString "valid"     = Just $ toInteger . rvfi_valid
rvfiGetFromString "order"     = Just $ toInteger . rvfi_order
rvfiGetFromString "insn"      = Just $ toInteger . rvfi_insn
rvfiGetFromString "trap"      = Just $ toInteger . rvfi_trap
rvfiGetFromString "halt"      = Just $ toInteger . rvfi_halt
rvfiGetFromString "intr"      = Just $ toInteger . rvfi_intr
rvfiGetFromString "rs1_addr"  = Just $ (maybe 0 $ toInteger . rvfi_rs1_addr) . rvfi_int_data
rvfiGetFromString "rs2_addr"  = Just $ (maybe 0 $ toInteger . rvfi_rs2_addr) . rvfi_int_data
rvfiGetFromString "rs1_rdata" = Just $ (maybe 0 $ toInteger . rvfi_rs1_rdata) . rvfi_int_data
rvfiGetFromString "rs2_rdata" = Just $ (maybe 0 $ toInteger . rvfi_rs2_rdata) . rvfi_int_data
rvfiGetFromString "rd_addr"   = Just $ (maybe 0 $ toInteger . rvfi_rd_addr) . rvfi_int_data
rvfiGetFromString "rd_wdata"  = Just $ (maybe 0 $ toInteger . rvfi_rd_wdata) . rvfi_int_data
rvfiGetFromString "pc_rdata"  = Just $ toInteger . rvfi_pc_rdata
rvfiGetFromString "pc_wdata"  = Just $ toInteger . rvfi_pc_wdata
rvfiGetFromString "mem_addr"  = Just $ (maybe 0 $ toInteger . rvfi_mem_addr) . rvfi_mem_data
rvfiGetFromString "mem_rmask" = Just $ (maybe 0 $ toInteger . rvfi_mem_rmask) . rvfi_mem_data
rvfiGetFromString "mem_wmask" = Just $ (maybe 0 $ toInteger . rvfi_mem_wmask) . rvfi_mem_data
rvfiGetFromString "mem_rdata" = Just $ (maybe 0 $ toInteger . toNatural . rvfi_mem_rdata) . rvfi_mem_data
rvfiGetFromString "mem_wdata" = Just $ (maybe 0 $ toInteger . toNatural . rvfi_mem_wdata) . rvfi_mem_data
rvfiGetFromString _           = Nothing

rvfi_rd_wdata_or_zero :: RVFI_Packet -> Word64
rvfi_rd_wdata_or_zero x = maybe 0 rvfi_rd_wdata $ rvfi_int_data x

data RVFI_IntData = RVFI_IntData {
  rvfi_rs1_addr  :: {-# UNPACK #-} !RV_RegIdx
, rvfi_rs2_addr  :: {-# UNPACK #-} !RV_RegIdx
, rvfi_rs1_rdata :: {-# UNPACK #-} !RV_WordXLEN
, rvfi_rs2_rdata :: {-# UNPACK #-} !RV_WordXLEN
, rvfi_rd_addr   :: {-# UNPACK #-} !RV_RegIdx
, rvfi_rd_wdata  :: {-# UNPACK #-} !RV_WordXLEN
  }
  deriving (Show)

data RVFI_MemAccessData = RVFI_MemAccessData {
 rvfi_mem_addr   :: {-# UNPACK #-} !RV_WordXLEN
, rvfi_mem_rmask :: {-# UNPACK #-} !Word32
, rvfi_mem_wmask :: {-# UNPACK #-} !Word32
, rvfi_mem_rdata :: {-# UNPACK #-} !Basement.Types.Word256.Word256
, rvfi_mem_wdata :: {-# UNPACK #-} !Basement.Types.Word256.Word256
}

hexStr :: BS.ByteString -> String
hexStr msg = show (toLazyByteString (lazyByteStringHex msg))

type ConnectionInfo = (String, Int)

connectionDebugMessage :: Int -> ConnectionInfo -> String -> IO ()
connectionDebugMessage minVerbosity (name, verbosity) msg = do
  when (verbosity >= minVerbosity) $
    putStrLn ("\t" ++ name ++ ": " ++ msg)

errorWithContext :: String -> String -> IO ()
errorWithContext name msg = do
  error (name ++ ": " ++ msg)

connectionError :: ConnectionInfo -> String -> IO ()
connectionError (name, _) msg = do
  errorWithContext name msg

rvfiCheckMagicBytes :: BS.ByteString -> String -> ConnectionInfo -> IO ()
rvfiCheckMagicBytes magicBytes expected conn = do
  let expBytes = C8.pack expected
  connectionDebugMessage 3 conn ("read header magic bytes: " ++ show magicBytes)
  when (magicBytes /= expBytes) $
    connectionError conn ("received invalid data packet: got magic=" ++ show magicBytes ++ " but expected " ++ show expBytes)
  return ()

rvfiReadDataPacketWithMagic :: (Int64 -> IO BS.ByteString, String, Int) -> Int64 -> String -> IO BS.ByteString
rvfiReadDataPacketWithMagic (reader, name, verbosity) size expectedMagic = do
  when (size < 8) $
    errorWithContext name ("Invalid packet size:" ++ show size)
  msg <- reader size
  connectionDebugMessage 3 (name, verbosity) ("read packet: " ++ hexStr msg)
  let (magic, bytes) = BS.splitAt 8 msg
  rvfiCheckMagicBytes magic expectedMagic (name, verbosity)
  return bytes

type RVFIFeatures = Word64

rvfiReadV2Response :: (Int64 -> IO BS.ByteString, String, Int) -> IO RVFI_Packet
rvfiReadV2Response (reader, name, verbosity) = do
  let connInfo = (name, verbosity)
  connectionDebugMessage 3 connInfo "reading V2 packet..."
  headerBytes <- rvfiReadDataPacketWithMagic (reader, name, verbosity) 64 "trace-v2"
  let (traceSizeBytes, payloadBytes) = BS.splitAt 8 headerBytes
  let traceSize = runGet getWord64le traceSizeBytes
  connectionDebugMessage 3 connInfo ("trace-v2 common payload bytes: " ++ hexStr payloadBytes)
  -- Ensure that we read all bytes in the packet
  let (basicData, availableFeatures) = runGet (isolate 48 rvfiDecodeV2Header) payloadBytes
  connectionDebugMessage 3 connInfo ("features: " ++ (printf "0x%016x" availableFeatures))
  (intData, rf1, numBytes1) <- rvfiMaybeReadIntData (reader, name, verbosity) availableFeatures
  (memData, rf2, numBytes2) <- rvfiMaybeReadMemData (reader, name, verbosity) rf1
  when (rf2 /= 0) $
    errorWithContext name ("Remaining unknown feature bits set: " ++ show rf2)
  let remainingBytes = (fromIntegral traceSize) - 64 - numBytes1 - numBytes2
  when (remainingBytes /= 0) $
    errorWithContext name ("Did not read all bytes of V2 trace packet: " ++ show remainingBytes ++ " remaining")
  return $ basicData {rvfi_int_data = intData, rvfi_mem_data = memData}

rvfiDecodeV2Header :: Get (RVFI_Packet, RVFIFeatures)
rvfiDecodeV2Header = do
  order <- getWord64le
  insn <- getWord64le
  trap <- getWord8
  halt <- getWord8
  intr <- getWord8
  mode <- getWord8
  ixl <- getWord8
  valid <- getWord8
  skip 2 -- 2 bytes of padding
  pc_rdata <- getWord64le
  pc_wdata <- getWord64le
  availableFeatures <- getWord64le
  return $ (RVFI_Packet {
      rvfi_trace_version = 2
    , rvfi_valid = valid
    , rvfi_order = order
    , rvfi_insn = insn
    , rvfi_trap = trap
    , rvfi_halt = halt
    , rvfi_intr = intr
    , rvfi_mode = Just (toEnum (fromIntegral mode))
    , rvfi_ixl = Just ixl
    , rvfi_pc_rdata = pc_rdata
    , rvfi_pc_wdata = pc_wdata
    , rvfi_int_data = Nothing
    , rvfi_mem_data = Nothing
    }, availableFeatures)

-- See sail-riscv/model/rvfi_dii.sail for the bitfield definitions
rvfiMaybeReadIntData :: (Int64 -> IO BS.ByteString, String, Int) -> RVFIFeatures -> IO (Maybe RVFI_IntData, RVFIFeatures, Int)
rvfiMaybeReadIntData connection availableFeatures = do
  let remainingFeatures = availableFeatures .&. (complement 0x1)
  if ((availableFeatures .&. 0x1) == 0)
    then do return (Nothing, remainingFeatures, 0)
    else do
      bytes <- rvfiReadDataPacketWithMagic connection 40 "int-data"
      return $ (Just (runGet (isolate 32 rvfiDecodeIntData) bytes), remainingFeatures, 40)

rvfiDecodeIntData :: Get RVFI_IntData
rvfiDecodeIntData = do
  rd_wdata <- getWord64le
  rs1_rdata <- getWord64le
  rs2_rdata <- getWord64le
  rd_addr <- getWord8
  rs1_addr <- getWord8
  rs2_addr <- getWord8
  skip 5 -- 5 bytes of padding
  return $! RVFI_IntData {
      rvfi_rd_wdata = rd_wdata
    , rvfi_rs1_rdata = rs1_rdata
    , rvfi_rs2_rdata = rs2_rdata
    , rvfi_rd_addr = rd_addr
    , rvfi_rs1_addr = rs1_addr
    , rvfi_rs2_addr = rs2_addr
    }

rvfiMaybeReadMemData :: (Int64 -> IO BS.ByteString, String, Int) -> RVFIFeatures -> IO (Maybe RVFI_MemAccessData, RVFIFeatures, Int)
rvfiMaybeReadMemData connection availableFeatures = do
  let remainingFeatures = availableFeatures .&. (complement 0x2)
  if ((availableFeatures .&. 0x2) == 0)
    then do return (Nothing, remainingFeatures, 0)
    else do
      bytes <- rvfiReadDataPacketWithMagic connection 88 "mem-data"
      return $ (Just (runGet (isolate 80 rvfiDecodeMemData) bytes), remainingFeatures, 88)

rvfiDecodeMemData :: Get RVFI_MemAccessData
rvfiDecodeMemData = do
  rdata1 <- getWord64le
  rdata2 <- getWord64le
  rdata3 <- getWord64le
  rdata4 <- getWord64le
  wdata1 <- getWord64le
  wdata2 <- getWord64le
  wdata3 <- getWord64le
  wdata4 <- getWord64le
  mem_rmask <- getWord32le
  mem_wmask <- getWord32le
  mem_addr <- getWord64le
  return $! RVFI_MemAccessData {
      rvfi_mem_addr = mem_addr
    , rvfi_mem_rmask = mem_rmask
    , rvfi_mem_wmask = mem_wmask
    , rvfi_mem_rdata = Basement.Types.Word256.Word256 rdata4 rdata3 rdata2 rdata1
    , rvfi_mem_wdata = Basement.Types.Word256.Word256 wdata4 wdata3 wdata2 wdata1
    }

rvfiReadV1Response :: (Int64 -> IO BS.ByteString, String, Int) -> IO RVFI_Packet
rvfiReadV1Response (reader, name, verbosity) = do
  msg <- reader 88
  connectionDebugMessage 3 (name, verbosity) ("read packet: " ++ hexStr msg)
  -- Note: BS.reverse since the decode was written in BE order
  return $ runGet (isolate 88 rvfiDecodeV1Response) (BS.reverse msg)

rvfiDecodeV1Response :: Get RVFI_Packet
rvfiDecodeV1Response = do
  intr <- getWord8
  halt <- getWord8
  trap <- getWord8
  rd_addr <- getWord8
  rs2_addr <- getWord8
  rs1_addr <- getWord8
  mem_wmask <- getWord8
  mem_rmask <- getWord8
  mem_wdata <- getWord64be
  mem_rdata <- getWord64be
  mem_addr <- getWord64be
  rd_wdata <- getWord64be
  rs2_rdata <- getWord64be
  rs1_rdata <- getWord64be
  insn <- getWord64be
  pc_wdata <- getWord64be
  pc_rdata <- getWord64be
  order <- getWord64be
  return $ RVFI_Packet {
      rvfi_trace_version = 1
    , rvfi_valid = 1
    , rvfi_order = order
    , rvfi_insn = insn
    , rvfi_trap = trap
    , rvfi_halt = halt
    , rvfi_intr = intr
    , rvfi_mode = Nothing
    , rvfi_ixl = Nothing
    , rvfi_pc_rdata = pc_rdata
    , rvfi_pc_wdata = pc_wdata
    , rvfi_int_data = Just RVFI_IntData {
        rvfi_rs1_addr = rs1_addr
      , rvfi_rs2_addr = rs2_addr
      , rvfi_rs1_rdata = rs1_rdata
      , rvfi_rs2_rdata = rs2_rdata
      , rvfi_rd_addr = rd_addr
      , rvfi_rd_wdata = rd_wdata
      }
    , rvfi_mem_data = Just RVFI_MemAccessData {
         rvfi_mem_addr = mem_addr
        , rvfi_mem_rmask = fromIntegral mem_rmask -- FIXME: I think this zero-extends?
        , rvfi_mem_wmask = fromIntegral mem_wmask -- FIXME: I think this zero-extends?
        , rvfi_mem_rdata = Basement.Types.Word256.Word256 0 0 0 mem_rdata
        , rvfi_mem_wdata = Basement.Types.Word256.Word256 0 0 0 mem_wdata
        }
      }

-- | An otherwise empty halt token for padding
rvfiEmptyHaltPacket :: RVFI_Packet
rvfiEmptyHaltPacket = RVFI_Packet {
      rvfi_trace_version = 0
    , rvfi_halt = 1
    , rvfi_valid = 0
    , rvfi_order = 0
    , rvfi_insn = 0
    , rvfi_trap = 0
    , rvfi_intr = 0
    , rvfi_mode = Nothing
    , rvfi_ixl = Nothing
    , rvfi_pc_rdata = 0
    , rvfi_pc_wdata = 0
    , rvfi_int_data = Nothing
    , rvfi_mem_data = Nothing
    }

instance Show RVFI_MemAccessData where
  show tok =
    printf
      "MA: 0x%016x, MWD: %s, MWM: 0b%08b, MRD: %s, MRM: 0b%08b "
      (rvfi_mem_addr tok) -- MA
      (printMemData wmask wdata) -- MWD
      wmask -- MWM
      (printMemData rmask rdata) -- MRD
      rmask -- MRM
    where rmask = rvfi_mem_rmask tok
          rdata = toInteger . toNatural . rvfi_mem_rdata $ tok
          wmask = rvfi_mem_wmask tok
          wdata = toInteger . toNatural . rvfi_mem_wdata $ tok
          printMemData :: Word32 -> Integer -> String
          printMemData mask value
            | mask <= 255 = printf "0x%016x" value
            | mask <= 65535 = printf "0x%032x" value
            | otherwise = printf "0x%064x" value

instance Show RVFI_Packet where
  show tok
    | rvfiIsHalt tok = "halt token"
    | otherwise =
      printf
        "Trap: %5s, PCWD: 0x%016x, RD: %02d, RWD: 0x%016x, %sI: 0x%016x %s XL:%s (%s)"
        (show $ rvfi_trap tok /= 0) -- Trap
        (rvfi_pc_wdata tok) -- PCWD
        (maybe 0 rvfi_rd_addr $ rvfi_int_data tok) -- RD
        (maybe 0 rvfi_rd_wdata $ rvfi_int_data tok) -- RWD
        (maybe "" show $ rvfi_mem_data tok) -- mem data
        (rvfi_insn tok)
        (privString (rvfi_mode tok))
        (xlenString (rvfi_ixl tok))
        (rv_pretty (MkInstruction (toInteger (rvfi_insn tok)))) -- Inst

-- | Return 'True' for halt 'RVFI_Packet's
rvfiIsHalt :: RVFI_Packet -> Bool
rvfiIsHalt x = rvfi_halt x /= 0

-- | Return the trace version for a halt packet (using the previously-unused bits)
rvfiHaltVersion :: RVFI_Packet -> Word8
rvfiHaltVersion x = 1 + shiftR (rvfi_halt x) 1

-- | Return 'True' for trap 'RVFI_Packet's
rvfiIsTrap :: RVFI_Packet -> Bool
rvfiIsTrap x = rvfi_trap x /= 0

-- | Compare two Maybe fields, but assume success if one of them does not exist.
optionalFieldsSame :: Eq a => (Maybe a) -> (Maybe a) -> Bool
optionalFieldsSame x y = fromMaybe True (mzipWith (==) x y)

compareMemData :: Bool -> RVFI_Packet -> RVFI_Packet -> (RVFI_MemAccessData -> Word32)
               -> (RVFI_MemAccessData -> Basement.Types.Word256.Word256) -> Bool
compareMemData is64 x y getMask getData = do
  -- If one of the implementations is reporting V1 traces, we can only compare
  -- the low 64 bits of the memory packet rather than the full 256.
  let compareV1BitsOnly = rvfi_trace_version x < 2 || rvfi_trace_version y < 2
  let clampMask m = if compareV1BitsOnly then m .&. 0xff else m
  let xMask = clampMask (getOrZero getMask x)
  let yMask = clampMask (getOrZero getMask y)
  (xMask == yMask) && ((xMask == 0) || (getMemAddr x == getMemAddr y))
    && (maskWith (getOrZero getData x) xMask == maskWith (getOrZero getData y) yMask)
  where
    clampAddr a = if is64 then a else a .&. 0x00000000FFFFFFFF
    getOrZero getter pkt = (maybe 0 getter $ rvfi_mem_data pkt)
    getMemAddr pkt = clampAddr (getOrZero rvfi_mem_addr pkt)
    byteMask2bitMask mask = BW.fromListLE $ concatMap (replicate 8) (BW.toListLE mask)
    maskWith a b = a Data.Bits..&. byteMask2bitMask b

-- Internal assert + rvfi helper functions
maskUpper :: (Bits p, Num p) => Bool -> p -> p
maskUpper _is64 _x = if _is64 then _x else _x Data.Bits..&. 0x00000000FFFFFFFF
getRDAddr :: RVFI_Packet -> RV_RegIdx
getRDAddr pkt = maybe 0 rvfi_rd_addr $ rvfi_int_data pkt
getRDWData :: Bool -> RVFI_Packet -> RV_WordXLEN
getRDWData _is64 pkt =
  maskUpper _is64 (maybe 0 rvfi_rd_wdata $ rvfi_int_data pkt)
getRS1Addr :: RVFI_Packet -> RV_RegIdx
getRS1Addr pkt = maybe 0 rvfi_rs1_addr $ rvfi_int_data pkt
getRS1RData :: Bool -> RVFI_Packet -> RV_WordXLEN
getRS1RData _is64 pkt =
  maskUpper _is64 (maybe 0 rvfi_rs1_rdata $ rvfi_int_data pkt)
getRS2Addr :: RVFI_Packet -> RV_RegIdx
getRS2Addr pkt = maybe 0 rvfi_rs2_addr $ rvfi_int_data pkt
getRS2RData :: Bool -> RVFI_Packet -> RV_WordXLEN
getRS2RData _is64 pkt =
  maskUpper _is64 (maybe 0 rvfi_rs2_rdata $ rvfi_int_data pkt)
getMemAddr :: Bool -> RVFI_Packet -> RV_WordXLEN
getMemAddr _is64 pkt =
  maskUpper _is64 (maybe 0 rvfi_mem_addr $ rvfi_mem_data pkt)

_checkField :: String -> Bool -> String -> Maybe String
_checkField msg matches ctx =
  if matches then Nothing else Just ("mismatch in field " ++ msg ++ ": " ++ ctx)

checkField :: Show a => Eq a => String -> a -> a -> Maybe String
checkField msg a b = _checkField msg (a == b) (show a ++ " != " ++ show b)

checkOptionalField :: Show a => Eq a => String -> Maybe a -> Maybe a
                   -> Maybe String
checkOptionalField msg a b =
  _checkField msg (optionalFieldsSame a b) (show a ++ " != " ++ show b)

-- | Compare 'RVFI_Packet's
rvfiCheck :: Bool -> RVFI_Packet -> RVFI_Packet -> Maybe String
rvfiCheck is64 x y
  | rvfiIsHalt x = if rvfi_halt x == rvfi_halt y then Nothing else Just "expected halt package"
  | otherwise =
    msum
      [ checkField "insn" (maskUpper False (rvfi_insn x)) (maskUpper False (rvfi_insn y)),
        checkField "insn" (maskUpper False (rvfi_insn x)) (maskUpper False (rvfi_insn y)),
        checkField "trap" (rvfi_trap x) (rvfi_trap y),
        checkField "halt" (rvfi_halt x) (rvfi_halt y),
        checkOptionalField "mode" (rvfi_mode x) (rvfi_mode y),
        checkOptionalField "XLEN" (rvfi_ixl x) (rvfi_ixl y),
        checkField "rd_addr" (getRDAddr x) (getRDAddr y),
        checkField "rd_wdata" (getRDWData is64 x) (getRDWData is64 y),
        checkField "rs1_addr" (getRS1Addr x) (getRS1Addr y),
        checkField "rs1_rdata" (getRS1RData is64 x) (getRS1RData is64 y),
        checkField "rs2_addr" (getRS2Addr x) (getRS2Addr y),
        checkField "rs2_rdata" (getRS2RData is64 x) (getRS2RData is64 y),
        checkField "pc_wdata" (maskUpper is64 (rvfi_pc_wdata x)) (maskUpper is64 (rvfi_pc_wdata y)),
        checkField "mem_addr" (getMemAddr is64 x) (getMemAddr is64 y),
        _checkField "mem_wdata" (compareMemData is64 x y rvfi_mem_wmask rvfi_mem_wdata) "", -- TODO: context
        _checkField "mem_rdata" (compareMemData is64 x y rvfi_mem_rmask rvfi_mem_rdata) "" -- TODO: context
      ]

assertCheck :: Bool -> RVFI_Packet -> [(RVFI_Packet -> Bool, String, Integer, String)] -> [String]
assertCheck is64 x asserts
  | rvfiIsHalt x = (\(_,b,_,_) -> printf "Failed assert \"%s\" due to halt packet" b) <$> asserts
  | otherwise = catMaybes (evalAssert <$> asserts)
  where evalAssert (pred, field, val, desc) = if pred x then Nothing else Just (printf "Failed assert \"%s\" (%s == 0x%x)" desc field val)

-- | Compare 2 'RVFI_Packet's and produce a 'String' output displaying the
--   the content of the packet once only for equal inputs or the content of
--   each input 'RVFI_Packet' if inputs are not succeeding the 'rvfiCheck'
rvfiCheckAndShow :: Bool -> Bool -> Maybe RVFI_Packet -> Maybe RVFI_Packet -> [(RVFI_Packet -> Bool, String, Integer, String)] -> (Bool, String)
rvfiCheckAndShow singleImp is64 x y asserts
  | singleImp, Just x' <- x, assertFails <- assertCheck is64 x' asserts = (null assertFails,  "     " ++ show x' ++ (suffix assertFails))
  | Just x' <- x, Just y' <- y, isNothing (rvfiCheck is64 x' y'),  assertFails <- assertCheck is64 y' asserts = (null assertFails,  "     " ++ show x' ++ (suffix assertFails))
  | Just x' <- x, Just y' <- y, mismatch <- rvfiCheck is64 x' y' =
    (False, "     " ++ fromJust mismatch
         ++ "\n A < " ++ show x' ++ suffix (maybe [] (\x' -> assertCheck is64 x' asserts) x)
         ++ "\n B > " ++ show y' ++ suffix (maybe [] (\y' -> assertCheck is64 y' asserts) y))
  | otherwise = (False,      " A < " ++ maybe "No report received" show x ++ suffix (maybe [] (\x' -> assertCheck is64 x' asserts) x)
                        ++ "\n B > " ++ maybe "No report received" show y ++ suffix (maybe [] (\y' -> assertCheck is64 y' asserts) y))
    where suffix assertFails = foldr (\(_,f,v,_) acc -> printf "%s (assert %s == 0x%x)" acc f v) "" asserts
                               ++ foldl (\x y -> x ++ "\n       " ++ y) "" assertFails
