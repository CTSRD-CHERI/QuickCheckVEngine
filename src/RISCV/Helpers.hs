--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019-2020 Alexandre Joannou
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

{-# LANGUAGE BinaryLiterals #-}

{-|
    Module      : RISCV.Helpers
    Description : Miscellaneous helpers internal to the 'RISCV' module

    The 'RISCV.Helpers' module provides miscellaneous helpers internal to the
    'RISCV' module, predominantly for RISC-V instruction pretty-printing
-}

module RISCV.Helpers (
-- * Instruction pretty-printers
  prettyR
, prettyI
, prettyI_sig
, prettyL
, prettyS
, prettyU
, prettyU_jal
, prettyB
, prettyF
, prettySfence
, prettyR_2op
, prettyCSR
, prettyCSR_imm
, prettyR_A
, prettyR_A_1op
, prettyR_FF_1op
, prettyR_IF_1op
, prettyR_FI_1op
, prettyR_FF_1op_rm
, prettyR_IF_1op_rm
, prettyR_FI_1op_rm
, prettyR_rm
, prettyR4_rm
, prettyS_F
-- * Others
, reg
, int
, fpRoundingMode
, intRegABI
, fpRegABI
, PrivMode
, privString
, XLen
, xlenString
, ExtractedRegs
) where

import Numeric
import Data.Maybe
import Data.Word (Word8)
import RISCV.RV_CSRs
import Data.Bits
import InstrCodec (Instruction)

data PrivMode = PRV_U | PRV_S | PRV_Reserved | PRV_M deriving (Enum, Show, Eq)
privString :: Maybe PrivMode -> String
privString Nothing = "PRV_?"
privString (Just x) = show x

type XLen = Word8
xlenString :: Maybe XLen -> String
xlenString Nothing = "?"
xlenString (Just 1) = "32"
xlenString (Just 2) = "64"
xlenString (Just 3) = "128"
xlenString (Just _) = "Invalid"

-- | Integer register pretty printer
reg :: Integer -> String
reg = intReg

-- | Gives a RISCV register name 'String' when provided an 'Integer' index
intReg :: Integer -> String
intReg i
  | i >= 0 && i < 32 = "x" ++ show i
  | otherwise = "unknownRegIdx" ++ show i

-- | Gives a RISCV register ABI name 'String' when provided an 'Integer' index
intRegABI :: Integer -> String
intRegABI 0 = "zero"
intRegABI 1 = "ra"
intRegABI 2 = "sp"
intRegABI 3 = "gp"
intRegABI 4 = "tp"
intRegABI 5 = "t0"
intRegABI 6 = "t1"
intRegABI 7 = "t2"
intRegABI 8 = "s0"
intRegABI 9 = "s1"
intRegABI 10 = "a0"
intRegABI 11 = "a1"
intRegABI 12 = "a2"
intRegABI 13 = "a3"
intRegABI 14 = "a4"
intRegABI 15 = "a5"
intRegABI 16 = "a6"
intRegABI 17 = "a7"
intRegABI 18 = "s2"
intRegABI 19 = "s3"
intRegABI 20 = "s4"
intRegABI 21 = "s5"
intRegABI 22 = "s6"
intRegABI 23 = "s7"
intRegABI 24 = "s8"
intRegABI 25 = "s9"
intRegABI 26 = "s10"
intRegABI 27 = "s11"
intRegABI 28 = "t3"
intRegABI 29 = "t4"
intRegABI 30 = "t5"
intRegABI 31 = "t6"
intRegABI  i = "unknownRegIdx" ++ show i

-- | Gives a RISCV floating point register name 'String' when provided an
--   'Integer' index
fpReg :: Integer -> String
fpReg i
  | i >= 0 && i < 32 = "f" ++ show i
  | otherwise = "unknownFPRegIdx" ++ show i

-- | Gives a RISCV floating point register ABI name 'String' when provided an
--   'Integer' index
fpRegABI :: Integer -> String
fpRegABI 0 = "ft0"
fpRegABI 1 = "ft1"
fpRegABI 2 = "ft2"
fpRegABI 3 = "ft3"
fpRegABI 4 = "ft4"
fpRegABI 5 = "ft5"
fpRegABI 6 = "ft6"
fpRegABI 7 = "ft7"
fpRegABI 8 = "fs0"
fpRegABI 9 = "fs1"
fpRegABI 10 = "fa0"
fpRegABI 11 = "fa1"
fpRegABI 12 = "fa2"
fpRegABI 13 = "fa3"
fpRegABI 14 = "fa4"
fpRegABI 15 = "fa5"
fpRegABI 16 = "fa6"
fpRegABI 17 = "fa7"
fpRegABI 18 = "fs2"
fpRegABI 19 = "fs3"
fpRegABI 20 = "fs4"
fpRegABI 21 = "fs5"
fpRegABI 22 = "fs6"
fpRegABI 23 = "fs7"
fpRegABI 24 = "fs8"
fpRegABI 25 = "fs9"
fpRegABI 26 = "fs10"
fpRegABI 27 = "fs11"
fpRegABI 28 = "ft8"
fpRegABI 29 = "ft9"
fpRegABI 30 = "ft10"
fpRegABI 31 = "ft11"
fpRegABI  i = "unknownFPRegIdx" ++ show i

-- | Wrapper for 'Integer' pretty printer
int :: Integer -> String
int i = show i

-- | Helper for turning numbers into signed integer
toSigned :: (Ord a, Num a) => Int -> a -> a
toSigned w x | x >= 0 = if x >= 2^(w-1) then x - 2^w else x
             | otherwise = error $ "cannot toSigned on negative number"


-- ** Instructions

-- | R-type instruction pretty printer
prettyR :: String -> Integer -> Integer -> Integer -> String
prettyR instr rs2 rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2]

-- | I-type instruction pretty printer
prettyI :: String -> Integer -> Integer -> Integer -> String
prettyI instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", int imm]

-- | I-type instruction pretty printer signed immediates
prettyI_sig :: String -> Integer -> Integer -> Integer -> String
prettyI_sig instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, ", ", int $ toSigned 12 imm]


-- | Pretty printer for load instructions
prettyL :: String -> Integer -> Integer -> Integer -> String
prettyL instr imm rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, "[", int $ toSigned 12 imm, "]"]

-- | S-type instruction pretty printer
prettyS :: String -> Integer -> Integer -> Integer -> String
prettyS instr imm rs2 rs1 =
  concat [instr, " ", reg rs2, ", ", reg rs1, "[", int $ toSigned 12 imm, "]"]

-- | U-type instruction pretty printer
prettyU :: String -> Integer -> Integer -> String
prettyU instr imm rd =
  concat [instr, " ", reg rd, ", ", int imm]

-- | U-type instruction pretty printer for jal instructions
prettyU_jal :: String -> Integer -> Integer -> String
prettyU_jal instr imm rd =
  concat [instr, " ", reg rd, ", ", int $ toSigned 21 (imm `shiftL` 1)]

-- | B-type instruction pretty printer
prettyB :: String -> Integer -> Integer -> Integer -> String
prettyB instr imm rs2 rs1 =
  concat [instr, " ", reg rs1, ", ", reg rs2, ", ", int $ toSigned 13 (imm `shiftL` 1)]

-- | Pretty printer for fence instructions
prettyF :: Integer -> Integer -> String
prettyF pre suc =
  concat ["fence ", int pre, ", ", int suc]

-- | Pretty printer for sfence.vma instructions
prettySfence :: Integer -> Integer -> String
prettySfence rs1 rs2 =
  concat ["sfence.vma ", int rs1, ", ", int rs2]

-- | R-type, 2-operand instruction pretty printer
prettyR_2op :: String -> Integer -> Integer -> String
prettyR_2op instr cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1]

-- | Pretty printer for CSR instructions
prettyCSR :: String -> Integer -> Integer -> Integer -> String
prettyCSR instr csr rs1 rd =
  concat [instr, " ", reg rd, ", ", csr_nm, ", ", reg rs1]
  where csr_nm  = (fromMaybe "unknown" (csrs_nameFromIndex csr)) ++ idx_str
        idx_str = " (0x" ++ showHex csr "" ++ ")"

-- | Pretty printer for immediate CSR instructions
prettyCSR_imm :: String -> Integer -> Integer -> Integer -> String
prettyCSR_imm instr csr imm rd =
  concat [instr, " ", reg rd, ", ", csr_nm, ", ", int imm]
  where csr_nm  = (fromMaybe "unknown" (csrs_nameFromIndex csr)) ++ idx_str
        idx_str = " (0x" ++ showHex csr "" ++ ")"

-- | R-type Atomic instruction pretty printer
prettyR_A :: String -> Integer -> Integer -> Integer -> Integer -> Integer
          -> String
prettyR_A instr aq rl rs2 rs1 rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1, ", ", reg rs2]
         ++ [if aq == 1 then " (aq)" else ""]
         ++ [if rl == 1 then " (rl)" else ""]

-- | R-type, single operand Atomic instruction pretty printer
prettyR_A_1op :: String -> Integer -> Integer -> Integer -> Integer -> String
prettyR_A_1op instr aq rl rs1 rd =
  concat $  [instr, " ", reg rd, ", ", reg rs1]
         ++ [if aq == 1 then " (aq)" else ""]
         ++ [if rl == 1 then " (rl)" else ""]

-- ** Floating Point

-- | Floating Point rounding modes pretty printer
fpRoundingMode :: Integer -> String
fpRoundingMode 0b000 = "rne"
fpRoundingMode 0b001 = "rtz"
fpRoundingMode 0b010 = "rdn"
fpRoundingMode 0b011 = "rup"
fpRoundingMode 0b100 = "rmm"
fpRoundingMode 0b101 = "Reserved5"
fpRoundingMode 0b110 = "Reserved6"
fpRoundingMode 0b111 = "rdyn"
fpRoundingMode x =
  "unsupported floating point rounding mode 0x" ++ (showHex x "")

prettyR_FF_1op :: String -> Integer -> Integer -> String
prettyR_FF_1op instr rs1 rd =
  concat [instr, " ", fpReg rd, ", ", fpReg rs1]

prettyR_FI_1op :: String -> Integer -> Integer -> String
prettyR_FI_1op instr rs1 rd =
  concat [instr, " ", fpReg rd, ", ", reg rs1]

prettyR_IF_1op :: String -> Integer -> Integer -> String
prettyR_IF_1op instr rs1 rd =
  concat [instr, " ", reg rd, ", ", fpReg rs1]

prettyR_FF_1op_rm :: String -> Integer -> Integer -> Integer -> String
prettyR_FF_1op_rm instr rs1 rm rd =
  concat $  [instr, " ", fpReg rd, ", ", fpReg rs1]
         ++ [", " ++ fpRoundingMode rm ]

prettyR_IF_1op_rm :: String -> Integer -> Integer -> Integer -> String
prettyR_IF_1op_rm instr rs1 rm rd =
  concat $  [instr, " ", reg rd, ", ", fpReg rs1]
         ++ [", " ++ fpRoundingMode rm ]

prettyR_FI_1op_rm :: String -> Integer -> Integer -> Integer -> String
prettyR_FI_1op_rm instr rs1 rm rd =
  concat $  [instr, " ", fpReg rd, ", ", reg rs1]
         ++ [", " ++ fpRoundingMode rm ]

prettyR_rm :: String -> Integer -> Integer -> Integer -> Integer -> String
prettyR_rm instr rs2 rs1 rm rd =
  concat $  [instr, " ", fpReg rd, ", ", fpReg rs1, ", ", fpReg rs2]
         ++ [", " ++ fpRoundingMode rm ]

prettyR4_rm :: String -> Integer -> Integer -> Integer -> Integer -> Integer
            -> String
prettyR4_rm instr rs3 rs2 rs1 rm rd =
  concat $  [ instr, " ", fpReg rd, ", "
            , fpReg rs1, ", ", fpReg rs2, ", ", fpReg rs3 ]
         ++ [", " ++ fpRoundingMode rm ]

prettyS_F :: String -> Integer -> Integer -> Integer -> String
prettyS_F instr imm rs2 rs1 =
  concat [instr, " ", fpReg rs2, ", ", reg rs1, "(", int imm, ")"]

type ExtractedRegs = ( Bool -- ^ is_bypass
                     , Maybe Integer -- ^ rs2
                     , Maybe Integer -- ^ rs1
                     , Maybe Integer -- ^ rd
                     ,    Integer -- ^ rs2
                       -> Integer -- ^ rs1
                       -> Integer -- ^ rd
                       -> Instruction -- re-encode
                     )
