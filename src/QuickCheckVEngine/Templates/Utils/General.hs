--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.Utils.General (
  -- *RISCV pseudo-instructions
  li
, li32
, li64
, csrr
, csrw
, csrwi
, csrs
, csrc
, csrsi
, csrci
  -- * Arbitrary value generators
, src
, dest
, sbcRegs
, csr
, roundingMode
, bits
, exclude
, geomBits
  -- * Memory helpers
  -- ** Basic memory helpers
, memOffset
, loadOp
, storeOp
, writeData
  -- ** Advanced memory helpers
, legalLoad
, legalStore
, surroundWithMemAccess
  -- * Other helpers
, csrBitSetOrClear
, prepReg
, prepReg32
, prepReg64
) where

import qualified Data.Bits.Bitwise as BW
import Test.QuickCheck hiding ((.&.))
import QuickCheckVEngine.Template
import RISCV
import Data.Bits
import Data.Word
import Data.List.Split

-- * RISCV pseudo-instructions
--------------------------------------------------------------------------------

-- | 'li' returns a 'Template' that loads an immediate into a register
li :: ArchDesc -> Integer -> Integer -> Template
li arch reg imm =
  if has_xlen_64 arch
     then random $ oneof (map return [li32 reg imm, li64 reg imm])
     else li32 reg imm

-- | 'li32' returns a 'Template' that loads a 32-bit immediate into a register
li32 :: Integer -> Integer -> Template
li32 reg imm = instSeq $ doHi20 ++ doLo12
  where doHi20 = [ lui reg (toInteger hi20) | hi20 /= 0]
        doLo12 = if (lo12 /= 0 || hi20 == 0)
                    then [addi reg (if hi20 /= 0 then reg else 0) (toInteger lo12)] else []
        imm32 :: Word32 = fromInteger imm
        hi20  :: Word32 = ((imm32 + 0x800) `shiftR` 12) .&. 0xfffff
        lo12  :: Word32 = if testBit imm32 12 then imm32 .|. 0xfffff000
                                             else imm32 .&. 0x00000fff

-- | 'li64' returns a 'Template' that loads a 64-bit immediate into a register
li64 :: Integer -> Integer -> Template
li64 reg imm = instSeq [ addi reg   0 (shiftAndMask imm 52 0xfff)
                       , slli reg reg 11
                       , addi reg reg (shiftAndMask imm 41 0x7ff)
                       , slli reg reg 11
                       , addi reg reg (shiftAndMask imm 30 0x7ff)
                       , slli reg reg 11
                       , addi reg reg (shiftAndMask imm 19 0x7ff)
                       , slli reg reg 11
                       , addi reg reg (shiftAndMask imm  8 0x7ff)
                       , slli reg reg 8
                       , addi reg reg (shiftAndMask imm  0 0x0ff) ]
  where shiftAndMask i shamt msk = toInteger $ ((fromInteger i :: Word64) `shiftR` shamt) .&. msk

-- | 'csrr' pseudo-instruction to read a CSR
csrr :: Integer -> Integer -> Template
csrr rd csr_idx = inst $ csrrs rd csr_idx 0

-- | 'csrw' pseudo-instruction to write a general purpose register's value to a CSR
csrw :: Integer -> Integer -> Template
csrw csr_idx rs1 = inst $ csrrw 0 csr_idx rs1

-- | 'csrwi' pseudo-instruction to write an immediate value to a CSR
csrwi :: Integer -> Integer -> Template
csrwi csr_idx uimm = inst $ csrrwi csr_idx 0 uimm

-- | 'csrs' pseudo-instruction to set the bits in a CSR corresponding to the
--   set bits of a mask value in a general purpose register
csrs :: Integer -> Integer -> Template
csrs csr_idx rs1 = inst $ csrrs 0 csr_idx rs1

-- | 'csrc' pseudo-instruction to clear the bits in a CSR corresponding to the
--   set bits of a mask value in a general purpose register
csrc :: Integer -> Integer -> Template
csrc csr_idx rs1 = inst $ csrrc 0 csr_idx rs1

-- | 'csrsi' pseudo-instruction to set the bits in a CSR corresponding to the
--   set bits of a mask value obtained by zero extending the 5-bit uimm
csrsi :: Integer -> Integer -> Template
csrsi csr_idx uimm = inst $ csrrsi 0 csr_idx uimm

-- | 'csrci' pseudo-instruction to clear the bits in a CSR corresponding to the
--   set bits of a mask value obtained by zero extending the 5-bit uimm
csrci :: Integer -> Integer -> Template
csrci csr_idx uimm = inst $ csrrci 0 csr_idx uimm

-- * Arbitrary value generators
--------------------------------------------------------------------------------

-- | 'inUseReg' generates a register index from the "in use registers",
--   currently one of r0, r1, r2, r3, r4, r16, r17, r18, r19 and r20
inUseReg :: Gen Integer
inUseReg = oneof [choose (0, 4), choose (16, 20)]
-- Alternative implementation:
--   frequency [
--     (32, return 1)
--   , (16, return 2)
--   , (8,  return 3)
--   , (4,  return 4)
--   , (2,  return 5)
--   , (1,  return 0)
--   ]

-- | 'src' generates an arbitrary source register index
src :: Gen Integer
src = inUseReg

-- | 'dest' generates an arbitrary destination register index
dest :: Gen Integer
dest = inUseReg

-- | 'sbcRegs' generates an arbitrary register index for SBC use
sbcRegs :: Gen Integer
sbcRegs = choose(22, 29)

-- | 'csr' generates an arbitrary csr register index
csr :: Gen Integer
csr = elements $ map fst csrs_map

-- | 'roundingMode' generates a random floating point rounding mode
-- Modes 5 and 6 are reserved for future use in the RISV ISA.
roundingMode :: Gen Integer
roundingMode = oneof $ map return [0, 1, 2, 3, 4, 7]

-- | 'bits' generates an arbitrary integer value of the given bit width
bits :: Int -> Gen Integer
bits w = choose (0, 2^w - 1)

-- | 'exclude' generates an arbitrary value of type 'a' using the provided
--   'Gen a', excluding those present in the provided exclusion list
exclude :: Eq a => [a] -> Gen a -> Gen a
exclude excl orig = orig >>= \x -> if elem x excl then exclude excl orig
                                                  else return x

-- | 'geomBits' generates an arbitrary power of two value, clustered around 1
geomBits :: Int -> Int -> Gen Integer
geomBits hi lo = frequency [(2^(32-i), return (2^i)) | i <- [lo..(hi-1)]]

-- * Memory helpers
--------------------------------------------------------------------------------

-- ** Basic memory helpers

-- | 'memOffset' generates an arbitrary memory address offset
memOffset :: Gen Integer
memOffset = oneof $ map return [0, 1, 64, 65]

-- | 'loadOp' provides a 'Template' for a memory load operation
--   The 'ArchDesc' argument determines which load instructions can be selected
--   The 'RISV.ArchDesc' module provides a 'archDesc_null' value with all its
--   fields set to 'False' which can be used to easily select a subset ignoring
--   the current architecture description
--   For example, to select only 'flw' instruction
--
--   > loadOp archDesc_null{ has_f = True, has_xlen_32 = True }
loadOp :: ArchDesc -> Integer -> Integer -> Template
loadOp arch rs1 rd = random $ oneof $ map (return . instUniform) $
     [ rv32_i_load rs1 rd 0 | has_xlen_32 arch ]
  ++ [ rv64_i_load rs1 rd 0 | has_xlen_64 arch ]
  ++ [ rv32_f_load rs1 rd 0 | has_f arch && has_xlen_32 arch ]
  ++ [ rv32_d_load rs1 rd 0 | has_d arch && has_xlen_32 arch ]

-- | 'storeOp' provides a 'Template' for a memory store operation
--   The 'ArchDesc' argument determines which store instructions can be selected
--   The 'RISV.ArchDesc' module provides a 'archDesc_null' value with all its
--   fields set to 'False' which can be used to easily select a subset ignoring
--   the current architecture description
--   For example, to select only 'fsw' instruction
--
--   > loadOp archDesc_null{ has_f = True, has_xlen_32 = True }
storeOp :: ArchDesc -> Integer -> Integer -> Template
storeOp arch rs1 rs2 = random $ oneof $ map (return . instUniform) $
     [ rv32_i_store rs1 rs2 0 | has_xlen_32 arch ]
  ++ [ rv64_i_store rs1 rs2 0 | has_xlen_64 arch ]
  ++ [ rv32_f_store rs1 rs2 0 | has_f arch && has_xlen_32 arch ]
  ++ [ rv32_d_store rs1 rs2 0 | has_d arch && has_xlen_32 arch ]

-- | Write provided list of 32-bit 'Integer's in memory starting at the provided
--   address by deriving a sequence initializing register 1 with that address,
--   register 2 with a 32-bit immediate value of each word, storing the content
--   of register 2 at the address contained in register 1, and incrementing
--   that address by 4 each time
writeData :: Integer -> [Integer] -> Template
writeData addr ws = li64 1 addr <> mconcat (map writeWord ws)
  where writeWord w =  li32 2 (byteSwap w)
                    <> instSeq [ sw 1 2 0, addi 1 1 4 ]
        byteSwap w = swpHlp ((fromInteger w) :: Word32)
        swpHlp = BW.fromListLE . concat . reverse . chunksOf 8 . BW.toListLE

-- ** Advanced memory helpers

-- | 'legalLoad' provides a 'Template' for a load operation from an arbitrary
--   "RVFI-DII legal" address into an arbitrary register
legalLoad :: ArchDesc -> Template
legalLoad arch = random $ do
  tmpReg    <- src
  addrReg   <- src
  targetReg <- dest
  return $ instSeq [ andi addrReg addrReg 0xff
                   , lui tmpReg 0x40004
                   , slli tmpReg tmpReg 1
                   , add addrReg tmpReg addrReg ]
           <> loadOp arch addrReg targetReg

-- | 'legalStore' provides a 'Template' for a store operation from an arbitrary
--   register to an arbitrary "RVFI-DII legal" address
legalStore :: ArchDesc -> Template
legalStore arch = random $ do
  tmpReg  <- src
  addrReg <- src
  dataReg <- dest
  return $ instSeq [ andi addrReg addrReg 0xff
                   , lui tmpReg 0x40004
                   , slli tmpReg tmpReg 1
                   , add addrReg tmpReg addrReg ]
           <> storeOp arch dataReg addrReg

-- | 'surroundWithMemAccess' wraps a 'Template' by performing a store operation
--   before it, and following it by a load operation to the same
--   "RVFI-DII legal" address
surroundWithMemAccess :: ArchDesc -> Template -> Template
surroundWithMemAccess arch x = random $ do
  regAddr <- dest
  regData <- dest
  offset  <- bits 8
  value   <- bits 12
  shift   <- bits 6
  return $    storeToAddress regAddr regData offset value shift
           <> x
           <> loadFromAddress regAddr offset regData
  where loadFromAddress reg offset dest =
          instSeq [ lui reg 0x40004
                  , slli reg reg 1
                  , addi reg reg offset ]
          <> loadOp arch reg dest
        storeToAddress regAddr regData offset value shift =
          instSeq [ addi regData 0 value
                  , slli regData regData shift
                  , lui regAddr 0x40004
                  , slli regAddr regAddr 1
                  , addi regAddr regAddr offset ]
          <> storeOp arch regAddr regData

-- * Other helpers
--------------------------------------------------------------------------------

-- | Helper for single bit setting / clearing of a CSR
csrBitSetOrClear :: Bool -> CSRIdx -> Integer -> Integer -> Template
csrBitSetOrClear set csrIdx bitIdx tmpReg
  | bitIdx < 5 = insti csrIdx mask
  | otherwise = li32 tmpReg mask <> inst csrIdx tmpReg
  where mask  = (1 `shiftL` fromInteger bitIdx)
        inst  = if set then  csrs else csrc
        insti = if set then csrsi else csrci

{- No longer used, use 'li32' instead
-- | 'loadImm32' initializes the given register with the given value
loadImm32 dst imm =
  instSeq [ addi dst 0 ((shift imm (-21)) Data.Bits..&. 0x7FF)
          , slli dst dst 11
          , addi dst dst ((shift imm (-10)) Data.Bits..&. 0x7FF)
          , slli dst dst 10
          , addi dst dst (imm Data.Bits..&. 0x3FF)]
-}

-- | 'prepReg' provides a 'Template' to initialize a given register to an
--   arbitrary value
prepReg :: ArchDesc -> Integer -> Template
prepReg arch dst =
  if has_xlen_64 arch
     then random $ oneof (map return [prepReg32 dst, prepReg64 dst])
     else prepReg32 dst

-- | 'prepReg32' provides a 'Template' to initialize a given register to an
--   arbitrary 32-bit value
prepReg32 :: Integer -> Template
prepReg32 dst = random $ do imm <- bits 32
                            return (li32 dst imm)

-- | 'prepReg64' provides a 'Template' to initialize a given register to an
--   arbitrary 64-bit value
prepReg64 :: Integer -> Template
prepReg64 dst = repeatN 6 $ random $ do
  val <- bits 12
  return $ instSeq [ slli dst dst 12
                   , xori dst dst val ]
