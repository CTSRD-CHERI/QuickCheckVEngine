--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
-- Copyright (c) 2021 Jonathan Woodruff
-- Copyright (c) 2021-2022 Franz Fuchs
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
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

module QuickCheckVEngine.Templates.GenTransExec (
  gen_csc_data_verify
, gen_bsc_cond_1_verify
, gen_csc_inst_verify
, gen_bsc_jumps_verify
, gen_bsc_exceptions_verify
, gen_tsc_verify
) where

import InstrCodec
import Test.QuickCheck
import RISCV.RV32_I
import RISCV.RV64_I
import RISCV.RV32_Zcheri
import RISCV.RV32_F
import RISCV.RV32_D
import RISCV.RV_CSRs
import RISCV.ArchDesc
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.GenMemory
import QuickCheckVEngine.Templates.Utils.CHERI
import QuickCheckVEngine.Templates.Utils.HPM
import QuickCheckVEngine.Templates.Utils.General
import QuickCheckVEngine.RVFI_DII.RVFI
import QuickCheckVEngine.Templates.GenMemory
import Data.Bits

rv32_xcheri_misc_alt :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_misc_alt src1 src2 imm dest =
  [ acperm    dest src1 src2
  , cbld      dest src1 src2
  , scmode    dest src1 src2
  , sentry    dest src1]

genCSCDataTorture :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
genCSCDataTorture capReg tmpReg bitsReg sldReg nopermReg authReg = random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  csrAddr  <- frequency [ (1, return (unsafe_csrs_indexFromName "mccsr"))
                        , (1, return (unsafe_csrs_indexFromName "mcause"))
                        , (1, bits 12) ]
  src1     <- frequency [ (1, return capReg), (1, return tmpReg), (1, return bitsReg), (1, return sldReg) ]
  src2     <- frequency [ (1, return capReg), (1, return tmpReg), (1, return bitsReg), (1, return sldReg) ]
--  let rv32_xcheri_misc_alt = filter (/= (cspecialrw tmpReg csrAddr src1)) (rv32_xcheri_misc src1 src2 csrAddr imm tmpReg)
  return $  (uniform [ instUniform $ rv32_xcheri_arithmetic src1 src2 imm tmpReg
                     , instUniform $ rv32_xcheri_misc_alt src1 src2 imm dest
                     , instUniform $ rv32_xcheri_inspection src1 dest
                     , inst $ cinvoke src2 src1
                     , inst $ cload tmpReg tmpReg 0x08
                     ])


genBSC_Cond_1_Torture :: Template
genBSC_Cond_1_Torture = random $ do
  imm_rand      <- bits 12
  longImm_rand  <- bits 20
  fenceOp1      <- bits 3
  fenceOp2      <- bits 3
  srcData       <- src
  let zeroReg = 0
  let addrReg = 5
  let tmpReg = 10
  let src1 = 12
  let src2 = 13
  let captmpReg = 14
  let capsrc1 = 15
  let capsrc2 = 16
  let dest = 17
  -- immediate has to be divisible by 8
  let imm = (imm_rand `shiftR` 0x3) `shiftL` 0x3
  -- long immediate has to be divisible by 4
  let longImm = (longImm_rand `shiftR` 0x2) `shiftL` 0x2
  return $ uniform [ instUniform $ rv64_i_arith src1 src2 imm tmpReg
                   , instUniform $ rv32_i_arith src1 src2 imm longImm tmpReg
                   , instUniform $ rv64_i_mem addrReg srcData dest imm
                   , instUniform $ rv32_i_mem addrReg srcData dest imm fenceOp1 fenceOp2
                   , inst $ jal zeroReg longImm
                   , readParams $ \p -> instUniform $ rv32_xcheri_mem (archDesc p) capsrc1 capsrc2 imm 0xb tmpReg
                   ]

genBSC_Jumps_Torture :: Template
genBSC_Jumps_Torture = random $ do
  imm_branches <- bits 7
  longImm_jumps <- bits 8
  imm <- bits 12
  longImm <- bits 20
  imm_jumps <- bits 7
  -- sbcRegs can return on of the following: 22,23,24,25,26,27,28,29
  src1 <- sbcRegs
  src2 <- sbcRegs
  dest <- sbcRegs
  let zeroReg = 0
  let retReg = 1
  dest_jumps <- frequency [ (1, return zeroReg), (1, return retReg) ]
  let regJump = 10
  return $ uniform [ instUniform $ rv64_i_arith src1 src2 dest imm
                   , instUniform $ rv32_i_arith src1 src2 dest imm longImm
                   , instUniform $ rv32_xcheri_inspection src1 dest
                   , instUniform $ rv32_i_ctrl_jumps regJump dest_jumps imm_jumps longImm_jumps
                   , instUniform $ rv32_i_ctrl_branches src1 src2 imm_branches
                   , instUniform $ rv32_xcheri_control src1 src2 dest
                   ]

genBSC_Excps_Torture :: Integer -> Template
genBSC_Excps_Torture tmpReg = random $ do
  imm <- bits 12
  longImm <- bits 20
  src1 <- sbcRegs
  src2 <- sbcRegs
  src3 <- sbcRegs
  srcSCr <- bits 5
  dest <- sbcRegs
  let rm = 0x7 -- dynamic rounding mode
  let fenceOp1 = 17
  let fenceOp2 = 18
  return $ uniform [ instUniform $ rv64_i_arith src1 src2 dest imm
                   , instUniform $ rv32_i_arith src1 src2 dest imm longImm
                   , instUniform $ rv64_i_mem src1 src2 dest imm
                   , instUniform $ rv32_i_mem src1 src2 dest imm fenceOp1 fenceOp2
                   , instUniform $ rv32_i_exc
                   , instUniform $ rv32_i_ctrl src1 src2 dest imm longImm
                   , readParams $ \p -> instUniform $ rv32_xcheri_mem (archDesc p) src1 src2 imm 0xb tmpReg
                   , instUniform $ rv32_xcheri_arithmetic src1 src2 imm dest
                   , instUniform $ rv32_xcheri_inspection src1 dest
                   , instUniform $ rv32_xcheri_misc src1 src2 srcSCr imm dest
                   --, instUniform $ rv32_f_macc src1 src2 src3 dest rm
                   --, instUniform $ rv32_f_arith src1 src2 dest rm
                   ]


genMOSC_Torture :: ArchDesc -> Integer -> Template
genMOSC_Torture arch tmpReg = random $ do
  imm <- bits 12
  longImm <- bits 20
  src1 <- sbcRegs
  src2 <- sbcRegs
  src3 <- sbcRegs
  srcSCr <- bits 5
  dest <- sbcRegs
  let rm = 0x7 -- dynamic rounding mode
  let fenceOp1 = 17
  let fenceOp2 = 18
  return $ uniform [ instUniform $ rv64_i_mem src1 src2 dest imm
                   , instUniform $ rv32_i_mem src1 src2 dest imm fenceOp1 fenceOp2
                   ]


genTSCTorture :: Template
genTSCTorture = random $ do
  imm_bits <- bits 10
  longImm <- bits 20
  src1 <- choose(16,18)
  src2 <- sbcRegs
  dest <- sbcRegs
  let fenceOp1 = 19
  let fenceOp2 = 20
  let sstatus = unsafe_csrs_indexFromName "sstatus"
  let imm = (imm_bits `shiftR` 3) `shiftL` 3
  let access_inst = uniform [ instUniform $ rv64_i_mem src1 src2 dest imm
                            , instUniform $ rv32_i_mem src1 src2 dest imm fenceOp1 fenceOp2
                            ]
  return $ mconcat [ access_inst
                   , csrr src2 sstatus
                   , inst $ sret
                   ]

prepareBSCExcpsGen :: Template
prepareBSCExcpsGen = random $ do
  let fcsr = unsafe_csrs_indexFromName "fcsr"
  let mstatus = unsafe_csrs_indexFromName "mstatus"
  let a0 = 10
  return $ mconcat [ inst $ lui a0 2
                   ,        csrs mstatus a0
                   , inst $ addi a0 0 192
                   ,        csrs fcsr a0
                   ]


prepareMOSCGen :: Template
prepareMOSCGen = random $ do
  let reg0 = 10
  let reg1 = 11
  return $ mconcat [ li64 reg0 0x80002000
                   , li64 reg1 0x80004000
                   ]

setUpPageTable :: Template
setUpPageTable = random $ do
  let a0 = 10
  let t0 = 6
  return $ mconcat [ li64 a0 0x80002000
                   , li64 t0 0x20000c01
                   , inst $ sd a0 t0 0
                   , li64 t0 0x20000801
                   , inst $ sd a0 t0 16
                   , li64 a0 0x80003000
                   , li64 t0 0x2000004b
                   , inst $ sd a0 t0 0
                   , li64 t0 0x20000447
                   , inst $ sd a0 t0 8
                   , li64 t0 0x2000105b
                   , inst $ sd a0 t0 32
                   , li64 t0 0x20001457
                   , inst $ sd a0 t0 40
                   ]

prepareTSCGen :: Template
prepareTSCGen = random $ do
  let s0 = 8
  let s1 = 9
  let s2 = 18
  let counterReg = 30
  let hpmCntIdx = 3
  let evt = 0x31
  let mstatus = unsafe_csrs_indexFromName "mstatus"
  let sstatus = unsafe_csrs_indexFromName "sstatus"
  let mepc = unsafe_csrs_indexFromName "mepc"
  let sepc = unsafe_csrs_indexFromName "sepc"
  let satp = unsafe_csrs_indexFromName "satp"
  let medeleg = unsafe_csrs_indexFromName "medeleg"
  let sedeleg = unsafe_csrs_indexFromName "sedeleg"
  let stval = unsafe_csrs_indexFromName "stval"
  return $ mconcat [ inst $ lui s1 0x100
                   ,        csrc mstatus s1
                   , inst $ lui s1 0x1
                   ,        csrc mstatus s1
                   , inst $ lui s1 0x1
                   , inst $ addi s1 s1 0x800
                   ,        csrs mstatus s1
                   , inst $ auipc s2 0
                   , inst $ addi s2 s2 16
                   ,        csrw mepc s2
                   , inst $ lui s2 0xa
                   ,        csrw medeleg s2
                   ,        setUpPageTable
                   ,        setupHPMEventSel counterReg hpmCntIdx evt
                   ,        enableHPMCounterM counterReg hpmCntIdx
                   , inst $ mret
                   , inst $ lui s0 0xfffe0
                   , inst $ addi s0 s0 1
                   , inst $ slli s0 s0 0x1b
                   , inst $ addi s0 s0 1
                   , inst $ slli s0 s0 0x13
                   , inst $ addi s0 s0 2
                   ,        csrw satp s0
                   , inst $ addi s1 s1 256
                   ,        csrc sstatus s1
                   , inst $ lui s2 2
                   ,        enableHPMCounterS counterReg hpmCntIdx
                   ,        li64 s2 0x80004000
                   ,        csrw sepc s2
                   ,        csrw stval s2
                   , inst $ sret
                   ]

-- | Verify Data Capability Speculation Constraint (CSC)
gen_csc_data_verify = random $ do
  let capReg = 1
  let tmpReg0 = 30
  let tmpReg1 = 31
  let bitsReg = 3
  let sldReg = 4
  let nopermReg = 5
  let authReg = 6
  let hpmEventIdx_dcache_miss = 0x31
  let hpmCntIdx = 3
  let prolog = mconcat [ makeCap capReg  authReg tmpReg1 0x80010000     8 0
                       , makeCap bitsReg authReg tmpReg1 0x80014000 0x100 0
                       , inst $ sentry sldReg bitsReg
                       , inst $ acperm nopermReg bitsReg 0
                       , inst $ cbld bitsReg 0 bitsReg -- clear tag
                       , inst $ lw tmpReg1 capReg 0
                       ]
  let body = surroundWithHPMAccess_core False hpmEventIdx_dcache_miss (repeatTillEnd (genCSCDataTorture capReg tmpReg1 bitsReg sldReg nopermReg authReg)) tmpReg0 hpmCntIdx Nothing
  let epilog = instAssert (addi tmpReg0 tmpReg0 0) 0
  return $ shrinkScope $ noShrink prolog <> body <> noShrink epilog



genJump :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
genJump memReg reg0 reg1 reg2 imm offset = random $ do
  let czero = 0
  let ra = 1
  return $ instSeq [ jalr_cap ra reg1
                   , jalr_cap czero ra
                   ]

genCSCInst :: Integer -> Integer -> Integer -> Integer -> Template
genCSCInst memReg reg0 reg1 reg2 = random $ do
  let czero = 0
  return $ instDist [ (1, jalr_cap czero reg0)
                    , (2, add 29 29 29)
                    , (1, cload reg1 reg2 0x8)
                    , (1, auipc reg2 0)
                    ]

-- | Verify instruction Capability Speculation Constraint (CSC)
gen_csc_inst_verify = random $ do
  let hpmEventIdx_dcache_miss = 0x31
  let hpmCntIdx_dcache_miss = 3
  let rand = 7
  let zeroReg = 0
  let jumpReg = 10
  let dataReg = 11
  let tmpReg = 12
  let counterReg = 13
  let authReg = 14
  let startReg = 15
  let pccReg = 16
  let loadReg = 17
  let authReg2 = 18
  let memReg = 19
  let memReg2 = 20
  let memReg3 = 21
  let reg0 = 23
  let reg1 = 24
  let reg2 = 25
  let mtcc = 28
  let startSeq = inst $ jalr_cap zeroReg startReg
  let trainSeq = repeatN (18) (genJump memReg tmpReg pccReg loadReg 0x20 0x0)
  let leakSeq = repeatN (1) (genJump memReg2 tmpReg pccReg loadReg 0x20 0x100)
  let tortSeq = startSeq <> leakSeq
  let prolog = mconcat [ switchEncodingMode
                       , inst $ cspecialrw authReg2 0 0 -- read PCC
                       , makeCap_core jumpReg authReg2 tmpReg 0x80001000
                       , makeCap_core pccReg authReg2 tmpReg 0x80002000
                       , makeCap_core memReg authReg2 tmpReg 0x80007000
                       , makeCap_core memReg2 authReg2 tmpReg 0x80007100
                       , inst $ cmv startReg jumpReg
                       , inst $ cstore jumpReg memReg 0x0c
                       , inst $ caddi tmpReg jumpReg 0x100
                       , inst $ cbld tmpReg 0 tmpReg -- clear tag
                       , inst $ cstore tmpReg memReg2 0x0c
                       , inst $ cload tmpReg memReg2 0x1f
                       , startSeq
                       , trainSeq
                       , inst $ cload tmpReg jumpReg 0x8
                       , inst $ caddi tmpReg jumpReg 0x40
                       , inst $ cload tmpReg tmpReg 0x8
                       , inst $ caddi tmpReg jumpReg 0x80
                       , inst $ cload tmpReg tmpReg 0x8
                       , inst $ caddi tmpReg jumpReg 0xc0
                       , inst $ cload tmpReg tmpReg 0x8
                       , inst $ caddi tmpReg jumpReg 0x100
                       , inst $ cload tmpReg tmpReg 0x8
                       , inst $ add pccReg zeroReg zeroReg
                       , inst $ cmv jumpReg startReg
                       -- zero out all sbcRegs
                       , inst $ cmv 22 zeroReg
                       , inst $ cmv 23 zeroReg
                       , inst $ cmv 24 zeroReg
                       , inst $ cmv 25 zeroReg
                       , inst $ cmv 26 zeroReg
                       , inst $ cmv 27 zeroReg
                       , inst $ cmv 28 zeroReg
                       , inst $ cmv 29 zeroReg
                       , inst $ scbndsi jumpReg jumpReg 1 16 -- 256
                       , inst $ scbndsi tmpReg startReg 1 16 -- 256
                       , inst $ cspecialrw 0 mtcc jumpReg
                       , startSeq
                       , inst $ fence 3 3 -- fence rw, rw
                       ]
  let body = surroundWithHPMAccess_core False hpmEventIdx_dcache_miss (repeatN (64)(genCSCInst memReg2 reg0 reg1 reg2)) counterReg hpmCntIdx_dcache_miss Nothing
  let epilog = instAssert (addi counterReg counterReg 0) 0
  return $ shrinkScope $ noShrink prolog <> body <> noShrink epilog

-- | Verify condition 1 of Branching Speculation Constraint (BSC)
gen_bsc_cond_1_verify = random $ do
  let tmpReg1 = 1
  let tmpReg2 = 2
  let tmpReg3 = 3
  let tmpReg4 = 4
  let addrReg = 5
  let capReg = 15
  let authReg = 16
  let tmpReg5 = 17
  let addrVal = 0x80001000
  let hpmEventIdx_renamed_insts = 0x70
  let hpmCntIdx_renamed_insts = 3
  let hpmEventIdx_traps = 0x2
  let hpmCntIdx_traps = 4
  let inner_hpm_access = surroundWithHPMAccess_core False hpmEventIdx_renamed_insts (repeatTillEnd genBSC_Cond_1_Torture) tmpReg1 hpmCntIdx_renamed_insts (Just (tmpReg2, tmpReg3))
  let outer_hpm_access = surroundWithHPMAccess_core False hpmEventIdx_traps inner_hpm_access tmpReg4 hpmCntIdx_traps Nothing
  let prolog = mconcat [ li64 addrReg addrVal
                       , makeCap capReg authReg tmpReg5 addrVal 0x10000 0 ]
  let body = outer_hpm_access
  let epilog = mconcat [ inst $ sub tmpReg3 tmpReg3 tmpReg2
                       , inst $ sub tmpReg1 tmpReg1 tmpReg3
                       , instAssert (sub tmpReg1 tmpReg1 tmpReg4) 2 ]
  return $ shrinkScope $ noShrink prolog <> body <> noShrink epilog

-- | Verify the jumping conditions of Branching Speculation Constraint (BSC)
gen_bsc_jumps_verify = random $ do
  let zeroReg = 0
  let tmpReg = 21
  let regJump = 10
  let addrVal = 0x80001000
  let hpmCntIdx_wild_jumps = 3
  let hpmEventIdx_wild_jumps = 0x71
  let prolog = li64 regJump addrVal
  let body = surroundWithHPMAccess_core False hpmEventIdx_wild_jumps (repeatTillEnd (genBSC_Jumps_Torture)) tmpReg hpmCntIdx_wild_jumps Nothing
  let epilog = instAssert (add tmpReg tmpReg zeroReg) 0
  return $ shrinkScope $ noShrink prolog <> body <> noShrink epilog

-- | Verify the exception conditions of Branching Speculation Constraint (BSC)
gen_bsc_exceptions_verify = random $ do
  let zeroReg = 0
  let capReg = 12
  let authReg = 13
  let excReg = 14
  let tmpReg = 11
  let counterReg = 31
  let addr = 0x80002000
  let hpmCntIdx_wild_excps = 3
  let hpmEventIdx_wild_excps = 0x72
  let prolog = mconcat [ prepareBSCExcpsGen
                       , makeCap capReg authReg tmpReg addr 0x100 0
                       , makeCap_core excReg authReg tmpReg 0x80000000
                       , inst $ cspecialrw 0 28 excReg ]
  let body = surroundWithHPMAccess_core False hpmEventIdx_wild_excps (repeatTillEnd (genBSC_Excps_Torture tmpReg)) counterReg hpmCntIdx_wild_excps Nothing
  let epilog = instAssert (add counterReg counterReg zeroReg) 0
  return $ shrinkScope $ noShrink prolog <> body <> noShrink epilog

-- | Verify Translation Speculation Constraint (TSC)
gen_tsc_verify = random $ do
  let lxReg = 1
  let addrReg = 2
  let tmpReg = 31
  let counterReg = 30
  let addrReg1 = 16
  let addrReg2 = 17
  let addrReg3 = 18
  let hpmCntIdx_dcache_miss = 3
  let hpmEventIdx_dcache_miss = 0x31
  let uepc = unsafe_csrs_indexFromName "uepc"
  let cntrIdx = hpmcounter_idx_to_counter_csr_idx 3
  let prolog = mconcat [ prepareTSCGen
                       , inst $ sfence 0 0
                       , li64 addrReg1 0x80001800
                       , li64 addrReg2 0x80012000
                       , li64 addrReg3 0x80008000
                       , csrr tmpReg cntrIdx ]
  let body = repeatN (20) (genTSCTorture)
  let epilog = mconcat [ csrr counterReg cntrIdx
                       , instAssert (sub counterReg counterReg tmpReg) 0 ]
  return $ shrinkScope $ noShrink prolog <> body <> noShrink epilog
