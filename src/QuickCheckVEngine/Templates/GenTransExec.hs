--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
-- Copyright (c) 2021 Jonathan Woodruff
-- Copyright (c) 2021 Franz Fuchs
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
  gen_data_scc_verify
, gen_sbc_cond_1_verify
, gen_inst_scc_verify
, gen_sbc_jumps_verify
, gen_sbc_exceptions_verify
, gen_stc_verify
) where

import InstrCodec
import Test.QuickCheck
import RISCV.RV32_I
import RISCV.RV64_I
import RISCV.RV32_Xcheri
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.GenMemory
import QuickCheckVEngine.Templates.Utils
import QuickCheckVEngine.RVFI_DII.RVFI
import QuickCheckVEngine.Templates.GenMemory
import Data.Bits

genDataSCCTorture :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
genDataSCCTorture capReg tmpReg bitsReg sldReg nopermReg authReg = Random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  size     <- getSize
  csrAddr  <- frequency [ (1, return 0xbc0), (1, return 0x342), (1, bits 12) ]
  src1     <- frequency [ (1, return capReg), (1, return tmpReg), (1, return bitsReg), (1, return sldReg) ]
  src2     <- frequency [ (1, return capReg), (1, return tmpReg), (1, return bitsReg), (1, return sldReg) ]
  let rv32_xcheri_misc_alt = filter (/= (cspecialrw tmpReg csrAddr src1)) (rv32_xcheri_misc src1 src2 csrAddr imm tmpReg)
  return $  (Distribution [ (1, uniformTemplate $ rv32_xcheri_arithmetic src1 src2 imm tmpReg)
                          , (1, uniformTemplate $ rv32_xcheri_misc_alt)
                          , (1, Single $ cinvoke src2 src1)
                          , (1, Single $ cload tmpReg tmpReg 0x08)
                          ])

genInstSCCTorture :: Template
genInstSCCTorture = Random $ do
  let src1 = 20
  let src2 = 21
  let tmpReg = 22
  let tmpReg2 = 23
  let capReg2 = 24
  let imm = 0x40
  let zeroReg = 0
  let capReg = 11
  return $ Sequence [ ( Single $ cjalr zeroReg capReg)
                    , ( Single $ auipc capReg2 0x0)
                    , ( Single $ cload tmpReg2 capReg2 0xb)
                    ]


genSBC_Cond_1_Torture :: Template
genSBC_Cond_1_Torture = Random $ do
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
  return $ (Distribution  [ (1, uniformTemplate $ rv64_i_arith src1 src2 imm tmpReg)
                          , (1, uniformTemplate $ rv32_i_arith src1 src2 imm longImm tmpReg)
                          , (1, uniformTemplate $ rv64_i_mem addrReg srcData dest imm)
                          , (1, uniformTemplate $ rv32_i_mem addrReg srcData dest imm fenceOp1 fenceOp2)
                          , (1, Single $ jal zeroReg longImm)
                          , (1, uniformTemplate $ rv32_xcheri_mem capsrc1 capsrc2 imm 0xb tmpReg)
                          ])

genSBC_Jumps_Torture :: Template
genSBC_Jumps_Torture = Random $ do
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
  return $ (Distribution  [ (1, uniformTemplate $ rv64_i_arith src1 src2 dest imm)
                          , (1, uniformTemplate $ rv32_i_arith src1 src2 dest imm longImm)
                          , (1, uniformTemplate $ rv32_i_ctrl_jumps regJump dest_jumps imm_jumps longImm_jumps)
                          , (1, uniformTemplate $ rv32_i_ctrl_branches src1 src2 imm_branches)
                          ])

genSBC_Excps_Torture :: Integer -> Template
genSBC_Excps_Torture tmpReg = Random $ do
  imm <- bits 12
  longImm <- bits 20
  src1 <- sbcRegs
  src2 <- sbcRegs
  dest <- sbcRegs
  let fenceOp1 = 17
  let fenceOp2 = 18
  return $ (Distribution  [ (1, uniformTemplate $ rv64_i_arith src1 src2 dest imm)
                          , (1, uniformTemplate $ rv32_i_arith src1 src2 dest imm longImm)
                          , (1, uniformTemplate $ rv64_i_mem src1 src2 dest imm)
                          , (1, uniformTemplate $ rv32_i_mem src1 src2 dest imm fenceOp1 fenceOp2)
                          , (1, uniformTemplate $ rv32_i_exc)
                          , (1, uniformTemplate $ rv32_xcheri_mem src1 src2 imm 0xb tmpReg)
                          ])


genSTCTorture :: Template
genSTCTorture = Random $ do
  imm <- bits 12
  longImm <- bits 20
  src1 <- sbcRegs
  src2 <- sbcRegs
  dest <- sbcRegs
  let fenceOp1 = 17
  let fenceOp2 = 18
  return $ (Distribution  [ (1, uniformTemplate $ rv64_i_arith src1 src2 dest imm)
                          , (1, uniformTemplate $ rv32_i_arith src1 src2 dest imm longImm)
                          , (1, uniformTemplate $ rv64_i_mem src1 src2 dest imm)
                          , (1, uniformTemplate $ rv32_i_mem src1 src2 dest imm fenceOp1 fenceOp2)
                          ])

gen_data_scc_verify = Random $ do
  let capReg = 1
  let tmpReg0 = 30
  let tmpReg1 = 31
  let bitsReg = 3
  let sldReg = 4
  let nopermReg = 5
  let authReg = 6
  let hpmEventIdx_dcache_miss = 0x31
  let hpmCntIdx = 3
  size <- getSize
  return $ Sequence [ NoShrink (makeCap capReg  authReg tmpReg1 0x80010000     8 0)
                    , NoShrink (makeCap bitsReg authReg tmpReg1 0x80014000 0x100 0)
                    , NoShrink (Single $ csealentry sldReg bitsReg)
                    , NoShrink (Single $ candperm nopermReg bitsReg 0)
                    , NoShrink (Single $ ccleartag bitsReg bitsReg)
                    , NoShrink (Single $ lw tmpReg1 capReg 0)
                    , surroundWithHPMAccess_core False hpmEventIdx_dcache_miss (replicateTemplate (size - 100) (genDataSCCTorture capReg tmpReg1 bitsReg sldReg nopermReg authReg)) tmpReg0 hpmCntIdx Nothing
                    , NoShrink (SingleAssert (addi tmpReg0 tmpReg0 0) 0)
                    ]



genJump :: Integer -> Integer -> Integer -> Integer -> Integer -> Template
genJump reg0 reg1 reg2 imm offset = Random $ do
  imm_bits <- bits 5
  let czero = 0
  return $ instSeq [ (cincoffsetimmediate reg0 reg0 imm)
                   , (cjalr czero reg0)
                   , (auipc reg1 0x1)
                   , (lw reg2 reg1 offset)
                   ]


-- | Verify instruction Speculative Capability Constraint (SCC)
gen_inst_scc_verify = Random $ do
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
  let startSeq = Sequence [NoShrink (Single $ cjalr zeroReg startReg)]
  let trainSeq = replicateTemplate (10) (genJump jumpReg pccReg loadReg 0xa0 0x0)
  let leakSeq = replicateTemplate (10) (genJump jumpReg pccReg loadReg 0xa0 0x80)
  let tortSeq = startSeq <> leakSeq
  return $ Sequence [ NoShrink (switchEncodingMode)
                    , NoShrink (makeCap_core jumpReg authReg tmpReg 0x80001000)
                    , NoShrink (Single $ cmove startReg jumpReg)
                    , startSeq
                    , NoShrink (trainSeq)
                    , NoShrink (Single $ csetboundsimmediate startReg startReg 0x8)
                    , NoShrink (Single $ lw loadReg startReg 0)
                    , NoShrink (Single $ lw loadReg pccReg 0)
                    , NoShrink (Single $ add pccReg zeroReg zeroReg)
                    , NoShrink (Single $ ccleartag jumpReg jumpReg)
                    , surroundWithHPMAccess_core False hpmEventIdx_dcache_miss (tortSeq) counterReg hpmCntIdx_dcache_miss Nothing
                    , NoShrink (SingleAssert (addi counterReg counterReg 0) 0)
                    ]


-- | Verify condition 1 of Speculative Branching Constraint (SBC)
gen_sbc_cond_1_verify = Random $ do
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
  size <- getSize
  let inner_hpm_access = surroundWithHPMAccess_core False hpmEventIdx_renamed_insts (replicateTemplate (size - 100) ( (genSBC_Cond_1_Torture))) tmpReg1 hpmCntIdx_renamed_insts (Just (tmpReg2, tmpReg3))
  let outer_hpm_access = surroundWithHPMAccess_core False hpmEventIdx_traps inner_hpm_access tmpReg4 hpmCntIdx_traps Nothing
  return $ Sequence [ NoShrink ( (li64 addrReg addrVal))
                    , NoShrink ( (makeCap capReg authReg tmpReg5 addrVal 0x10000 0))
                    , outer_hpm_access
                    , NoShrink (Single $ sub tmpReg3 tmpReg3 tmpReg2)
                    , NoShrink (Single $ sub tmpReg1 tmpReg1 tmpReg3)
                    , NoShrink (SingleAssert (sub tmpReg1 tmpReg1 tmpReg4) 2)
                    ]

-- | Verify the jumping conditions of Speculative Branching Constraint (SBC)
gen_sbc_jumps_verify = Random $ do
  let zeroReg = 0
  let tmpReg = 21
  let regJump = 10
  let addrVal = 0x80001000
  let hpmCntIdx_wild_jumps = 3
  let hpmEventIdx_wild_jumps = 0x71
  size <- getSize
  return $ Sequence [ NoShrink ((li64 regJump addrVal))
                    , surroundWithHPMAccess_core False hpmEventIdx_wild_jumps (replicateTemplate (size - 100) (genSBC_Jumps_Torture)) tmpReg hpmCntIdx_wild_jumps Nothing
                    , NoShrink(SingleAssert (add tmpReg tmpReg zeroReg) 0)
                    ]

-- | Verify the exception conditions of Speculative Branching Constraint (SBC)
gen_sbc_exceptions_verify = Random $ do
  let zeroReg = 0
  let capReg = 22
  let authReg = 23
  let tmpReg = 21
  let addr = 0x80002000
  let hpmCntIdx_wild_excps = 3
  let hpmEventIdx_wild_excps = 0x72
  size <- getSize
  return $ Sequence [ NoShrink ((li64 tmpReg 0x0))
                    , NoShrink ((makeCap capReg authReg tmpReg addr 0x100 0))
                    , surroundWithHPMAccess_core False hpmEventIdx_wild_excps (replicateTemplate (size - 100) (genSBC_Excps_Torture tmpReg)) tmpReg hpmCntIdx_wild_excps Nothing
                    , NoShrink(SingleAssert (add tmpReg tmpReg zeroReg) 0)
                    ]

gen_stc_verify = Random $ do
  let lxReg = 1
  let addrReg = 2
  let tmpReg = 3
  let pteReg = 30
  let hpmCntIdx_dcache_miss = 3
  let hpmEventIdx_dcache_miss = 0x31
  size <- getSize
  return $ (gen_pte_trans_core lxReg addrReg pteReg)
           <>
           Sequence [ surroundWithHPMAccess_core False hpmEventIdx_dcache_miss (replicateTemplate (size - 100) (genSTCTorture)) tmpReg hpmCntIdx_dcache_miss Nothing
                    , NoShrink(SingleAssert (addi tmpReg tmpReg 0) 0)
                    ]
