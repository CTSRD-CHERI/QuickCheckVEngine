--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
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
  gen_scc_verify
) where

import InstrCodec
import Test.QuickCheck
import RISCV.RV32_I
import RISCV.RV64_I
import RISCV.RV32_Xcheri
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import QuickCheckVEngine.RVFI_DII.RVFI
import QuickCheckVEngine.Templates.GenMemory

genSCCTorture :: Template
genSCCTorture = Random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  size     <- getSize
  csrAddr  <- frequency [ (1, return 0xbc0), (1, return 0x342), (1, bits 12) ]
  let capReg = 1
  let tmpReg = 2
  let bitsReg = 3
  let sldReg = 4
  let nopermReg = 5
  let authReg = 6
  src1     <- frequency [ (1, return capReg), (1, return tmpReg), (1, return bitsReg), (1, return sldReg) ]
  src2     <- frequency [ (1, return capReg), (1, return tmpReg), (1, return bitsReg), (1, return sldReg) ]
  return $  (Distribution [ (1, uniformTemplate $ rv32_xcheri_arithmetic src1 src2 imm tmpReg)
                          , (1, uniformTemplate $ rv32_xcheri_misc       src1 src2 imm csrAddr tmpReg)
                          , (1, Single $ cload tmpReg tmpReg 0x08)
                          ])
  --return $ replicateTemplate (size `div` 2) (Distribution [ (1, uniformTemplate $ rv32_xcheri_arithmetic srcAddr srcData imm dest)
  --                                                        , (1, gen_rv32_i_memory) ]))

genSBC_Cond_1_Torture :: Template
genSBC_Cond_1_Torture = Random $ do
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  let tmpReg = 1
  let src1 = 2
  let src2 = 3
  let captmpReg = 4
  let capsrc1 = 5
  let capsrc2 = 6
  return $ (Distribution  [ (1, uniformTemplate $ rv64_i_arith src1 src2 imm tmpReg)
                          , (1, uniformTemplate $ rv32_i_arith src1 src2 imm longImm tmpReg)
                          , (1, uniformTemplate $ rv64_i_mem srcAddr srcData dest imm)
                          , (1, uniformTemplate $ rv32_i_mem srcAddr srcData dest imm fenceOp1 fenceOp2)
                          , (1, uniformTemplate $ rv32_xcheri_arithmetic capsrc1 capsrc2 imm captmpReg)
                          ])

gen_scc_verify = Random $ do
  let capReg = 1
  let tmpReg = 2
  let bitsReg = 3
  let sldReg = 4
  let nopermReg = 5
  let authReg = 6
  let hpmEventIdx_dcache_miss = 0x31
  size <- getSize
  return $ Sequence [ NoShrink (makeCap capReg  authReg tmpReg 0x80010000     8 0)
                    , NoShrink (makeCap bitsReg authReg tmpReg 0x80014000 0x100 0)
                    , NoShrink (Single $ csealentry sldReg bitsReg)
                    , NoShrink (Single $ candperm nopermReg bitsReg 0)
                    , NoShrink (Single $ ccleartag bitsReg bitsReg)
                    , NoShrink (Single $ lw 0 tmpReg capReg)
                    , surroundWithHPMAccess_core False hpmEventIdx_dcache_miss (replicateTemplate (size - 100) genSCCTorture) tmpReg
                    , NoShrink (SingleAssert (addi tmpReg tmpReg 0) 0)
                    ]

-- | Verify instruction Speculative Capability Constraint (SCC)
--gen_inst_scc_verify = Random $ do

{-
    Actions to take:
    - prime the BTB (execute many jump instructions with the same target capability) running in unconstrained code
    - constrain code and pull in all cache lines reachable from the current PCC
    - start measurements (L1 D cache misses)
    - execute priming code sequence again
    - evaluate measurements
-}


-- | Verify condition 1 of Speculative Branching Constraint (SBC)
--gen_sbc_clause_1_verify = Random $ do
--  let tmpReg1 = 1
--  let tmpReg2 = 2
--  let hpmEventIdx_renamed_insts = 0x80
--  size <- getSize
--  return $ Sequence [ surroundWithHPMAccess_core False hpmEventIdx_renamed_insts (replicateTemplate (size - 100) genSBC_Cond_1_Torture) tmpReg1
--                    , NoShrink (Single $ csrr tmpReg2 minstret)
--                    , NoShrink (SingleAssert (sub tmpReg1 tmpReg1 tmpReg2) 1)
--                    ]
