--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019-2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.GenAll where

import Test.QuickCheck
import RISCV
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils

genAll :: ArchDesc -> Template
genAll desc = random $ do
  imm     <- bits 12
  src1    <- src
  src2    <- src
  src3    <- src
  dst     <- dest
  longImm <- bits 20
  fOp1    <- bits 4
  fOp2    <- bits 4
  aq      <- bits 1
  rl      <- bits 1
  rm      <- roundingMode
  mop       <- elements [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7,
                          0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf,
                          -- Skip LR,SC since these have non-determinism which is problematic for TestRIG
                          0x17,0x1f]
  uimm    <- bits 5
  offset  <- memOffset
  srcScr  <- elements [28, 29, 30, 31]
  let insts = [[ (8, instUniform (rv32_i_arith src1 src2 dst imm longImm))
               , (8, instUniform (rv32_i_ctrl src1 src2 dst imm longImm))
               , (8, instUniform (rv32_i_mem src1 src2 dst offset fOp1 fOp2))
               , (32, inst $ lui dst 0x80008)
               ]]
           ++ [[ (8, instUniform (rv64_i_arith src1 src2 dst imm))
               , (8, instUniform (rv64_i_mem src1 src2 dst offset))
               ] | has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_m src1 src2 imm))
               ] | has_m desc]
           ++ [[ (8, instUniform (rv64_m src1 src2 imm))
               ] | has_m desc && has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_a src1 src2 dst aq rl))
               ] | has_a desc]
           ++ [[ (8, instUniform (rv64_a src1 src2 dst aq rl))
               ] | has_a desc && has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_f src1 src2 src3 dst rm imm))
               ] | has_f desc]
           ++ [[ (8, instUniform (rv64_f src1 dst rm))
               ] | has_f desc && has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_d src1 src2 src3 dst rm imm))
               ] | has_d desc]
           ++ [[ (8, instUniform (rv64_d src1 dst rm))
               ] | has_d desc && has_xlen_64 desc]
           ++ [[ (8, instUniform rv32_zifencei)
               ] | has_ifencei desc]
           ++ [[ (8, instUniform (rv32_zicsr src1 dst imm uimm))
               ] | has_icsr desc]
           ++ [[ (8, instUniform (rv32_xcheri desc src1 src2 srcScr imm mop dst))
               ] | has_cheri desc]
  return $ shrinkScope $ (if has_f desc || has_d desc then noShrink (fp_prologue desc)
                                                      else mempty)
                         <> repeatTillEnd (dist $ concat insts)
