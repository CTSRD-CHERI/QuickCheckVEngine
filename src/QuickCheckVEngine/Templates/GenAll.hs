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

import InstrCodec
import Test.QuickCheck
import RISCV
import RISCV.ArchDesc
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.FP
import QuickCheckVEngine.Templates.Utils.General

genAll :: Template
genAll = readParams $ \params -> random $ do
  imm     <- bits 12
  csrIdx  <- csr (csrFilter params)
  src1    <- src
  src2    <- src
  src3    <- src
  dest    <- dest
  longImm <- bits 20
  fOp1    <- bits 4
  fOp2    <- bits 4
  aq      <- bits 1
  rl      <- bits 1
  rm      <- roundingMode
  uimm    <- bits 5
  offset  <- memOffset
  srcScr  <- elements [28, 29, 30, 31]
  let desc = archDesc params
  let insts = [[ (8, instUniform (rv32_i_arith src1 src2 dest imm longImm))
               , (8, instUniform (rv32_i_ctrl src1 src2 dest imm longImm))
               , (8, instUniform (rv32_i_mem src1 src2 dest offset fOp1 fOp2))
               , (32, inst $ lui dest 0x80008)
               ]]
           ++ [[ (8, instUniform (rv64_i_arith src1 src2 dest imm))
               , (8, instUniform (rv64_i_mem src1 src2 dest offset))
               ] | has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_m src1 src2 imm))
               ] | has_m desc]
           ++ [[ (8, instUniform (rv64_m src1 src2 imm))
               ] | has_m desc && has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_a src1 src2 dest aq rl))
               ] | has_a desc]
           ++ [[ (8, instUniform (rv64_a src1 src2 dest aq rl))
               ] | has_a desc && has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_f src1 src2 src3 dest rm imm))
               ] | has_f desc]
           ++ [[ (8, instUniform (rv64_f src1 dest rm))
               ] | has_f desc && has_xlen_64 desc]
           ++ [[ (8, instUniform (rv32_d src1 src2 src3 dest rm imm))
               ] | has_d desc]
           ++ [[ (8, instUniform (rv64_d src1 dest rm))
               ] | has_d desc && has_xlen_64 desc]
           ++ [[ (8, instUniform rv32_zifencei)
               ] | has_ifencei desc]
           ++ [[ (8, maybe mempty (\idx -> instUniform (rv32_zicsr src1 dest idx uimm)) csrIdx)
               ] | has_icsr desc]
           ++ [[ (8, instUniform (rv32_xcheri desc src1 src2 imm dest))
               ] | has_cheri desc]
  return $ fp_prologue $ repeatTillEnd (dist $ concat insts)
