--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.GenCompressed (
  gen_rv_c
) where

import RISCV.RV_C
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.Compressed
import QuickCheckVEngine.Templates.Utils.General

gen_rv_c :: Template
gen_rv_c = random $ do
  imm       <- genCompressed_imm
  uimm      <- genCompressed_uimm
  nzimm     <- genCompressed_nzimm
  nzuimm    <- genCompressed_nzuimm
  rs1'      <- genCompressed_rs1'
  rs1'_rd'  <- genCompressed_rs1'_rd'
  rs1_nz    <- genCompressed_rs1_nz
  rs1_rd_nz <- genCompressed_rs1_rd_nz
  rs2       <- genCompressed_rs2
  rs2'      <- genCompressed_rs2'
  rs2_nz    <- genCompressed_rs2_nz
  rd        <- genCompressed_rd
  rd'       <- genCompressed_rd'
  rd_nz     <- genCompressed_rd_nz
  rd_nz_n2  <- genCompressed_rd_nz_n2
  let insts = rv_c imm uimm nzimm nzuimm
                   rs1' rs1'_rd' rs1_nz rs1_rd_nz
                   rs2 rs2' rs2_nz
                   rd rd' rd_nz rd_nz_n2
  return $ dist [ (9, instUniform insts), (1, prepReg32 rd) ]
