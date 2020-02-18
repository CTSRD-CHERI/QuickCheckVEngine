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

module QuickCheckVEngine.Templates.Utils.Compressed (
  genCompressed_imm
, genCompressed_uimm
, genCompressed_nzimm
, genCompressed_nzuimm
, genCompressed_rs1'
, genCompressed_rs1'_rd'
, genCompressed_rs1_nz
, genCompressed_rs1_rd_nz
, genCompressed_rs2
, genCompressed_rs2'
, genCompressed_rs2_nz
, genCompressed_rd
, genCompressed_rd'
, genCompressed_rd_nz
, genCompressed_rd_nz_n2
) where

import Test.QuickCheck
import QuickCheckVEngine.Templates.Utils.General

regs = [0..31]
regs_nz = [1..31]
regs_nz_n2 = 1:[3..31]
-- for regs', map (+8) for actual integer regiter index
regs' = [0..7]
regs'_nz = [1..7]

genCompressed_imm       = bits 6
genCompressed_uimm      = bits 8
genCompressed_nzimm     = bits 18
genCompressed_nzuimm    = bits 6
genCompressed_rs1'      = bits 3
genCompressed_rs1'_rd'  = oneof $ map return regs'
genCompressed_rs1_nz    = oneof $ map return regs_nz
genCompressed_rs1_rd_nz = oneof $ map return regs_nz
genCompressed_rs2       = oneof $ map return regs
genCompressed_rs2'      = oneof $ map return regs'
genCompressed_rs2_nz    = oneof $ map return regs_nz
genCompressed_rd        = oneof $ map return regs
genCompressed_rd'       = oneof $ map return regs'
genCompressed_rd_nz     = oneof $ map return regs_nz
genCompressed_rd_nz_n2  = oneof $ map return regs_nz_n2
