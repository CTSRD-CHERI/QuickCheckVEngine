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

{-|
    Module      : RISCV.RV_C
    Description : RISC-V compressed instructions extension

    The 'RISCV.RV_C' module provides the description of the RISC-V compressed
    instructions extension
-}

module RISCV.RV_C (
-- * RISC-V compressed instructions, instruction definitions
  c_illegal
, c_addi4spn
, c_fld
, c_flq
, c_lw
, c_flw
, c_ld
--, c_res_a
, c_fsd
, c_fsq
, c_sw
, c_fsw
, c_sd
, c_nop
, c_addi
, c_jal
, c_addiw
, c_li
, c_addi16sp
, c_lui
, c_srli64
, c_srli
, c_srai64
, c_srai
, c_andi
, c_sub
, c_xor
, c_or
, c_and
, c_subw
, c_addw
--, c_res_b
--, c_res_c
, c_j
, c_beqz
, c_bnez
, c_slli64
, c_slli
, c_fldsp
, c_lqsp
, c_lwsp
, c_flwsp
, c_ldsp
, c_jr
, c_mv
, c_ebreak
, c_jalr
, c_add
, c_fsdsp
, c_sqsp
, c_swsp
, c_fswsp
, c_sdsp
-- * RISC-V compressed instructions, others
, rv_c_disass
, rv_c
) where

import InstrCodec (DecodeBranch, (-->), encode)

c_illegal  = "000                                          00000000      000 00"
c_addi4spn = "000 nzuimm[5:4] nzuimm[9:6] nzuimm[2]       nzuimm[3] rd'[2:0] 00"
c_fld      = "001   uimm[5:3]             rs1'[2:0]       uimm[7:6] rd'[2:0] 00"
c_flq      = "001   uimm[5:4]     uimm[8] rs1'[2:0]       uimm[7:6] rd'[2:0] 00"
c_lw       = "010   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
c_flw      = "011   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
c_ld       = "011   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
--c_res_a    = "100                       _                          00"
c_fsd      = "101   uimm[5:3]            rs1'[2:0]       uimm[7:6] rs2'[2:0] 00"
c_fsq      = "101   uimm[5:4]   uimm[8]  rs1'[2:0]       uimm[7:6] rs2'[2:0] 00"
c_sw       = "110   uimm[5:3]            rs1'[2:0] uimm[2] uimm[6] rs2'[2:0] 00"
c_fsw      = "111   uimm[5:3]            rs1'[2:0] uimm[2] uimm[6] rs2'[2:0] 00"
c_sd       = "111   uimm[5:3]            rs1'[2:0]       uimm[7:6] rs2'[2:0] 00"

c_nop      = "000 nzimm[5]          00000 nzimm[4:0] 01"
c_addi     = "000 nzimm[5] rs1_rd_nz[4:0] nzimm[4:0] 01"
c_jal      = "001 imm[11] imm[4] imm[9:8] imm[10] imm[6] imm[7] imm[3:1] imm[5] 01"
c_addiw    = "001   imm[5] rs1_rd_nz[4:0]   imm[4:0] 01"
c_li       = "010   imm[5]     rd_nz[4:0]   imm[4:0] 01"
c_addi16sp = "011  nzimm[9] 00010 nzimm[4] nzimm[6] nzimm[8:7] nzimm[5] 01"
c_lui      = "011 nzimm[17] rd_nz_n2[4:0]                  nzimm[16:12] 01"
c_srli64   = "100         0 00 rs1'_rd'[2:0]            0 01"
c_srli     = "100 nzuimm[5] 00 rs1'_rd'[2:0]  nzuimm[4:0] 01"
c_srai64   = "100         0 01 rs1'_rd'[2:0]            0 01"
c_srai     = "100 nzuimm[5] 01 rs1'_rd'[2:0]  nzuimm[4:0] 01"
c_andi     = "100    imm[5] 10 rs1'_rd'[2:0]     imm[4:0] 01"
c_sub      = "100         0 11 rs1'_rd'[2:0] 00 rs2'[2:0] 01"
c_xor      = "100         0 11 rs1'_rd'[2:0] 01 rs2'[2:0] 01"
c_or       = "100         0 11 rs1'_rd'[2:0] 10 rs2'[2:0] 01"
c_and      = "100         0 11 rs1'_rd'[2:0] 11 rs2'[2:0] 01"
c_subw     = "100         1 11 rs1'_rd'[2:0] 00 rs2'[2:0] 01"
c_addw     = "100         1 11 rs1'_rd'[2:0] 01 rs2'[2:0] 01"
--c_res_b    = "100         1 11             _ 10         _ 01"
--c_res_c    = "100         1 11             _ 11         _ 01"
c_j        = "101 imm[11] imm[4] imm[9:8] imm[10] imm[6] imm[7] imm[3:1] imm[5] 01"
c_beqz     = "110 imm[8] imm[4:3] rs1'[2:0] imm[7:6] imm[2:1] imm[5] 01"
c_bnez     = "111 imm[8] imm[4:3] rs1'[2:0] imm[7:6] imm[2:1] imm[5] 01"

c_slli64   = "000         0 rs1_rd_nz[4:0]                   0 10"
c_slli     = "000 nzuimm[5] rs1_rd_nz[4:0]         nzuimm[4:0] 10"
c_fldsp    = "001   uimm[5]        rd[4:0] uimm[4:3] uimm[8:6] 10"
c_lqsp     = "001   uimm[5]     rd_nz[4:0]   uimm[4] uimm[9:6] 10"
c_lwsp     = "010   uimm[5]     rd_nz[4:0] uimm[4:2] uimm[7:6] 10"
c_flwsp    = "011   uimm[5]        rd[4:0] uimm[4:2] uimm[7:6] 10"
c_ldsp     = "011   uimm[5]     rd_nz[4:0] uimm[4:2] uimm[7:6] 10"
c_jr       = "100         0    rs1_nz[4:0]                   0 10"
c_mv       = "100         0     rd_nz[4:0]         rs2_nz[4:0] 10"
c_ebreak   = "100         1              0                   0 10"
c_jalr     = "100         1    rs1_nz[4:0]                   0 10"
c_add      = "100         1 rs1_rd_nz[4:0]         rs2_nz[4:0] 10"
c_fsdsp    = "101      uimm[5:3] uimm[8:6]            rs2[4:0] 10"
c_sqsp     = "101      uimm[5:4] uimm[9:6]            rs2[4:0] 10"
c_swsp     = "110      uimm[5:2] uimm[7:6]            rs2[4:0] 10"
c_fswsp    = "111      uimm[5:2] uimm[7:6]            rs2[4:0] 10"
c_sdsp     = "111      uimm[5:3] uimm[8:6]            rs2[4:0] 10"

-- | TODO Dissassembly of RISC-V compressed instructions
rv_c_disass :: [DecodeBranch String]
rv_c_disass = []

-- | TODO List of RISC-V compressed instructions
rv_c :: Integer -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer -> Integer
     -> [Integer]
rv_c imm uimm nzimm nzuimm
     rs1' rs1'_rd' rs1_nz rs1_rd_nz
     rs2 rs2' rs2_nz
     rd rd' rd_nz rd_nz_n2 = [ encode c_illegal
                             , encode c_addi4spn nzuimm rd'
                             , encode c_fld      uimm rs1' rd'
                             , encode c_flq      uimm rs1' rd'
                             , encode c_lw       uimm rs1' rd'
                             , encode c_flw      uimm rs1' rd'
                             , encode c_ld       uimm rs1' rd'
--                             , encode c_res_a
                             , encode c_fsd      uimm rs1' rs2'
                             , encode c_fsq      uimm rs1' rs2'
                             , encode c_sw       uimm rs1' rs2'
                             , encode c_fsw      uimm rs1' rs2'
                             , encode c_sd       uimm rs1' rs2'

                             , encode c_nop      nzimm
                             , encode c_addi     nzimm rs1_rd_nz
                             , encode c_jal      imm
                             , encode c_addiw    imm rs1_rd_nz
                             , encode c_li       imm rd_nz
                             , encode c_addi16sp nzimm
                             , encode c_lui      nzimm rd_nz_n2
                             , encode c_srli64   rs1'_rd'
                             , encode c_srli     nzuimm rs1'_rd'
                             , encode c_srai64   rs1'_rd'
                             , encode c_srai     nzuimm rs1'_rd'
                             , encode c_andi     imm rs1'_rd'
                             , encode c_sub      rs1'_rd' rs2'
                             , encode c_xor      rs1'_rd' rs2'
                             , encode c_or       rs1'_rd' rs2'
                             , encode c_and      rs1'_rd' rs2'
                             , encode c_subw     rs1'_rd' rs2'
                             , encode c_addw     rs1'_rd' rs2'
--                             , encode c_res_b
--                             , encode c_res_c
                             , encode c_j        imm
                             , encode c_beqz     imm rs1'
                             , encode c_bnez     imm rs1'

                             , encode c_slli64   rs1_rd_nz
                             , encode c_slli     nzuimm rs1_rd_nz
                             , encode c_fldsp    uimm rd
                             , encode c_lqsp     uimm rd_nz
                             , encode c_lwsp     uimm rd_nz
                             , encode c_flwsp    uimm rd
                             , encode c_ldsp     uimm rd_nz
                             , encode c_jr       rs1_nz
                             , encode c_mv       rd_nz rs2_nz
                             , encode c_ebreak
                             , encode c_jalr     rs1_nz
                             , encode c_add      rs1_rd_nz rs2_nz
                             , encode c_fsdsp    uimm rs2
                             , encode c_sqsp     uimm rs2
                             , encode c_swsp     uimm rs2
                             , encode c_fswsp    uimm rs2
                             , encode c_sdsp     uimm rs2 ]
