--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
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

import RISCV.Helpers (prettyCR, prettyCR_1op,
                      prettyCI, prettyCI_sig, prettyCI_F, prettyCI_reg,prettyCI_imm, prettyCI_sig_imm,
                      prettyCSS, prettyCSS_F,
                      prettyCIW,
                      prettyCL, prettyCL_F,
                      prettyCS, prettyCS_F,
                      prettyCA,
                      prettyCB, prettyCB_sig, prettyCB_reg,
                      prettyCJ)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

c_illegal_raw           =                            "000                                          00000000      000 00"
c_illegal               = encode c_illegal_raw
c_addi4spn_raw          =                            "000 nzuimm[5:4] nzuimm[9:6] nzuimm[2]       nzuimm[3] rd'[2:0] 00"
c_addi4spn rd' nzuimm   = encode c_addi4spn_raw           nzuimm                                            rd'
c_fld_raw               =                            "001   uimm[5:3]             rs1'[2:0]       uimm[7:6] rd'[2:0] 00"
c_fld rd' rs1' uimm     = encode c_fld_raw                  uimm                  rs1'                      rd'
c_flq_raw               =                            "001   uimm[5:4]     uimm[8] rs1'[2:0]       uimm[7:6] rd'[2:0] 00"
c_flq rd' rs1' uimm     = encode c_flq_raw                  uimm          uimm    rs1'                      rd'
c_lw_raw                =                            "010   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
c_lw rd' rs1' uimm      = encode c_lw_raw                   uimm                  rs1'                      rd'
c_flw_raw               =                            "011   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
c_flw rd' rs1' uimm     = encode c_flw_raw                  uimm                  rs1'                      rd'
c_ld_raw                =                            "011   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
c_ld rd' rs1' uimm      = encode c_ld_raw                   uimm                  rs1'                      rd'
--c_res_a_raw             =                            "100                       _                          00"
--c_res_a                 = encode c_res_a_raw
c_fsd_raw               =                            "101   uimm[5:3]            rs1'[2:0]       uimm[7:6] rs2'[2:0] 00"
c_fsd rs1' rs2' uimm    = encode c_fsd_raw                  uimm                 rs1'                      rs2'
c_fsq_raw               =                            "101   uimm[5:4]   uimm[8]  rs1'[2:0]       uimm[7:6] rs2'[2:0] 00"
c_fsq rs1' rs2' uimm    = encode c_fsq_raw                  uimm        uimm     rs1'                      rs2'
c_sw_raw                =                            "110   uimm[5:3]            rs1'[2:0] uimm[2] uimm[6] rs2'[2:0] 00"
c_sw rs1' rs2' uimm     = encode c_sw_raw                   uimm                 rs1'                      rs2'
c_fsw_raw               =                            "111   uimm[5:3]            rs1'[2:0] uimm[2] uimm[6] rs2'[2:0] 00"
c_fsw rs1' rs2' uimm    = encode c_fsw_raw                  uimm                 rs1'                      rs2'
c_sd_raw                =                            "111   uimm[5:3]            rs1'[2:0]       uimm[7:6] rs2'[2:0] 00"
c_sd rs1' rs2' uimm     = encode c_sd_raw                   uimm                 rs1'                      rs2'

c_nop_raw               =                            "000 nzimm[5]          00000 nzimm[4:0] 01"
c_nop nzimm             = encode c_nop_raw                nzimm
c_addi_raw              =                            "000 nzimm[5] rs1_rd_nz[4:0] nzimm[4:0] 01"
c_addi rs1_rd_nz nzimm  = encode c_addi_raw               nzimm    rs1_rd_nz
c_jal_raw               =                            "001 imm[11] imm[4] imm[9:8] imm[10] imm[6] imm[7] imm[3:1] imm[5] 01"
c_jal imm               = encode c_jal_raw                imm
c_addiw_raw             =                            "001   imm[5] rs1_rd_nz[4:0]   imm[4:0] 01"
c_addiw rs1_rd_nz imm   = encode c_addiw_raw                imm    rs1_rd_nz
c_li_raw                =                            "010   imm[5]     rd_nz[4:0]   imm[4:0] 01"
c_li rd_nz imm          = encode c_li_raw                   imm        rd_nz
c_addi16sp_raw          =                            "011  nzimm[9] 00010 nzimm[4] nzimm[6] nzimm[8:7] nzimm[5] 01"
c_addi16sp nzimm        = encode c_addi16sp_raw            nzimm
c_lui_raw               =                            "011 nzimm[17] rd_nz_n2[4:0]                  nzimm[16:12] 01"
c_lui rd_nz_n2 nzimm    = encode c_lui_raw                nzimm     rd_nz_n2
c_srli64_raw            =                            "100         0 00 rs1'_rd'[2:0]        00000 01"
c_srli64 rs1'_rd'       = encode c_srli64_raw                          rs1'_rd'
c_srli_raw              =                            "100 nzuimm[5] 00 rs1'_rd'[2:0]  nzuimm[4:0] 01"
c_srli rs1'_rd' nzuimm  = encode c_srli_raw               nzuimm       rs1'_rd'
c_srai64_raw            =                            "100         0 01 rs1'_rd'[2:0]        00000 01"
c_srai64 rs1'_rd'       = encode c_srai64_raw                          rs1'_rd'
c_srai_raw              =                            "100 nzuimm[5] 01 rs1'_rd'[2:0]  nzuimm[4:0] 01"
c_srai rs1'_rd' nzuimm  = encode c_srai_raw               nzuimm       rs1'_rd'
c_andi_raw              =                            "100    imm[5] 10 rs1'_rd'[2:0]     imm[4:0] 01"
c_andi rs1'_rd' imm     = encode c_andi_raw                  imm       rs1'_rd'
c_sub_raw               =                            "100         0 11 rs1'_rd'[2:0] 00 rs2'[2:0] 01"
c_sub rs1'_rd' rs2'     = encode c_sub_raw                             rs1'_rd'         rs2'
c_xor_raw               =                            "100         0 11 rs1'_rd'[2:0] 01 rs2'[2:0] 01"
c_xor rs1'_rd' rs2'     = encode c_xor_raw                             rs1'_rd'         rs2'
c_or_raw                =                            "100         0 11 rs1'_rd'[2:0] 10 rs2'[2:0] 01"
c_or rs1'_rd' rs2'      = encode c_or_raw                              rs1'_rd'         rs2'
c_and_raw               =                            "100         0 11 rs1'_rd'[2:0] 11 rs2'[2:0] 01"
c_and rs1'_rd' rs2'     = encode c_and_raw                             rs1'_rd'         rs2'
c_subw_raw              =                            "100         1 11 rs1'_rd'[2:0] 00 rs2'[2:0] 01"
c_subw rs1'_rd' rs2'    = encode c_subw_raw                            rs1'_rd'         rs2'
c_addw_raw              =                            "100         1 11 rs1'_rd'[2:0] 01 rs2'[2:0] 01"
c_addw rs1'_rd' rs2'    = encode c_addw_raw                            rs1'_rd'         rs2'
--c_res_b_raw             =                            "100         1 11             _ 10         _ 01"
--c_res_b                 = encode c_res_b_raw
--c_res_c_raw             =                            "100         1 11             _ 11         _ 01"
--c_res_c                 = encode c_res_c_raw
c_j_raw                 =                            "101 imm[11] imm[4] imm[9:8] imm[10] imm[6] imm[7] imm[3:1] imm[5] 01"
c_j imm                 = encode c_j_raw                  imm
c_beqz_raw              =                            "110 imm[8] imm[4:3] rs1'[2:0] imm[7:6] imm[2:1] imm[5] 01"
c_beqz rs1' imm         = encode c_beqz_raw               imm             rs1'
c_bnez_raw              =                            "111 imm[8] imm[4:3] rs1'[2:0] imm[7:6] imm[2:1] imm[5] 01"
c_bnez rs1' imm         = encode c_bnez_raw               imm             rs1'
c_slli64_raw            =                            "000         0 rs1_rd_nz[4:0]               00000 10"
c_slli64 rs1_rd_nz      = encode c_slli64_raw                       rs1_rd_nz
c_slli_raw              =                            "000 nzuimm[5] rs1_rd_nz[4:0]         nzuimm[4:0] 10"
c_slli rs1_rd_nz nzuimm = encode c_slli_raw               nzuimm    rs1_rd_nz
c_fldsp_raw             =                            "001   uimm[5]        rd[4:0] uimm[4:3] uimm[8:6] 10"
c_fldsp rd uimm         = encode c_fldsp_raw                uimm           rd
c_lqsp_raw              =                            "001   uimm[5]     rd_nz[4:0]   uimm[4] uimm[9:6] 10"
c_lqsp rd_nz uimm       = encode c_lqsp_raw                 uimm        rd_nz
c_lwsp_raw              =                            "010   uimm[5]     rd_nz[4:0] uimm[4:2] uimm[7:6] 10"
c_lwsp rd_nz uimm       = encode c_lwsp_raw                 uimm        rd_nz
c_flwsp_raw             =                            "011   uimm[5]        rd[4:0] uimm[4:2] uimm[7:6] 10"
c_flwsp rd uimm         = encode c_flwsp_raw                uimm           rd
c_ldsp_raw              =                            "011   uimm[5]     rd_nz[4:0] uimm[4:2] uimm[7:6] 10"
c_ldsp rd_nz uimm       = encode c_ldsp_raw                 uimm        rd_nz
c_jr_raw                =                            "100         0    rs1_nz[4:0]               00000 10"
c_jr rs1_nz             = encode c_jr_raw                              rs1_nz
c_mv_raw                =                            "100         0     rd_nz[4:0]         rs2_nz[4:0] 10"
c_mv rd_nz rs2_nz       = encode c_mv_raw                               rd_nz              rs2_nz
c_ebreak_raw            =                            "100         1          00000               00000 10"
c_ebreak                = encode c_ebreak_raw
c_jalr_raw              =                            "100         1    rs1_nz[4:0]               00000 10"
c_jalr rs1_nz           = encode c_jalr_raw                            rs1_nz
c_add_raw               =                            "100         1 rs1_rd_nz[4:0]         rs2_nz[4:0] 10"
c_add rs1_rd_nz rs2_nz  = encode c_add_raw                          rs1_rd_nz              rs2_nz
c_fsdsp_raw             =                            "101      uimm[5:3] uimm[8:6]            rs2[4:0] 10"
c_fsdsp rs2 uimm        = encode c_fsdsp_raw                   uimm                           rs2
c_sqsp_raw              =                            "101      uimm[5:4] uimm[9:6]            rs2[4:0] 10"
c_sqsp rs2 uimm         = encode c_sqsp_raw                    uimm                           rs2
c_swsp_raw              =                            "110      uimm[5:2] uimm[7:6]            rs2[4:0] 10"
c_swsp rs2 uimm         = encode c_swsp_raw                    uimm                           rs2
c_fswsp_raw             =                            "111      uimm[5:2] uimm[7:6]            rs2[4:0] 10"
c_fswsp rs2 uimm        = encode c_fswsp_raw                   uimm                           rs2
c_sdsp_raw              =                            "111      uimm[5:3] uimm[8:6]            rs2[4:0] 10"
c_sdsp rs2 uimm         = encode c_sdsp_raw                    uimm                           rs2

-- | Dissassembly of RISC-V compressed instructions padded to 32-bits.
-- | No instruction expansion or implicit operands are shown.
-- |
-- | Note: left-shifting of RV_C immediates is done in `decode`,
-- |       not in the pretty printing functions.
pad :: String -> String
pad = (++) "0000000000000000"
rv_c_disass :: [DecodeBranch String]
rv_c_disass = [ pad c_illegal_raw  --> "c.illegal"
              , pad c_addi4spn_raw --> prettyCIW "c.addi4spn"
              , pad c_fld_raw      --> prettyCL_F "c.fld"
              , pad c_flq_raw      --> prettyCL_F "c.flq"
              , pad c_lw_raw       --> prettyCL   "c.lw"
              , pad c_flw_raw      --> prettyCL_F "c.flw"
              , pad c_ld_raw       --> prettyCL   "c.ld"
--            , c_res_a_raw
              , pad c_fsd_raw      --> prettyCS_F "c.fsd"
              , pad c_fsq_raw      --> prettyCS_F "c.fsq"
              , pad c_sw_raw       --> prettyCS   "c.sw"
              , pad c_fsw_raw      --> prettyCS_F "c.fsw"
              , pad c_sd_raw       --> prettyCS   "c.sd"
              , pad c_nop_raw      --> prettyCI_imm "c.nop"
              , pad c_addi_raw     --> prettyCI_sig 6 "c.addi"
              , pad c_jal_raw      --> prettyCJ "c.jal"
              , pad c_addiw_raw    --> prettyCI_sig 6 "c.addiw"
              , pad c_li_raw       --> prettyCI_sig 6 "c.li"
              , pad c_addi16sp_raw --> prettyCI_sig_imm 10 "c.addi16sp"
              , pad c_lui_raw      --> prettyCI_sig 18 "c.lui"
              , pad c_srli64_raw   --> prettyCB_reg "c.srli64"
              , pad c_srli_raw     --> prettyCB     "c.srli"
              , pad c_srai64_raw   --> prettyCB_reg "c.srai64"
              , pad c_srai_raw     --> prettyCB     "c.srai"
              , pad c_andi_raw     --> prettyCB_sig 6 "c.andi"
              , pad c_sub_raw      --> prettyCA "c.sub"
              , pad c_xor_raw      --> prettyCA "c.xor"
              , pad c_or_raw       --> prettyCA "c.or"
              , pad c_and_raw      --> prettyCA "c.and"
              , pad c_subw_raw     --> prettyCA "c.subw"
              , pad c_addw_raw     --> prettyCA "c.addw"
--            , c_res_b_raw
--            , c_res_c_raw
              , pad c_j_raw        --> prettyCJ "c.j"
              , pad c_beqz_raw     --> prettyCB_sig 9 "c.beqz"
              , pad c_bnez_raw     --> prettyCB_sig 9 "c.bnez"
              , pad c_slli64_raw   --> prettyCI_reg "c.slli64"
              , pad c_slli_raw     --> prettyCI     "c.slli"
              , pad c_fldsp_raw    --> prettyCI_F "c.fldsp"
              , pad c_lqsp_raw     --> prettyCI   "c.lqsp"
              , pad c_lwsp_raw     --> prettyCI   "c.lwsp"
              , pad c_flwsp_raw    --> prettyCI_F "c.flwsp"
              , pad c_ldsp_raw     --> prettyCI   "c.ldsp"
              , pad c_jr_raw       --> prettyCR_1op "c.jr"
              , pad c_mv_raw       --> prettyCR "c.mv"
              , pad c_ebreak_raw   --> "c.ebreak"
              , pad c_jalr_raw     --> prettyCR_1op "c.jalr"
              , pad c_add_raw      --> prettyCR "c.add"
              , pad c_fsdsp_raw    --> prettyCSS_F "c.fsdsp"
              , pad c_sqsp_raw     --> prettyCSS   "c.sqsp"
              , pad c_swsp_raw     --> prettyCSS   "c.swsp"
              , pad c_fswsp_raw    --> prettyCSS_F "c.fswsp"
              , pad c_sdsp_raw     --> prettyCSS   "c.sdsp" ]

-- | List of RISC-V compressed instructions
rv_c :: Integer -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer -> Integer
     -> [Instruction]
rv_c imm uimm nzimm nzuimm
     rs1' rs1'_rd' rs1_nz rs1_rd_nz
     rs2 rs2' rs2_nz
     rd rd' rd_nz rd_nz_n2 = [ c_illegal
                             , c_addi4spn rd'      nzuimm
                             , c_fld      rd' rs1' uimm
                             , c_flq      rd' rs1' uimm
                             , c_lw       rd' rs1' uimm
                             , c_flw      rd' rs1' uimm
                             , c_ld       rd' rs1' uimm
--                           , c_res_a
                             , c_fsd      rs1' rs2' uimm
                             , c_fsq      rs1' rs2' uimm
                             , c_sw       rs1' rs2' uimm
                             , c_fsw      rs1' rs2' uimm
                             , c_sd       rs1' rs2' uimm

                             , c_nop      nzimm
                             , c_addi     rs1_rd_nz nzimm
                             , c_jal      imm
                             , c_addiw    rs1_rd_nz imm
                             , c_li       rd_nz imm
                             , c_addi16sp nzimm
                             , c_lui      rd_nz_n2 nzimm
                             , c_srli64   rs1'_rd'
                             , c_srli     rs1'_rd' nzuimm
                             , c_srai64   rs1'_rd'
                             , c_srai     rs1'_rd' nzuimm
                             , c_andi     rs1'_rd' imm
                             , c_sub      rs1'_rd' rs2'
                             , c_xor      rs1'_rd' rs2'
                             , c_or       rs1'_rd' rs2'
                             , c_and      rs1'_rd' rs2'
                             , c_subw     rs1'_rd' rs2'
                             , c_addw     rs1'_rd' rs2'
--                           , c_res_b
--                           , c_res_c
                             , c_j             imm
                             , c_beqz     rs1' imm
                             , c_bnez     rs1' imm

                             , c_slli64   rs1_rd_nz
                             , c_slli     rs1_rd_nz nzuimm
                             , c_fldsp    rd uimm
                             , c_lqsp     rd_nz uimm
                             , c_lwsp     rd_nz uimm
                             , c_flwsp    rd uimm
                             , c_ldsp     rd_nz uimm
                             , c_jr       rs1_nz
                             , c_mv       rd_nz rs2_nz
                             , c_ebreak
                             , c_jalr     rs1_nz
                             , c_add      rs1_rd_nz rs2_nz
                             , c_fsdsp    rs2 uimm
                             , c_sqsp     rs2 uimm
                             , c_swsp     rs2 uimm
                             , c_fswsp    rs2 uimm
                             , c_sdsp     rs2 uimm ]
