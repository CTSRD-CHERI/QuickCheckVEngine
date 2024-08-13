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
, c_addi4spn_res
, c_fld
, c_lq
, c_lw
, c_flw
, c_ld
, c_res_a
, c_fsd
, c_sq
, c_sw
, c_fsw
, c_sd
, c_nop
, c_nop_hint
, c_addi
, c_addi_hint
, c_jal
, c_addiw
, c_addiw_res
, c_li
, c_li_hint
, c_addi16sp
, c_addi16sp_res
, c_lui
, c_lui_hint
, c_lui_res
, c_srli64
, c_srli64_hint
, c_srli
, c_srli_nse
, c_srai64
, c_srai64_hint
, c_srai
, c_srai_nse
, c_andi
, c_sub
, c_xor
, c_or
, c_and
, c_subw
, c_subw_res
, c_addw
, c_addw_res
, c_res_b
, c_res_c
, c_j
, c_beqz
, c_bnez
, c_slli64
, c_slli64_hint0
, c_slli64_hintx
, c_slli
, c_slli_hint
, c_slli_nse
, c_fldsp
, c_lqsp
, c_lwsp
, c_lwsp_res
, c_flwsp
, c_ldsp
, c_ldsp_res
, c_jr
, c_jr_res
, c_mv
, c_mv_hint
, c_ebreak
, c_jalr
, c_add
, c_add_hint
, c_fsdsp
, c_sqsp
, c_swsp
, c_fswsp
, c_sdsp
-- * RISC-V compressed instructions, others
, rv_c_disass
, rv_c
, rv_c_exh
) where

import RISCV.Helpers (XLen,
                      prettyCR, prettyCR_1op,
                      prettyCI, prettyCI_sig, prettyCI_F, prettyCI_reg, prettyCI_imm, prettyCI_sig_imm,
                      prettyCSS, prettyCSS_F,
                      prettyCIW, prettyCIW_reg,
                      prettyCL, prettyCL_F,
                      prettyCS, prettyCS_F,
                      prettyCA,
                      prettyCB, prettyCB_sig, prettyCB_reg,
                      prettyCJ,
                      prettyIgnr1, prettyIgnr2, prettyIgnr3)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

-- | RVC Quadrant 0
c_illegal_raw           =                            "000                                          00000000      000 00"
c_illegal               = encode c_illegal_raw
c_addi4spn_res_raw      =                            "000          00        0000         0               0 rd'[2:0] 00" -- REServed, nzimm==0
c_addi4spn_res rd'      = encode c_addi4spn_res_raw                                                         rd'          -- REServed, nzimm==0
c_addi4spn_raw          =                            "000 nzuimm[5:4] nzuimm[9:6] nzuimm[2]       nzuimm[3] rd'[2:0] 00"
c_addi4spn rd' nzuimm   = encode c_addi4spn_raw           nzuimm                                            rd'
c_fld_raw               =                            "001   uimm[5:3]             rs1'[2:0]       uimm[7:6] rd'[2:0] 00" -- V MULTI V
c_fld rd' rs1' uimm     = encode c_fld_raw                  uimm                  rs1'                      rd'          --  RV32DC/RV64DC
c_lq_raw                =                            "001   uimm[5:4]     uimm[8] rs1'[2:0]       uimm[7:6] rd'[2:0] 00" -- v
c_lq rd' rs1' uimm      = encode c_lq_raw                   uimm          uimm    rs1'                      rd'          --  RV128C
c_lw_raw                =                            "010   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00"
c_lw rd' rs1' uimm      = encode c_lw_raw                   uimm                  rs1'                      rd'
c_flw_raw               =                            "011   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00" -- V MULTI V
c_flw rd' rs1' uimm     = encode c_flw_raw                  uimm                  rs1'                      rd'          --  RV32FC
c_ld_raw                =                            "011   uimm[5:3]             rs1'[2:0] uimm[2] uimm[6] rd'[2:0] 00" -- v
c_ld rd' rs1' uimm      = encode c_ld_raw                   uimm                  rs1'                      rd'          --  RV64C/RV128C
c_res_a_raw             =                            "100                            ignr[10:0]                      00" -- Reserved instr
c_res_a ignr            = encode c_res_a_raw                                         ignr                                -- Reserved instr
c_fsd_raw               =                            "101   uimm[5:3]            rs1'[2:0]       uimm[7:6] rs2'[2:0] 00" -- V MULTI V
c_fsd rs1' rs2' uimm    = encode c_fsd_raw                  uimm                 rs1'                      rs2'          --  RV32DC/RV64DC
c_sq_raw                =                            "101   uimm[5:4]   uimm[8]  rs1'[2:0]       uimm[7:6] rs2'[2:0] 00" -- v
c_sq rs1' rs2' uimm     = encode c_sq_raw                   uimm        uimm     rs1'                      rs2'          --  RV128C
c_sw_raw                =                            "110   uimm[5:3]            rs1'[2:0] uimm[2] uimm[6] rs2'[2:0] 00"
c_sw rs1' rs2' uimm     = encode c_sw_raw                   uimm                 rs1'                      rs2'
c_fsw_raw               =                            "111   uimm[5:3]            rs1'[2:0] uimm[2] uimm[6] rs2'[2:0] 00" -- V MULTI V
c_fsw rs1' rs2' uimm    = encode c_fsw_raw                  uimm                 rs1'                      rs2'          --  RV32FC
c_sd_raw                =                            "111   uimm[5:3]            rs1'[2:0]       uimm[7:6] rs2'[2:0] 00" -- v
c_sd rs1' rs2' uimm     = encode c_sd_raw                   uimm                 rs1'                      rs2'          --  RV64C/RV128C

-- | RVC Quadrant 1
c_nop_raw               =                            "000        0          00000      00000 01"
c_nop                   = encode c_nop_raw
c_nop_hint_raw          =                            "000 nzimm[5]          00000 nzimm[4:0] 01" -- HINT, nzimm\=0
c_nop_hint nzimm        = encode c_nop_hint_raw           nzimm                                  -- HINT, nzimm\=0
c_addi_hint_raw         =                            "000        0 rs1_rd_nz[4:0]      00000 01" -- HINT, nzimm==0
c_addi_hint rs1_rd_nz   = encode c_addi_hint_raw                   rs1_rd_nz                     -- HINT, nzimm==0
c_addi_raw              =                            "000 nzimm[5] rs1_rd_nz[4:0] nzimm[4:0] 01"
c_addi rs1_rd_nz nzimm  = encode c_addi_raw               nzimm    rs1_rd_nz
c_jal_raw               =                            "001 imm[11] imm[4] imm[9:8] imm[10] imm[6] imm[7] imm[3:1] imm[5] 01" -- RV32
c_jal imm               = encode c_jal_raw                imm                                                               -- RV32
c_addiw_res_raw         =                            "001   imm[5]          00000   imm[4:0] 01" -- RV64C/RV128C; REServed, rd==0
c_addiw_res imm         = encode c_addiw_res_raw            imm                                  -- RV64C/RV128C; REServed, rd==0
c_addiw_raw             =                            "001   imm[5] rs1_rd_nz[4:0]   imm[4:0] 01" -- RV64C/RV128C
c_addiw rs1_rd_nz imm   = encode c_addiw_raw                imm    rs1_rd_nz                     -- RV64C/RV128C
c_li_hint_raw           =                            "010   imm[5]          00000   imm[4:0] 01" -- HINT, rd==0
c_li_hint imm           = encode c_li_hint_raw              imm                                  -- HINT, rd==0
c_li_raw                =                            "010   imm[5]     rd_nz[4:0]   imm[4:0] 01"
c_li rd_nz imm          = encode c_li_raw                   imm        rd_nz
c_addi16sp_res_raw      =                            "011         0 00010        0        0         00        0 01" -- REServed, nzimm==0
c_addi16sp_res          = encode c_addi16sp_res_raw                                                                 -- REServed, nzimm==0
c_addi16sp_raw          =                            "011  nzimm[9] 00010 nzimm[4] nzimm[6] nzimm[8:7] nzimm[5] 01"
c_addi16sp nzimm        = encode c_addi16sp_raw            nzimm
c_lui_hint_raw          =                            "011 nzimm[17]         00000                  nzimm[16:12] 01" -- HINT, rd==0
c_lui_hint nzimm        = encode c_lui_hint_raw           nzimm                                                     -- HINT, rd==0
c_lui_res_raw           =                            "011         0 rd_nz_n2[4:0]                         00000 01" -- REServed, nzimm==0
c_lui_res rd_nz_n2      = encode c_lui_res_raw                      rd_nz_n2                                        -- REServed, nzimm==0
c_lui_raw               =                            "011 nzimm[17] rd_nz_n2[4:0]                  nzimm[16:12] 01"
c_lui rd_nz_n2 nzimm    = encode c_lui_raw                nzimm     rd_nz_n2
c_srli64_raw            =                            "100         0 00 rs1'_rd'[2:0]        00000 01" -- V MULTI V
c_srli64_hint rs1'_rd'  = encode c_srli64_raw                          rs1'_rd'                       --  RV32C/RV64C HINT instr
c_srli64 rs1'_rd'       = encode c_srli64_raw                          rs1'_rd'                       --  RV128C
c_srli_nse_raw          =                            "100         1 00 rs1'_rd'[2:0]  nzuimm[4:0] 01" -- RV32C NSE (reserved for custom extensions)
c_srli_nse rs1'_rd' nzuimm = encode c_srli_nse_raw        nzuimm       rs1'_rd'                       -- RV32C NSE (reserved for custom extensions)
c_srli_raw              =                            "100 nzuimm[5] 00 rs1'_rd'[2:0]  nzuimm[4:0] 01"
c_srli rs1'_rd' nzuimm  = encode c_srli_raw               nzuimm       rs1'_rd'
c_srai64_raw            =                            "100         0 01 rs1'_rd'[2:0]        00000 01" -- V MULTI V
c_srai64 rs1'_rd'       = encode c_srai64_raw                          rs1'_rd'                       --  RV128C
c_srai64_hint rs1'_rd'  = encode c_srai64_raw                          rs1'_rd'                       --  RV32C/RV64C HINT instr
c_srai_nse_raw          =                            "100         1 01 rs1'_rd'[2:0]  nzuimm[4:0] 01" -- RV32C NSE (reserved for custom extensions)
c_srai_nse rs1'_rd' nzuimm = encode c_srai_nse_raw                     rs1'_rd'       nzuimm          -- RV32C NSE (reserved for custom extensions)
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
c_subw_raw              =                            "100         1 11 rs1'_rd'[2:0] 00 rs2'[2:0] 01" -- V MULTI V
c_subw rs1'_rd' rs2'    = encode c_subw_raw                            rs1'_rd'         rs2'          --  RV64C/RV128C
c_subw_res rs1'_rd' rs2' = encode c_subw_raw                           rs1'_rd'         rs2'          --  RV32C REServed instr
c_addw_raw              =                            "100         1 11 rs1'_rd'[2:0] 01 rs2'[2:0] 01" -- V MULTI V
c_addw rs1'_rd' rs2'    = encode c_addw_raw                            rs1'_rd'         rs2'          --  RV64C/RV128C
c_addw_res rs1'_rd' rs2' = encode c_addw_raw                           rs1'_rd'         rs2'          --  RV32C REServed instr
c_res_b_raw             =                            "100         1 11     ignr[5:3] 10 ignr[2:0] 01" -- Reserved instr
c_res_b ignr            = encode c_res_b_raw                               ignr                       -- Reserved instr
c_res_c_raw             =                            "100         1 11     ignr[5:3] 11 ignr[2:0] 01" -- Reserved instr
c_res_c ignr            = encode c_res_c_raw                               ignr                       -- Reserved instr
c_j_raw                 =                            "101 imm[11] imm[4] imm[9:8] imm[10] imm[6] imm[7] imm[3:1] imm[5] 01"
c_j imm                 = encode c_j_raw                  imm
c_beqz_raw              =                            "110 imm[8] imm[4:3] rs1'[2:0] imm[7:6] imm[2:1] imm[5] 01"
c_beqz rs1' imm         = encode c_beqz_raw               imm             rs1'
c_bnez_raw              =                            "111 imm[8] imm[4:3] rs1'[2:0] imm[7:6] imm[2:1] imm[5] 01"
c_bnez rs1' imm         = encode c_bnez_raw               imm             rs1'

-- | RVC Quadrant 2
c_slli64_hint0_raw      =                            "000         0          00000               00000 10" -- HINT, rd==0 (overlap w/ c_slli64_hintx for RV32C/RV64C)
c_slli64_hint0          = encode c_slli64_hint0_raw                                                        -- HINT, rd==0 (overlap w/ c_slli64_hintx for RV32C/RV64C)
c_slli64_raw            =                            "000         0 rs1_rd_nz[4:0]               00000 10" -- V MULTI V
c_slli64 rs1_rd_nz      = encode c_slli64_raw                       rs1_rd_nz                              --  RV128C
c_slli64_hintx rs1_rd_nz = encode c_slli64_raw                      rs1_rd_nz                              --  RV32C/RV64C HINT instr
c_slli_hint_raw         =                            "000 nzuimm[5]          00000         nzuimm[4:0] 10" -- HINT, rd==0
c_slli_hint nzuimm      = encode c_slli_hint_raw          nzuimm                                           -- HINT, rd==0
c_slli_nse_raw          =                            "000         1 rs1_rd_nz[4:0]         nzuimm[4:0] 10" -- RV32C NSE (reserved for custom extensions)
c_slli_nse rs1_rd_nz nzuimm = encode c_slli_nse_raw                 rs1_rd_nz              nzuimm          -- RV32C NSE (reserved for custom extensions)
c_slli_raw              =                            "000 nzuimm[5] rs1_rd_nz[4:0]         nzuimm[4:0] 10"
c_slli rs1_rd_nz nzuimm = encode c_slli_raw               nzuimm    rs1_rd_nz
c_fldsp_raw             =                            "001   uimm[5]        rd[4:0] uimm[4:3] uimm[8:6] 10" -- RV32DC/RV64DC
c_fldsp rd uimm         = encode c_fldsp_raw                uimm           rd                              -- RV32DC/RV64DC
c_lqsp_res_raw          =                            "001   uimm[5]          00000   uimm[4] uimm[9:6] 10" -- RV128C; REServed, rd==0
c_lqsp_res uimm         = encode c_lqsp_res_raw             uimm                                           -- RV128C; REServed, rd==0
c_lqsp_raw              =                            "001   uimm[5]     rd_nz[4:0]   uimm[4] uimm[9:6] 10" -- RV128C
c_lqsp rd_nz uimm       = encode c_lqsp_raw                 uimm        rd_nz                              -- RV128C
c_lwsp_res_raw          =                            "010   uimm[5]          00000 uimm[4:2] uimm[7:6] 10" -- REServed, rd==0
c_lwsp_res uimm         = encode c_lwsp_res_raw             uimm                                           -- REServed, rd==0
c_lwsp_raw              =                            "010   uimm[5]     rd_nz[4:0] uimm[4:2] uimm[7:6] 10"
c_lwsp rd_nz uimm       = encode c_lwsp_raw                 uimm        rd_nz
c_flwsp_raw             =                            "011   uimm[5]        rd[4:0] uimm[4:2] uimm[7:6] 10" -- v MULTI V
c_flwsp rd uimm         = encode c_flwsp_raw                uimm           rd                              --  RV32FC
c_ldsp_res_raw          =                            "011   uimm[5]          00000 uimm[4:2] uimm[7:6] 10" -- v
c_ldsp_res uimm         = encode c_ldsp_res_raw             uimm                                           --  RV64C/RV128C; REServed, rd==0
c_ldsp_raw              =                            "011   uimm[5]     rd_nz[4:0] uimm[4:2] uimm[7:6] 10" -- v
c_ldsp rd_nz uimm       = encode c_ldsp_raw                 uimm        rd_nz                              --  RV64C/RV128C
c_jr_res_raw            =                            "100         0          00000               00000 10" -- REServed, rs1==0
c_jr_res                = encode c_jr_res_raw                                                              -- REServed, rs1==0
c_jr_raw                =                            "100         0    rs1_nz[4:0]               00000 10"
c_jr rs1_nz             = encode c_jr_raw                              rs1_nz
c_mv_hint_raw           =                            "100         0          00000         rs2_nz[4:0] 10" -- HINT, rd==0
c_mv_hint rs2_nz        = encode c_mv_hint_raw                                             rs2_nz          -- HINT, rd==0
c_mv_raw                =                            "100         0     rd_nz[4:0]         rs2_nz[4:0] 10"
c_mv rd_nz rs2_nz       = encode c_mv_raw                               rd_nz              rs2_nz
c_ebreak_raw            =                            "100         1          00000               00000 10"
c_ebreak                = encode c_ebreak_raw
c_jalr_raw              =                            "100         1    rs1_nz[4:0]               00000 10"
c_jalr rs1_nz           = encode c_jalr_raw                            rs1_nz
c_add_hint_raw          =                            "100         1          00000         rs2_nz[4:0] 10" -- HINT, rd==0
c_add_hint rs2_nz       = encode c_add_hint_raw                                            rs2_nz          -- HINT, rd==0
c_add_raw               =                            "100         1 rs1_rd_nz[4:0]         rs2_nz[4:0] 10"
c_add rs1_rd_nz rs2_nz  = encode c_add_raw                          rs1_rd_nz              rs2_nz
c_fsdsp_raw             =                            "101      uimm[5:3] uimm[8:6]            rs2[4:0] 10" -- RV32DC/RV64DC
c_fsdsp rs2 uimm        = encode c_fsdsp_raw                   uimm                           rs2          -- RV32DC/RV64DC
c_sqsp_raw              =                            "101      uimm[5:4] uimm[9:6]            rs2[4:0] 10" -- RV128C
c_sqsp rs2 uimm         = encode c_sqsp_raw                    uimm                           rs2          -- RV128C
c_swsp_raw              =                            "110      uimm[5:2] uimm[7:6]            rs2[4:0] 10"
c_swsp rs2 uimm         = encode c_swsp_raw                    uimm                           rs2
c_fswsp_raw             =                            "111      uimm[5:2] uimm[7:6]            rs2[4:0] 10" -- RV32FC
c_fswsp rs2 uimm        = encode c_fswsp_raw                   uimm                           rs2          -- RV32FC
c_sdsp_raw              =                            "111      uimm[5:3] uimm[8:6]            rs2[4:0] 10" -- RV64C/RV128C
c_sdsp rs2 uimm         = encode c_sdsp_raw                    uimm                           rs2          -- RV64C/RV128C

-- | Disassembly of RISC-V compressed instructions padded to 32-bits.
-- | Precise across all encodings only when XLEN is specified,
-- | otherwise will show both possibilities when two encodings overlap.
-- | No instruction expansion or implicit operands are shown.
-- |
-- | Note: left-shifting of RV_C immediates is done in `decode`,
-- |       not in the pretty printing functions.
pad :: String -> String
pad = (++) "0000000000000000"
rv_c_disass :: Maybe XLen -> [DecodeBranch String]
rv_c_disass (Just ixl) | ixl == 1  = rv_c_32_disass
                       | ixl == 2  = rv_c_64_disass
                       | ixl == 3  = rv_c_128_disass
                       | otherwise = rv_c_multi_disass
rv_c_disass Nothing = rv_c_multi_disass

-- | Precise RV32C disassembly
rv_c_32_disass :: [DecodeBranch String]
rv_c_32_disass = [ pad c_fld_raw          --> prettyCL_F "c.fld"
                 , pad c_flw_raw          --> prettyCL_F "c.flw"
                 , pad c_fsd_raw          --> prettyCS_F "c.fsd"
                 , pad c_fsw_raw          --> prettyCS_F "c.fsw"

                 , pad c_jal_raw          --> prettyCJ "c.jal"
                 , pad c_srli_nse_raw     --> prettyCB "c.srli.nse"
                 , pad c_srli64_raw       --> prettyCB_reg "c.srli64.hint"
                 , pad c_srai_nse_raw     --> prettyCB "c.srai.nse"
                 , pad c_srai64_raw       --> prettyCB_reg "c.srai64.hint"
                 , pad c_subw_raw         --> prettyCA "c.subw.res"
                 , pad c_addw_raw         --> prettyCA "c.addw.res"

                 , pad c_slli_nse_raw     --> prettyCI "c.slli.nse"
                 , pad c_slli64_raw       --> prettyCI_reg "c.slli64.hint"
                 , pad c_fldsp_raw        --> prettyCI_F "c.fldsp"
                 , pad c_flwsp_raw        --> prettyCI_F "c.flwsp"
                 , pad c_fsdsp_raw        --> prettyCSS_F "c.fsdsp"
                 , pad c_fswsp_raw        --> prettyCSS_F "c.fswsp"
                 ] ++ rv_c_common_disass

-- | Precise RV64C disassembly
rv_c_64_disass :: [DecodeBranch String]
rv_c_64_disass = [ pad c_fld_raw          --> prettyCL_F "c.fld"
                 , pad c_ld_raw           --> prettyCL   "c.ld"
                 , pad c_fsd_raw          --> prettyCS_F "c.fsd"
                 , pad c_sd_raw           --> prettyCS   "c.sd"

                 , pad c_addiw_raw        --> prettyCI_sig     6 "c.addiw"
                 , pad c_addiw_res_raw    --> prettyCI_sig_imm 6 "c.addiw.res"
                 , pad c_srli64_raw       --> prettyCB_reg "c.srli64.hint"
                 , pad c_srai64_raw       --> prettyCB_reg "c.srai64.hint"
                 , pad c_subw_raw         --> prettyCA "c.subw"
                 , pad c_addw_raw         --> prettyCA "c.addw"

                 , pad c_slli64_raw       --> prettyCI_reg "c.slli64.hint"
                 , pad c_ldsp_res_raw     --> prettyCI_imm  "c.ldsp.res"
                 , pad c_ldsp_raw         --> prettyCI      "c.ldsp"
                 , pad c_fsdsp_raw        --> prettyCSS_F "c.fsdsp"
                 , pad c_sdsp_raw         --> prettyCSS "c.sdsp"
                 ] ++ rv_c_common_disass

-- | Precise RV128C disassembly
rv_c_128_disass :: [DecodeBranch String]
rv_c_128_disass = [ pad c_lq_raw           --> prettyCL   "c.lq"
                  , pad c_ld_raw           --> prettyCL   "c.ld"
                  , pad c_sq_raw           --> prettyCS_F "c.sq"
                  , pad c_sd_raw           --> prettyCS   "c.sd"

                  , pad c_addiw_raw        --> prettyCI_sig     6 "c.addiw"
                  , pad c_addiw_res_raw    --> prettyCI_sig_imm 6 "c.addiw.res"
                  , pad c_srli64_raw       --> prettyCB_reg "c.srli64"
                  , pad c_srai64_raw       --> prettyCB_reg "c.srai64"
                  , pad c_subw_raw         --> prettyCA "c.subw"
                  , pad c_addw_raw         --> prettyCA "c.addw"

                  , pad c_slli64_raw       --> prettyCI_reg "c.slli64"
                  , pad c_lqsp_res_raw     --> prettyCI_imm "c.lqsp.res"
                  , pad c_lqsp_raw         --> prettyCI     "c.lqsp"
                  , pad c_ldsp_res_raw     --> prettyCI_imm  "c.ldsp.res"
                  , pad c_ldsp_raw         --> prettyCI      "c.ldsp"
                  , pad c_sqsp_raw         --> prettyCSS "c.sqsp"
                  , pad c_sdsp_raw         --> prettyCSS "c.sdsp"
                  ] ++ rv_c_common_disass

-- | General RV32C/RV64C/RV128C disassembly
rv_c_multi_disass :: [DecodeBranch String]
rv_c_multi_disass = [ pad c_lq_raw           --> prettyIgnr3 "c.fld (RV32DC/RV64DC) / c.lq (RV128C)"
                    , pad c_ld_raw           --> prettyIgnr3 "c.flw (RV32FC) / c.ld (RV64C/RV128C)"
                    , pad c_sq_raw           --> prettyIgnr3 "c.fsd (RV32DC/RV64DC) / c.sq (RV128C)"
                    , pad c_sd_raw           --> prettyIgnr3 "c.fsw (RV32FC) / c.sd (RV64C/RV128C)"

                    , pad c_jal_raw          --> prettyIgnr1 "c.jal (RV32C) / c.addiw (RV64C/RV128C)"
                    , pad c_srli_nse_raw     --> prettyIgnr2 "c.srli.nse (RV32C) / c.srli (RV64C/RV128C)"
                    , pad c_srli64_raw       --> prettyCB_reg "c.srli64.hint (RV32C/RV64C) / c.srli64 (RV128C)"
                    , pad c_srai_nse_raw     --> prettyIgnr2 "c.srai.nse (RV32C) / c.srai (RV64C/RV128C)"
                    , pad c_srai64_raw       --> prettyCB_reg "c.srai64.hint (RV32C/RV64C) / c.srai64 (RV128C)"
                    , pad c_subw_raw         --> prettyCA "c.subw.res (RV32C) / c.subw (RV64C/RV128C)"
                    , pad c_addw_raw         --> prettyCA "c.addw.res (RV32C) / c.addw (RV64C/RV128C)"

                    , pad c_slli_nse_raw     --> prettyIgnr2 "c.slli.nse (RV32C) / c.slli (RV64C/RV128C)"
                    , pad c_slli64_raw       --> prettyCI_reg "c.slli64.hint (RV32C/RV64C) / c.slli64 (RV128C)"
                    , pad c_lqsp_res_raw     --> prettyIgnr1 "c.fldsp (RV32DC/RV64DC) / c.lqsp.res (RV128C)"
                    , pad c_lqsp_raw         --> prettyIgnr2 "c.fldsp (RV32DC/RV64DC) / c.lqsp (RV128C)"
                    , pad c_ldsp_res_raw     --> prettyIgnr1 "c.flwsp (RV32FC) / c.ldsp.res (RV64C/RV128C)"
                    , pad c_ldsp_raw         --> prettyIgnr2 "c.flwsp (RV32FC) / c.ldsp (RV64C/RV128C)"
                    , pad c_sqsp_raw         --> prettyIgnr2 "c.fsdsp (RV32DC/RV64DC) / c.sqsp (RV128C)"
                    , pad c_sdsp_raw         --> prettyIgnr2 "c.fswsp (RV32FC) / c.sdsp (RV64C/RV128C)"
                    ] ++ rv_c_common_disass

-- | Disassembly of the common subset of compressed instruction encodings
-- | across RV32C, RV64C, and RV128C.
-- | Should be a correct disassembly regardless of XLEN.
rv_c_common_disass :: [DecodeBranch String]
rv_c_common_disass = [ pad c_illegal_raw      --> "c.illegal"
                     , pad c_addi4spn_raw     --> prettyCIW     "c.addi4spn"
                     , pad c_addi4spn_res_raw --> prettyCIW_reg "c.addi4spn.res"
                     , pad c_lw_raw           --> prettyCL "c.lw"
                     , pad c_res_a_raw        --> prettyIgnr1 "c.reserved"
                     , pad c_sw_raw           --> prettyCS "c.sw"

                     , pad c_nop_raw          -->              "c.nop"
                     , pad c_nop_hint_raw     --> prettyCI_imm "c.nop.hint"
                     , pad c_addi_hint_raw    --> prettyCI_reg   "c.addi.hint"
                     , pad c_addi_raw         --> prettyCI_sig 6 "c.addi"
                     , pad c_li_hint_raw      --> prettyCI_sig_imm 6 "c.li.hint"
                     , pad c_li_raw           --> prettyCI_sig     6 "c.li"
                     , pad c_addi16sp_res_raw -->                     "c.addi16sp.res"
                     , pad c_addi16sp_raw     --> prettyCI_sig_imm 10 "c.addi16sp"
                     , pad c_lui_hint_raw     --> prettyCI_sig_imm 18  "c.lui.hint"
                     , pad c_lui_res_raw      --> prettyCI_reg         "c.lui.res"
                     , pad c_lui_raw          --> prettyCI_sig     18  "c.lui"
                     , pad c_srli_raw         --> prettyCB "c.srli"
                     , pad c_srai_raw         --> prettyCB "c.srai"
                     , pad c_andi_raw         --> prettyCB_sig 6 "c.andi"
                     , pad c_sub_raw          --> prettyCA "c.sub"
                     , pad c_xor_raw          --> prettyCA "c.xor"
                     , pad c_or_raw           --> prettyCA "c.or"
                     , pad c_and_raw          --> prettyCA "c.and"
                     , pad c_res_b_raw        --> prettyIgnr1 "c.reserved"
                     , pad c_res_c_raw        --> prettyIgnr1 "c.reserved"
                     , pad c_j_raw            --> prettyCJ "c.j"
                     , pad c_beqz_raw         --> prettyCB_sig 9 "c.beqz"
                     , pad c_bnez_raw         --> prettyCB_sig 9 "c.bnez"

                     , pad c_slli64_hint0_raw --> "c.slli64.hint"
                     , pad c_slli_hint_raw    --> prettyCI_imm "c.slli.hint"
                     , pad c_slli_raw         --> prettyCI     "c.slli"
                     , pad c_lwsp_res_raw     --> prettyCI_imm  "c.lwsp.res"
                     , pad c_lwsp_raw         --> prettyCI      "c.lwsp"
                     , pad c_jr_res_raw       -->              "c.jr.res"
                     , pad c_jr_raw           --> prettyCR_1op "c.jr"
                     , pad c_mv_hint_raw      --> prettyCR_1op  "c.mv.hint"
                     , pad c_mv_raw           --> prettyCR      "c.mv"
                     , pad c_ebreak_raw       --> "c.ebreak"
                     , pad c_jalr_raw         --> prettyCR_1op "c.jalr"
                     , pad c_add_hint_raw     --> prettyCR_1op  "c.add.hint"
                     , pad c_add_raw          --> prettyCR      "c.add"
                     , pad c_swsp_raw         --> prettyCSS "c.swsp" ]

-- | List of normal RISC-V compressed instructions
rv_c :: Integer -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer
     -> Integer -> Integer -> Integer -> Integer
     -> [Instruction]
rv_c imm uimm nzimm nzuimm
     rs1' rs1'_rd' rs1_nz rs1_rd_nz
     rs2 rs2' rs2_nz
     rd rd' rd_nz rd_nz_n2 = [ c_illegal
                             , c_addi4spn     rd'               nzuimm
                             , c_fld          rd'   rs1'        uimm
                             , c_lq           rd'   rs1'        uimm
                             , c_lw           rd'   rs1'        uimm
                             , c_flw          rd'   rs1'        uimm
                             , c_ld           rd'   rs1'        uimm
                             , c_fsd                rs1'  rs2'  uimm
                             , c_sw                 rs1'  rs2'  uimm
                             , c_fsw                rs1'  rs2'  uimm
                             , c_sd                 rs1'  rs2'  uimm

                             , c_nop
                             , c_addi         rs1_rd_nz         nzimm
                             , c_jal                            imm
                             , c_addiw        rs1_rd_nz         imm
                             , c_li           rd_nz             imm
                             , c_addi16sp                       nzimm
                             , c_lui          rd_nz_n2          nzimm
                             , c_srli64       rs1'_rd'
                             , c_srli         rs1'_rd'          nzuimm
                             , c_srai64       rs1'_rd'
                             , c_srai         rs1'_rd'          nzuimm
                             , c_andi         rs1'_rd'          imm
                             , c_sub          rs1'_rd'    rs2'
                             , c_xor          rs1'_rd'    rs2'
                             , c_or           rs1'_rd'    rs2'
                             , c_and          rs1'_rd'    rs2'
                             , c_subw         rs1'_rd'    rs2'
                             , c_addw         rs1'_rd'    rs2'
                             , c_j                              imm
                             , c_beqz               rs1'        imm
                             , c_bnez               rs1'        imm

                             , c_slli64       rs1_rd_nz
                             , c_slli         rs1_rd_nz         nzuimm
                             , c_fldsp        rd                uimm
                             , c_lqsp         rd_nz             uimm
                             , c_lwsp         rd_nz             uimm
                             , c_flwsp        rd                uimm
                             , c_ldsp         rd_nz             uimm
                             , c_jr                 rs1_nz
                             , c_mv           rd_nz       rs2_nz
                             , c_ebreak
                             , c_jalr               rs1_nz
                             , c_add          rs1_rd_nz   rs2_nz
                             , c_fsdsp                    rs2   uimm
                             , c_sqsp                     rs2   uimm
                             , c_swsp                     rs2   uimm
                             , c_fswsp                    rs2   uimm
                             , c_sdsp                     rs2   uimm ]

-- | Exaustive list of RISC-V compressed instructions.
-- | Includes the normal list along with HINT, REServed, and NSE encodings.
rv_c_exh :: Integer -> Integer -> Integer -> Integer
         -> Integer -> Integer -> Integer -> Integer
         -> Integer -> Integer -> Integer
         -> Integer -> Integer -> Integer -> Integer
         -> [Instruction]
rv_c_exh imm uimm nzimm nzuimm
         rs1' rs1'_rd' rs1_nz rs1_rd_nz
         rs2 rs2' rs2_nz
         rd rd' rd_nz rd_nz_n2 = [ c_addi4spn_res rd'
                                 , c_res_a                          uimm

                                 , c_nop_hint                       nzimm
                                 , c_addi_hint    rs1_rd_nz
                                 , c_addiw_res                      imm   -- Reserved only for RV64C/RV128C (otherwise is c_jal)
                                 , c_li_hint                        imm
                                 , c_addi16sp_res
                                 , c_lui_hint                       nzimm
                                 , c_lui_res      rd_nz_n2
                                 , c_srli64_hint  rs1'_rd'                 -- HINT only for RV32C/RV64C
                                 , c_srli_nse     rs1'_rd'          nzuimm -- NSE only for RV32C
                                 , c_srai64_hint  rs1'_rd'                 -- HINT only for RV32C/RV64C
                                 , c_srai_nse     rs1'_rd'          nzuimm -- NSE only for RV32C
                                 , c_subw_res     rs1'_rd'    rs2'         -- Reserved only for RV32C
                                 , c_addw_res     rs1'_rd'    rs2'         -- Reserved only for RV32C
                                 , c_res_b                          uimm
                                 , c_res_c                          uimm

                                 , c_slli64_hint0
                                 , c_slli64_hintx rs1_rd_nz                -- HINT only for RV32C/RV64C
                                 , c_slli_hint                      nzuimm
                                 , c_slli_nse     rs1_rd_nz         nzuimm -- NSE only for RV32C
                                 , c_lqsp_res                       uimm   -- Reserved only for RV128C and RV32C/RV64C w/o F (otherwise is c_fldsp)
                                 , c_lwsp_res                       uimm
                                 , c_ldsp_res                       uimm   -- Reserved only for RV64C/RV128C and RV32C w/o F (otherwise is c_flwsp)
                                 , c_jr_res
                                 , c_mv_hint                  rs2_nz
                                 , c_add_hint                 rs2_nz ]

                                 ++ rv_c imm uimm nzimm nzuimm
                                    rs1' rs1'_rd' rs1_nz rs1_rd_nz
                                    rs2 rs2' rs2_nz
                                    rd rd' rd_nz rd_nz_n2
