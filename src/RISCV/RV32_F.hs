--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Alexandre Joannou
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
    Module      : RISCV.RV32_F
    Description : RISC-V RV32 floating-point extension

    The 'RISCV.RV32_F' module provides the description of the RISC-V RV32
    floating-point extension
-}

module RISCV.RV32_F (
-- * RV32 floating-point, instruction definitions
  flw
, fsw
, fmadd_s
, fmsub_s
, fnmsub_s
, fnmadd_s
, fadd_s
, fsub_s
, fmul_s
, fdiv_s
, fsqrt_s
, fsgnj_s
, fsgnjn_s
, fsgnjx_s
, fmin_s
, fmax_s
, fcvt_w_s
, fcvt_wu_s
, fmv_x_w
, feq_s
, flt_s
, fle_s
, fclass_s
, fcvt_s_w
, fcvt_s_wu
, fmv_w_x
-- * RV32 floating-point, others
, rv32_f_disass
, rv32_f_arith
, rv32_f_macc
, rv32_f_load
, rv32_f_store
, rv32_f
) where

import RISCV.Helpers (prettyR, prettyS, prettyR4_rm, prettyR_rm,
                      prettyR_FI_1op, prettyR_FF_1op, prettyR_IF_1op,
                      prettyR_FF_1op_rm, prettyR_FI_1op_rm, prettyR_IF_1op_rm,
                      prettyS_F)
import InstrCodec (DecodeBranch, (-->), encode)

flw_raw                    =                      "imm[11:0]            rs1[4:0]     010  rd[4:0] 0000111"
flw rd rs1 imm             = encode flw_raw        imm                  rs1               rd
fsw_raw                    =                      "imm[11:5]   rs2[4:0] rs1[4:0]     010 imm[4:0] 0100111"
fsw rs1 rs2 imm            = encode fsw_raw        imm         rs2      rs1
fmadd_s_raw                =                      "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000011"
fmadd_s rd rs1 rs2 rs3 rm  = encode fmadd_s_raw    rs3         rs2      rs1      rm       rd
fmsub_s_raw                =                      "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000111"
fmsub_s rd rs1 rs2 rs3 rm  = encode fmsub_s_raw    rs3         rs2      rs1      rm       rd
fnmsub_s_raw               =                      "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001011"
fnmsub_s rd rs1 rs2 rs3 rm = encode fnmsub_s_raw   rs3         rs2      rs1      rm       rd
fnmadd_s_raw               =                      "rs3[4:0] 00 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001111"
fnmadd_s rd rs1 rs2 rs3 rm = encode fnmadd_s_raw   rs3         rs2      rs1      rm       rd
fadd_s_raw                 =                      "0000000     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fadd_s rs2 rd rs1 rm       = encode fadd_s_raw                 rs2      rs1      rm       rd
fsub_s_raw                 =                      "0000100     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsub_s rs2 rd rs1 rm       = encode fsub_s_raw                 rs2      rs1      rm       rd
fmul_s_raw                 =                      "0001000     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fmul_s rs2 rd rs1 rm       = encode fmul_s_raw                 rs2      rs1      rm       rd
fdiv_s_raw                 =                      "0001100     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fdiv_s rs2 rd rs1 rm       = encode fdiv_s_raw                 rs2      rs1      rm       rd
fsqrt_s_raw                =                      "0101100        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsqrt_s rs1 rd rm          = encode fsqrt_s_raw                         rs1      rm       rd
fsgnj_s_raw                =                      "0010000     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fsgnj_s rs2 rd rs1         = encode fsgnj_s_raw                rs2      rs1               rd
fsgnjn_s_raw               =                      "0010000     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fsgnjn_s rs2 rd rs1        = encode fsgnjn_s_raw               rs2      rs1               rd
fsgnjx_s_raw               =                      "0010000     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
fsgnjx_s rs2 rd rs1        = encode fsgnjx_s_raw               rs2      rs1               rd
fmin_s_raw                 =                      "0010100     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fmin_s rs2 rd rs1          = encode fmin_s_raw                 rs2      rs1               rd
fmax_s_raw                 =                      "0010100     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fmax_s rs2 rd rs1          = encode fmax_s_raw                 rs2      rs1               rd
fcvt_w_s_raw               =                      "1100000        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_w_s rd rs1 rm         = encode fcvt_w_s_raw                        rs1      rm       rd
fcvt_wu_s_raw              =                      "1100000        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_wu_s rd rs1 rm        = encode fcvt_wu_s_raw                       rs1      rm       rd
fmv_x_w_raw                =                      "1110000        00000 rs1[4:0]     000  rd[4:0] 1010011"
fmv_x_w rd rs1             = encode fmv_x_w_raw                         rs1               rd
feq_s_raw                  =                      "1010000     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
feq_s rd rs1 rs2           = encode feq_s_raw                  rs2      rs1               rd
flt_s_raw                  =                      "1010000     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
flt_s rd rs1 rs2           = encode flt_s_raw                  rs2      rs1               rd
fle_s_raw                  =                      "1010000     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fle_s rd rs1 rs2           = encode fle_s_raw                  rs2      rs1               rd
fclass_s_raw               =                      "1110000        00000 rs1[4:0]     001  rd[4:0] 1010011"
fclass_s rd rs1            = encode fclass_s_raw                        rs1               rd
fcvt_s_w_raw               =                      "1101000        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_s_w rd rs1 rm         = encode fcvt_s_w_raw                        rs1      rm       rd
fcvt_s_wu_raw              =                      "1101000        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_s_wu rd rs1 rm        = encode fcvt_s_wu_raw                       rs1      rm       rd
fmv_w_x_raw                =                      "1111000        00000 rs1[4:0]     000  rd[4:0] 1010011"
fmv_w_x rd rs1             = encode fmv_w_x_raw                         rs1               rd


-- | Dissassembly of RV32 floating-point instructions
rv32_f_disass :: [DecodeBranch String]
rv32_f_disass = [ flw_raw       --> prettyR           "flw"
                , fsw_raw       --> prettyS_F         "fsw"
                , fmadd_s_raw   --> prettyR4_rm       "fmadd.s"
                , fmsub_s_raw   --> prettyR4_rm       "fmsub.s"
                , fnmsub_s_raw  --> prettyR4_rm       "fnmsub.s"
                , fnmadd_s_raw  --> prettyR4_rm       "fnmadd.s"
                , fadd_s_raw    --> prettyR_rm        "fadd.s"
                , fsub_s_raw    --> prettyR_rm        "fsub.s"
                , fmul_s_raw    --> prettyR_rm        "fmul.s"
                , fdiv_s_raw    --> prettyR_rm        "fdiv.s"
                , fsqrt_s_raw   --> prettyR_FF_1op_rm "fsqrt.s"
                , fsgnj_s_raw   --> prettyR           "fsgnj.s"
                , fsgnjn_s_raw  --> prettyR           "fsgnjn.s"
                , fsgnjx_s_raw  --> prettyR           "fsgnjx.s"
                , fmin_s_raw    --> prettyR           "fmin.s"
                , fmax_s_raw    --> prettyR           "fmax.s"
                , fcvt_w_s_raw  --> prettyR_IF_1op_rm "fcvt.w.s"
                , fcvt_wu_s_raw --> prettyR_FI_1op_rm "fcvt.wu.s"
                , fmv_x_w_raw   --> prettyR_IF_1op    "fmv.x.w"
                , feq_s_raw     --> prettyR           "feq.s"
                , flt_s_raw     --> prettyR           "flt.s"
                , fle_s_raw     --> prettyR           "fle.s"
                , fclass_s_raw  --> prettyR_IF_1op    "fclass.s"
                , fcvt_s_w_raw  --> prettyR_FI_1op_rm "fcvt.s.w"
                , fcvt_s_wu_raw --> prettyR_FI_1op_rm "fcvt.s.wu"
                , fmv_w_x_raw   --> prettyR_FI_1op    "fmv.w.x"
                ]

-- | List of RV32 floating-point load instructions
rv32_f_load :: Integer -> Integer -> Integer -> [Integer]
rv32_f_load src1 dest imm = [ flw dest src1 imm ]

-- | List of RV32 floating-point store instructions
rv32_f_store :: Integer -> Integer -> Integer -> [Integer]
rv32_f_store src1 src2 imm = [ fsw src1 src2 imm ]

-- | List of RV32 floating-point multiply-accumulate instructions
rv32_f_macc :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_f_macc src1 src2 src3 dest rm = [ fmadd_s  dest src1 src2 src3 rm
                                     , fmsub_s  dest src1 src2 src3 rm
                                     , fnmsub_s dest src1 src2 src3 rm
                                     , fnmadd_s dest src1 src2 src3 rm ]

-- | List of RV32 floating-point arithmetic instructions
rv32_f_arith :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_f_arith src1 src2 dest rm = [ fadd_s    dest src1 src2 rm
                                 , fsub_s    dest src1 src2 rm
                                 , fmul_s    dest src1 src2 rm
                                 , fdiv_s    dest src1 src2 rm
                                 , fsqrt_s   dest src1      rm
                                 , fsgnj_s   dest src1 src2
                                 , fsgnjn_s  dest src1 src2
                                 , fsgnjx_s  dest src1 src2
                                 , fmin_s    dest src1 src2
                                 , fmax_s    dest src1 src2
                                 , fcvt_w_s  dest src1      rm
                                 , fcvt_wu_s dest src1      rm
                                 , fmv_x_w   dest src1
                                 , feq_s     dest src1 src2
                                 , flt_s     dest src1 src2
                                 , fle_s     dest src1 src2
                                 , fclass_s  dest src1
                                 , fcvt_s_w  dest src1      rm
                                 , fcvt_s_wu dest src1      rm
                                 , fmv_w_x   dest src1         ]

-- | List of RV32 floating-point arithmetic instructions
rv32_f :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
       -> [Integer]
rv32_f src1 src2 src3 dest rm imm =    (rv32_f_arith src1 src2 dest rm)
                                    ++ (rv32_f_macc src1 src2 src3 dest rm)
                                    ++ (rv32_f_load src1 dest imm)
                                    ++ (rv32_f_store src1 src2 imm)
