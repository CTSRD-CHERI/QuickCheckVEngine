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
    Module      : RISCV.RV32_D
    Description : RISC-V RV32 double-precision floating-point extension

    The 'RISCV.RV32_D' module provides the description of the RISC-V RV32
    double-precision floating-point extension
-}

module RISCV.RV32_D (
-- * RV32 double-precision floating-point, instruction definitions
  fld
, fsd
, fmadd_d
, fmsub_d
, fnmsub_d
, fnmadd_d
, fadd_d
, fsub_d
, fmul_d
, fdiv_d
, fsqrt_d
, fsgnj_d
, fsgnjn_d
, fsgnjx_d
, fmin_d
, fmax_d
, fcvt_s_d
, fcvt_d_s
, feq_d
, flt_d
, fle_d
, fclass_d
, fcvt_w_d
, fcvt_wu_d
, fcvt_d_w
, fcvt_d_wu
-- * RV32 double-precision floating-point, others
, rv32_d_disass
, rv32_d_arith
, rv32_d_macc
, rv32_d_load
, rv32_d_store
, rv32_d
) where

import RISCV.Helpers (prettyR, prettyS, prettyR4_rm, prettyR_rm,
                      prettyR_IF_1op, prettyR_FF_1op_rm, prettyR_FI_1op_rm,
                      prettyR_IF_1op_rm, prettyS_F)
import InstrCodec (DecodeBranch, (-->), encode)

fld_raw                    =                      "imm[11:0]            rs1[4:0]     011  rd[4:0] 0000111"
fld rd rs1 imm             = encode fld_raw        imm                  rs1               rd
fsd_raw                    =                      "imm[11:5]   rs2[4:0] rs1[4:0]     011 imm[4:0] 0100111"
fsd rs1 rs2 imm            = encode fsd_raw        imm         rs2      rs1
fmadd_d_raw                =                      "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000011"
fmadd_d rd rs1 rs2 rs3 rm  = encode fmadd_d_raw    rs3         rs2      rs1      rm       rd
fmsub_d_raw                =                      "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1000111"
fmsub_d rd rs1 rs2 rs3 rm  = encode fmsub_d_raw    rs3         rs2      rs1      rm       rd
fnmsub_d_raw               =                      "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001011"
fnmsub_d rd rs1 rs2 rs3 rm = encode fnmsub_d_raw   rs3         rs2      rs1      rm       rd
fnmadd_d_raw               =                      "rs3[4:0] 01 rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1001111"
fnmadd_d rd rs1 rs2 rs3 rm = encode fnmadd_d_raw   rs3         rs2      rs1      rm       rd
fadd_d_raw                 =                      "0000001     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fadd_d rd rs1 rs2 rm       = encode fadd_d_raw                 rs2      rs1      rm       rd
fsub_d_raw                 =                      "0000101     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsub_d rd rs1 rs2 rm       = encode fsub_d_raw                 rs2      rs1      rm       rd
fmul_d_raw                 =                      "0001001     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fmul_d rd rs1 rs2 rm       = encode fmul_d_raw                 rs2      rs1      rm       rd
fdiv_d_raw                 =                      "0001101     rs2[4:0] rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fdiv_d rd rs1 rs2 rm       = encode fdiv_d_raw                 rs2      rs1      rm       rd
fsqrt_d_raw                =                      "0101101        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fsqrt_d rd rs1 rm          = encode fsqrt_d_raw                         rs1      rm       rd
fsgnj_d_raw                =                      "0010001     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fsgnj_d rd rs1 rs2         = encode fsgnj_d_raw                rs2      rs1               rd
fsgnjn_d_raw               =                      "0010001     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fsgnjn_d rd rs1 rs2        = encode fsgnjn_d_raw               rs2      rs1               rd
fsgnjx_d_raw               =                      "0010001     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
fsgnjx_d rd rs1 rs2        = encode fsgnjx_d_raw               rs2      rs1               rd
fmin_d_raw                 =                      "0010101     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fmin_d rd rs1 rs2          = encode fmin_d_raw                 rs2      rs1               rd
fmax_d_raw                 =                      "0010101     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
fmax_d rd rs1 rs2          = encode fmax_d_raw                 rs2      rs1               rd
fcvt_s_d_raw               =                      "0100000        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_s_d rd rs1 rm         = encode fcvt_s_d_raw                        rs1      rm       rd
fcvt_d_s_raw               =                      "0100001        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_d_s rd rs1 rm         = encode fcvt_d_s_raw                        rs1      rm       rd
feq_d_raw                  =                      "1010001     rs2[4:0] rs1[4:0]     010  rd[4:0] 1010011"
feq_d rd rs1 rs2           = encode feq_d_raw                  rs2      rs1               rd
flt_d_raw                  =                      "1010001     rs2[4:0] rs1[4:0]     001  rd[4:0] 1010011"
flt_d rd rs1 rs2           = encode flt_d_raw                  rs2      rs1               rd
fle_d_raw                  =                      "1010001     rs2[4:0] rs1[4:0]     000  rd[4:0] 1010011"
fle_d rd rs1 rs2           = encode fle_d_raw                  rs2      rs1               rd
fclass_d_raw               =                      "1110001        00000 rs1[4:0]     001  rd[4:0] 1010011"
fclass_d rd rs1            = encode fclass_d_raw                        rs1               rd
fcvt_w_d_raw               =                      "1100001        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_w_d rd rs1 rm         = encode fcvt_w_d_raw                        rs1      rm       rd
fcvt_wu_d_raw              =                      "1100001        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_wu_d rd rs1 rm        = encode fcvt_wu_d_raw                       rs1      rm       rd
fcvt_d_w_raw               =                      "1101001        00000 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_d_w rd rs1 rm         = encode fcvt_d_w_raw                        rs1      rm       rd
fcvt_d_wu_raw              =                      "1101001        00001 rs1[4:0] rm[2:0]  rd[4:0] 1010011"
fcvt_d_wu rd rs1 rm        = encode fcvt_d_wu_raw                       rs1      rm       rd

-- | Dissassembly of RV32 double-precision floating-point instructions
rv32_d_disass :: [DecodeBranch String]
rv32_d_disass = [ fld_raw       --> prettyR           "fld"
                , fsd_raw       --> prettyS_F         "fsd"
                , fmadd_d_raw   --> prettyR4_rm       "fmadd.d"
                , fmsub_d_raw   --> prettyR4_rm       "fmsub.d"
                , fnmsub_d_raw  --> prettyR4_rm       "fnmsub.d"
                , fnmadd_d_raw  --> prettyR4_rm       "fnmadd.d"
                , fadd_d_raw    --> prettyR_rm        "fadd.d"
                , fsub_d_raw    --> prettyR_rm        "fsub.d"
                , fmul_d_raw    --> prettyR_rm        "fmul.d"
                , fdiv_d_raw    --> prettyR_rm        "fdiv.d"
                , fsqrt_d_raw   --> prettyR_FF_1op_rm "fsqrt.d"
                , fsgnj_d_raw   --> prettyR           "fsgnj.d"
                , fsgnjn_d_raw  --> prettyR           "fsgnjn.d"
                , fsgnjx_d_raw  --> prettyR           "fsgnjx.d"
                , fmin_d_raw    --> prettyR           "fmin.d"
                , fmax_d_raw    --> prettyR           "fmax.d"
                , fcvt_s_d_raw  --> prettyR_FF_1op_rm "fcvt.s.d"
                , fcvt_d_s_raw  --> prettyR_FF_1op_rm "fcvt.d.s"
                , feq_d_raw     --> prettyR           "feq.d"
                , flt_d_raw     --> prettyR           "flt.d"
                , fle_d_raw     --> prettyR           "fle.d"
                , fclass_d_raw  --> prettyR_IF_1op    "fclass.d"
                , fcvt_w_d_raw  --> prettyR_IF_1op_rm "fcvt.w.d"
                , fcvt_wu_d_raw --> prettyR_IF_1op_rm "fcvt.wu.d"
                , fcvt_d_w_raw  --> prettyR_FI_1op_rm "fcvt.d.w"
                , fcvt_d_wu_raw --> prettyR_FI_1op_rm "fcvt.d.wu" ]

-- | List of RV32 double-precision floating-point load instructions
rv32_d_load :: Integer -> Integer -> Integer -> [Integer]
rv32_d_load src1 dest imm = [ fld dest src1 imm ]

-- | List of RV32 double-precision floating-point store instructions
rv32_d_store :: Integer -> Integer -> Integer -> [Integer]
rv32_d_store src1 src2 imm = [ fsd src1 src2 imm ]

-- | List of RV32 double-precision floating-point multiply-accumulate
--   instructions
rv32_d_macc :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_d_macc src1 src2 src3 dest rm = [ fmadd_d  dest src1 src2 src3 rm
                                     , fmsub_d  dest src1 src2 src3 rm
                                     , fnmsub_d dest src1 src2 src3 rm
                                     , fnmadd_d dest src1 src2 src3 rm ]

-- | List of RV32 double-precision floating-point arithmetic instructions
rv32_d_arith :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_d_arith src1 src2 dest rm = [ fadd_d    dest src1 src2 rm
                                 , fsub_d    dest src1 src2 rm
                                 , fmul_d    dest src1 src2 rm
                                 , fdiv_d    dest src1 src2 rm
                                 , fsqrt_d   dest src1      rm
                                 , fsgnj_d   dest src1 src2
                                 , fsgnjn_d  dest src1 src2
                                 , fsgnjx_d  dest src1 src2
                                 , fmin_d    dest src1 src2
                                 , fmax_d    dest src1 src2
                                 , fcvt_s_d  dest src1      rm
                                 , fcvt_d_s  dest src1      rm
                                 , feq_d     dest src1 src2
                                 , flt_d     dest src1 src2
                                 , fle_d     dest src1 src2
                                 , fclass_d  dest src1
                                 , fcvt_w_d  dest src1      rm
                                 , fcvt_wu_d dest src1      rm
                                 , fcvt_d_w  dest src1      rm
                                 , fcvt_d_wu dest src1      rm ]

-- | List of RV32 double-precision floating-point instructions
rv32_d :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
       -> [Integer]
rv32_d src1 src2 src3 dest rm imm =    (rv32_d_arith src1 src2 dest rm)
                                    ++ (rv32_d_macc src1 src2 src3 dest rm)
                                    ++ (rv32_d_load src1 dest imm)
                                    ++ (rv32_d_store src1 src2 imm)
