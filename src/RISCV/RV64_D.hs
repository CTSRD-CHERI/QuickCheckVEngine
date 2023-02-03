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
    Module      : RISCV.RV64_D
    Description : RISC-V RV64 double-precision floating-point extension

    The 'RISCV.RV64_D' module provides the description of the RISC-V RV64
    double-precision floating-point extension
-}

module RISCV.RV64_D (
-- * RV64 double-precision floating-point, instruction definitions
  fcvt_l_d
, fcvt_lu_d
, fmv_x_d
, fcvt_d_l
, fcvt_d_lu
, fmv_d_x
-- * RV64 double-precision floating-point, others
, rv64_d_disass
, rv64_d
) where

import RISCV.Helpers (prettyR_FI_1op_rm, prettyR_IF_1op_rm, prettyR_FI_1op, prettyR_IF_1op)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

fcvt_l_d_raw :: String
fcvt_l_d_raw        =                      "1100001    00010 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_l_d :: Integer -> Integer -> Integer -> Instruction
fcvt_l_d rd rs1 rm  = encode fcvt_l_d_raw                    rs1      rm      rd
fcvt_lu_d_raw :: String
fcvt_lu_d_raw       =                      "1100001    00011 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_lu_d :: Integer -> Integer -> Integer -> Instruction
fcvt_lu_d rd rs1 rm = encode fcvt_lu_d_raw                   rs1      rm      rd
fmv_x_d_raw :: String
fmv_x_d_raw         =                      "1110001    00000 rs1[4:0]     000 rd[4:0] 1010011"
fmv_x_d :: Integer -> Integer -> Instruction
fmv_x_d rd rs1      = encode fmv_x_d_raw                     rs1              rd
fcvt_d_l_raw :: String
fcvt_d_l_raw        =                      "1101001    00010 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_d_l :: Integer -> Integer -> Integer -> Instruction
fcvt_d_l rd rs1 rm  = encode fcvt_d_l_raw                    rs1      rm      rd
fcvt_d_lu_raw :: String
fcvt_d_lu_raw       =                      "1101001    00011 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_d_lu :: Integer -> Integer -> Integer -> Instruction
fcvt_d_lu rd rs1 rm = encode fcvt_d_lu_raw                   rs1      rm      rd
fmv_d_x_raw :: String
fmv_d_x_raw         =                      "1111001    00000 rs1[4:0]     000 rd[4:0] 1010011"
fmv_d_x :: Integer -> Integer -> Instruction
fmv_d_x rd rs1      = encode fmv_d_x_raw                     rs1              rd

-- | Dissassembly of RV64 double-precision floating-point instructions
rv64_d_disass :: [DecodeBranch String]
rv64_d_disass = [ fcvt_l_d_raw  --> prettyR_FI_1op_rm "fcvt.l.d"
                , fcvt_lu_d_raw --> prettyR_IF_1op_rm "fcvt.lu.d"
                , fmv_x_d_raw   --> prettyR_IF_1op    "fmv.x.d"
                , fcvt_d_l_raw  --> prettyR_FI_1op_rm "fcvt.d.l"
                , fcvt_d_lu_raw --> prettyR_FI_1op_rm "fcvt.d.lu"
                , fmv_d_x_raw   --> prettyR_FI_1op    "fmv.d.x"
                ]

-- | List of RV64 double-precision floating-point instructions
rv64_d :: Integer -> Integer -> Integer -> [Instruction]
rv64_d src1 dest rm = [ fcvt_l_d  dest src1 rm
                      , fcvt_lu_d dest src1 rm
                      , fmv_x_d   dest src1
                      , fcvt_d_l  dest src1 rm
                      , fcvt_d_lu dest src1 rm
                      , fmv_d_x   dest src1
                      ]
