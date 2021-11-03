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
    Module      : RISCV.RV64_F
    Description : RISC-V RV64 floating-point extension

    The 'RISCV.RV64_F' module provides the description of the RISC-V RV64
    floating-point extension
-}

module RISCV.RV64_F (
-- * RV64 floating-point, instruction definitions
  fcvt_l_s
, fcvt_lu_s
, fcvt_s_l
, fcvt_s_lu
-- * RV64 floating-point, others
, rv64_f_disass
, rv64_f
) where

import RISCV.Helpers (prettyR_IF_1op_rm, prettyR_FI_1op_rm)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

fcvt_l_s_raw                             =                       "1100000 00010 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_l_s rd rs1 rm                       = encode fcvt_l_s_raw                  rs1      rm      rd
fcvt_lu_s_raw                            =                       "1100000 00011 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_lu_s rd rs1 rm                      = encode fcvt_lu_s_raw                 rs1      rm      rd
fcvt_s_l_raw                             =                       "1101000 00010 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_s_l rd rs1 rm                       = encode fcvt_s_l_raw                  rs1      rm      rd
fcvt_s_lu_raw                            =                       "1101000 00011 rs1[4:0] rm[2:0] rd[4:0] 1010011"
fcvt_s_lu rd rs1 rm                      = encode fcvt_s_lu_raw                 rs1      rm      rd

-- | Dissassembly of RV64 floating-point instructions
rv64_f_disass :: [DecodeBranch String]
rv64_f_disass = [ fcvt_l_s_raw  --> prettyR_IF_1op_rm "fcvt.l.s"
                , fcvt_lu_s_raw --> prettyR_IF_1op_rm "fcvt.lu.s"
                , fcvt_s_l_raw  --> prettyR_FI_1op_rm "fcvt.s.l"
                , fcvt_s_lu_raw --> prettyR_FI_1op_rm "fcvt.s.lu"
                ]

-- | List of RV64 floating-point arithmetic instructions
rv64_f :: Integer -> Integer -> Integer -> [Instruction]
rv64_f src1 dest rm = [ fcvt_l_s  dest src1 rm
                      , fcvt_lu_s dest src1 rm
                      , fcvt_s_l  dest src1 rm
                      , fcvt_s_lu dest src1 rm
                      ]
