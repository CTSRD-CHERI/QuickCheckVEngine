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
    Module      : RISCV.RV64_A
    Description : RISC-V RV64 atomic extension

    The 'RISCV.RV64_A' module provides the description of the RISC-V RV64 Atomic
    extension
-}

module RISCV.RV64_A (
-- * RV64 atomic, instruction definitions
  lr_d
, sc_d
, amoswap_d
, amoadd_d
, amoxor_d
, amoand_d
, amoor_d
, amomin_d
, amomax_d
, amominu_d
, amomaxu_d
-- * RV64 atomic, others
, rv64_a_disass
, rv64_a
) where

import RISCV.Helpers (prettyR_A_1op, prettyR_A)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

lr_d_raw                   =                      "00010 aq[0] rl[0]    00000 rs1[4:0] 011 rd[4:0] 0101111"
lr_d rd rs1 aq rl          = encode lr_d_raw             aq    rl             rs1          rd
sc_d_raw                   =                      "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
sc_d rd rs1 rs2 aq rl      = encode sc_d_raw             aq    rl    rs2      rs1          rd
amoswap_d_raw              =                      "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoswap_d rd rs1 rs2 aq rl = encode amoswap_d_raw        aq    rl    rs2      rs1          rd
amoadd_d_raw               =                      "00000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoadd_d rd rs1 rs2 aq rl  = encode amoadd_d_raw         aq    rl    rs2      rs1          rd
amoxor_d_raw               =                      "00100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoxor_d rd rs1 rs2 aq rl  = encode amoxor_d_raw         aq    rl    rs2      rs1          rd
amoand_d_raw               =                      "01100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoand_d rd rs1 rs2 aq rl  = encode amoand_d_raw         aq    rl    rs2      rs1          rd
amoor_d_raw                =                      "01000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amoor_d rd rs1 rs2 aq rl   = encode amoor_d_raw          aq    rl    rs2      rs1          rd
amomin_d_raw               =                      "10000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amomin_d rd rs1 rs2 aq rl  = encode amomin_d_raw         aq    rl    rs2      rs1          rd
amomax_d_raw               =                      "10100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amomax_d rd rs1 rs2 aq rl  = encode amomax_d_raw         aq    rl    rs2      rs1          rd
amominu_d_raw              =                      "11000 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amominu_d rd rs1 rs2 aq rl = encode amominu_d_raw        aq    rl    rs2      rs1          rd
amomaxu_d_raw              =                      "11100 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 0101111"
amomaxu_d rd rs1 rs2 aq rl = encode amomaxu_d_raw        aq    rl    rs2      rs1          rd

-- | Dissassembly of RV64 atomic instructions
rv64_a_disass :: [DecodeBranch String]
rv64_a_disass = [ lr_d_raw      --> prettyR_A_1op "lr.d"
                , sc_d_raw      --> prettyR_A     "sc.d"
                , amoswap_d_raw --> prettyR_A     "amoswap.d"
                , amoadd_d_raw  --> prettyR_A     "amoadd.d"
                , amoxor_d_raw  --> prettyR_A     "amoxor.d"
                , amoand_d_raw  --> prettyR_A     "amoand.d"
                , amoor_d_raw   --> prettyR_A     "amoor.d"
                , amomin_d_raw  --> prettyR_A     "amomin.d"
                , amomax_d_raw  --> prettyR_A     "amomax.d"
                , amominu_d_raw --> prettyR_A     "amominu.d"
                , amomaxu_d_raw --> prettyR_A     "amomaxu.d" ]

-- | List of RV64 atomic instructions
rv64_a :: Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv64_a src1 src2 dest aq rl = [ lr_d      dest src1 src2 aq rl
                              , sc_d      dest src1 src2 aq rl
                              , amoswap_d dest src1 src2 aq rl
                              , amoadd_d  dest src1 src2 aq rl
                              , amoxor_d  dest src1 src2 aq rl
                              , amoand_d  dest src1 src2 aq rl
                              , amoor_d   dest src1 src2 aq rl
                              , amomin_d  dest src1 src2 aq rl
                              , amomax_d  dest src1 src2 aq rl
                              , amominu_d dest src1 src2 aq rl
                              , amomaxu_d dest src1 src2 aq rl ]
