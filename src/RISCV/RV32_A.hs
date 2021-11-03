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
    Module      : RISCV.RV32_A
    Description : RISC-V RV32 atomic extension

    The 'RISCV.RV32_A' module provides the description of the RISC-V RV32 Atomic
    extension
-}

module RISCV.RV32_A (
-- * RV32 atomic, instruction definitions
  lr_w
, sc_w
, amoswap_w
, amoadd_w
, amoxor_w
, amoand_w
, amoor_w
, amomin_w
, amomax_w
, amominu_w
, amomaxu_w
-- * RV32 atomic, others
, rv32_a
, rv32_a_disass
) where

import RISCV.Helpers (prettyR_A_1op, prettyR_A)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

lr_w_raw                   =                      "00010 aq[0] rl[0]    00000 rs1[4:0] 010 rd[4:0] 0101111"
lr_w rd rs1 aq rl          = encode lr_w_raw             aq    rl             rs1          rd
sc_w_raw                   =                      "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
sc_w rd rs1 rs2 aq rl      = encode sc_w_raw             aq    rl    rs2      rs1          rd
amoswap_w_raw              =                      "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoswap_w rd rs1 rs2 aq rl = encode amoswap_w_raw        aq    rl    rs2      rs1          rd
amoadd_w_raw               =                      "00000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoadd_w rd rs1 rs2 aq rl  = encode amoadd_w_raw         aq    rl    rs2      rs1          rd
amoxor_w_raw               =                      "00100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoxor_w rd rs1 rs2 aq rl  = encode amoxor_w_raw         aq    rl    rs2      rs1          rd
amoand_w_raw               =                      "01100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoand_w rd rs1 rs2 aq rl  = encode amoand_w_raw         aq    rl    rs2      rs1          rd
amoor_w_raw                =                      "01000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amoor_w rd rs1 rs2 aq rl   = encode amoor_w_raw          aq    rl    rs2      rs1          rd
amomin_w_raw               =                      "10000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amomin_w rd rs1 rs2 aq rl  = encode amomin_w_raw         aq    rl    rs2      rs1          rd
amomax_w_raw               =                      "10100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amomax_w rd rs1 rs2 aq rl  = encode amomax_w_raw         aq    rl    rs2      rs1          rd
amominu_w_raw              =                      "11000 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amominu_w rd rs1 rs2 aq rl = encode amominu_w_raw        aq    rl    rs2      rs1          rd
amomaxu_w_raw              =                      "11100 aq[0] rl[0] rs2[4:0] rs1[4:0] 010 rd[4:0] 0101111"
amomaxu_w rd rs1 rs2 aq rl = encode amomaxu_w_raw        aq    rl    rs2      rs1          rd

-- | Dissassembly of RV32 atomic instructions
rv32_a_disass :: [DecodeBranch String]
rv32_a_disass = [ lr_w_raw      --> prettyR_A_1op "lr.w"
                , sc_w_raw      --> prettyR_A     "sc.w"
                , amoswap_w_raw --> prettyR_A     "amoswap.w"
                , amoadd_w_raw  --> prettyR_A     "amoadd.w"
                , amoxor_w_raw  --> prettyR_A     "amoxor.w"
                , amoand_w_raw  --> prettyR_A     "amoand.w"
                , amoor_w_raw   --> prettyR_A     "amoor.w"
                , amomin_w_raw  --> prettyR_A     "amomin.w"
                , amomax_w_raw  --> prettyR_A     "amomax.w"
                , amominu_w_raw --> prettyR_A     "amominu.w"
                , amomaxu_w_raw --> prettyR_A     "amomaxu.w" ]

-- | List of RV32 atomic instructions
rv32_a :: Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_a src1 src2 dest aq rl = [ lr_w      dest src1      aq rl
                              , sc_w      dest src1 src2 aq rl
                              , amoswap_w dest src1 src2 aq rl
                              , amoadd_w  dest src1 src2 aq rl
                              , amoxor_w  dest src1 src2 aq rl
                              , amoand_w  dest src1 src2 aq rl
                              , amoor_w   dest src1 src2 aq rl
                              , amomin_w  dest src1 src2 aq rl
                              , amomax_w  dest src1 src2 aq rl
                              , amominu_w dest src1 src2 aq rl
                              , amomaxu_w dest src1 src2 aq rl ]
