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
    Module      : RISCV.RV32_M
    Description : RISC-V RV32 multiply/divide extension

    The 'RISCV.RV32_M' module provides the description of the RISC-V RV32
    multiply/divide extension
-}

module RISCV.RV32_M (
-- * RV32 multiply/divide, instruction definitions
  mul
, mulh
, mulhsu
, mulhu
, div
, divu
, rem
, remu
-- * RV32 multiply/divide, others
, rv32_m_disass
, rv32_m
) where

import Prelude hiding (rem, div)

import RISCV.Helpers (prettyR)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

mul_raw           =                   "0000001 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
mul rd rs1 rs2    = encode mul_raw             rs2      rs1          rd
mulh_raw          =                   "0000001 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
mulh rd rs1 rs2   = encode mulh_raw            rs2      rs1          rd
mulhsu_raw        =                   "0000001 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011"
mulhsu rd rs1 rs2 = encode mulhsu_raw          rs2      rs1          rd
mulhu_raw         =                   "0000001 rs2[4:0] rs1[4:0] 011 rd[4:0] 0110011"
mulhu rd rs1 rs2  = encode mulhu_raw           rs2      rs1          rd
div_raw           =                   "0000001 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
div rd rs1 rs2    = encode div_raw             rs2      rs1          rd
divu_raw          =                   "0000001 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
divu rd rs1 rs2   = encode divu_raw            rs2      rs1          rd
rem_raw           =                   "0000001 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
rem rd rs1 rs2    = encode rem_raw             rs2      rs1          rd
remu_raw          =                   "0000001 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
remu rd rs1 rs2   = encode remu_raw            rs2      rs1          rd

-- | Dissassembly of RV32 multiply/divide instructions
rv32_m_disass :: [DecodeBranch String]
rv32_m_disass = [ mul_raw    --> prettyR "mul"
                , mulh_raw   --> prettyR "mulh"
                , mulhsu_raw --> prettyR "mulhsu"
                , mulhu_raw  --> prettyR "mulhu"
                , div_raw    --> prettyR "div"
                , divu_raw   --> prettyR "divu"
                , rem_raw    --> prettyR "rem"
                , remu_raw   --> prettyR "remu" ]

-- | List of RV32 multiply/divide instructions
rv32_m :: Integer -> Integer -> Integer -> [Instruction]
rv32_m src1 src2 dest = [ mul    dest src1 src2
                        , mulh   dest src1 src2
                        , mulhsu dest src1 src2
                        , mulhu  dest src1 src2
                        , div    dest src1 src2
                        , divu   dest src1 src2
                        , rem    dest src1 src2
                        , remu   dest src1 src2 ]
