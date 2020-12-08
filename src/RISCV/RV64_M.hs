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
    Module      : RISCV.RV64_M
    Description : RISC-V RV64 multiply/divide extension

    The 'RISCV.RV64_M' module provides the description of the RISC-V RV64
    multiply/divide extension
-}

module RISCV.RV64_M (
-- * RV64 multiply/divide, instruction definitions
  mulw
, divw
, divuw
, remw
, remuw
-- * RV64 multiply/divide, others
, rv64_m_disass
, rv64_m
) where

import RISCV.Helpers (prettyR)
import InstrCodec (DecodeBranch, (-->), encode)

mulw_raw         =                  "0000001 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
mulw rd rs1 rs2  = encode mulw_raw           rs2      rs1          rd
divw_raw         =                  "0000001 rs2[4:0] rs1[4:0] 100 rd[4:0] 0111011"
divw rd rs1 rs2  = encode divw_raw           rs2      rs1          rd
divuw_raw        =                  "0000001 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"
divuw rd rs1 rs2 = encode divuw_raw          rs2      rs1          rd
remw_raw         =                  "0000001 rs2[4:0] rs1[4:0] 110 rd[4:0] 0111011"
remw rd rs1 rs2  = encode remw_raw           rs2      rs1          rd
remuw_raw        =                  "0000001 rs2[4:0] rs1[4:0] 111 rd[4:0] 0111011"
remuw rd rs1 rs2 = encode remuw_raw          rs2      rs1          rd

-- | Dissassembly of RV64 multiply/divide instructions
rv64_m_disass :: [DecodeBranch String]
rv64_m_disass = [ mulw_raw  --> prettyR "mulw"
                , divw_raw  --> prettyR "divw"
                , divuw_raw --> prettyR "divuw"
                , remw_raw  --> prettyR "remw"
                , remuw_raw --> prettyR "remuw"
                ]

-- | List of RV64 multiply/divide instructions
rv64_m :: Integer -> Integer -> Integer -> [Integer]
rv64_m src1 src2 dest = [ mulw  dest src1 src2
                        , divw  dest src1 src2
                        , divuw dest src1 src2
                        , remw  dest src1 src2
                        , remuw dest src1 src2
                        ]
