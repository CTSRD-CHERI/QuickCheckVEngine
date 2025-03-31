--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
-- Copyright (c) 2025 SCI Semiconductor
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
    Module      : RISCV.RV64_B
    Description : RISC-V RV64 bit manipulation extensions

    The 'RISCV.RV64_B' module provides the description of the RISC-V RV64
    Bit-Manipulation extension
-}

module RISCV.RV64_B (
-- * RV64 bitmanip, instruction definitions
  add_uw
, sh1add_uw
, sh2add_uw
, sh3add_uw
, slli_uw
, clzw
, ctzw
, cpopw
, zext_h_64
, rori
, rolw
, roriw
, rorw
, rev8_64
, bclri
, bexti
, binvi
, bseti
, -- * RV64 bitmanip, others
  rv64_b_disass
) where

import RISCV.Helpers (prettyR, prettyI, prettyR_nors2)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

-- Zba
add_uw_raw                =                      "0000100 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
add_uw rd rs1 rs2         = encode add_uw_raw             rs2      rs1          rd

sh1add_uw_raw             =                      "0010000 rs2[4:0] rs1[4:0] 010 rd[4:0] 0111011"
sh1add_uw rd rs1 rs2      = encode sh1add_uw_raw          rs2      rs1          rd
sh2add_uw_raw             =                      "0010000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0111011"
sh2add_uw rd rs1 rs2      = encode sh2add_uw_raw          rs2      rs1          rd
sh3add_uw_raw             =                      "0010000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0111011"
sh3add_uw rd rs1 rs2      = encode sh3add_uw_raw          rs2      rs1          rd

slli_uw_raw               =                      "000010 shamt[5:0] rs1[4:0] 001 rd[4:0] 0011011"
slli_uw   rd rs1 shamt    = encode slli_uw_raw           shamt      rs1          rd

-- Zbb

clzw_raw                  =                      "0110000 00000 rs1[4:0] 001 rd[4:0] 0011011"
clzw rd rs1               = encode clzw_raw                     rs1          rd
ctzw_raw                  =                      "0110000 00001 rs1[4:0] 001 rd[4:0] 0011011"
ctzw rd rs1               = encode ctzw_raw                     rs1          rd

cpopw_raw                 =                      "0110000 00010 rs1[4:0] 001 rd[4:0] 0011011"
cpopw rd rs1              = encode cpopw_raw                    rs1          rd

-- The spec says simply "The zext.h mnemonic corresponds to different
-- instruction encodings in RV32 and Rv64". Import qualified if needed.
zext_h_64_raw             =                      "0000100 00000 rs1[4:0] 100 rd[4:0] 0111011"
zext_h_64 rd rs1          = encode zext_h_64_raw                rs1          rd

-- Bitwise rotation

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
rori_raw                  =                      "011000 shamt[5:0] rs1[4:0] 101 rd[4:0] 0010011"
rori rd rs1 shamt         = encode rori_raw              shamt      rs1          rd
                        
rolw_raw                  =                      "0110000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0111011"
rolw rd rs1 rs2           = encode rolw_raw               rs2      rs1          rd
roriw_raw                 =                      "0110000 shamt[4:0] rs1[4:0] 101 rd[4:0] 0011011"
roriw rd rs1 shamt        = encode roriw_raw              shamt      rs1          rd
rorw_raw                  =                      "0110000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"
rorw rd rs1 rs2           = encode rorw_raw               rs2      rs1          rd

-- The spec says simply "The rev8 mnemonic corresponds to different
-- instruction encodings in RV32 and Rv64". Import qualified if needed.
rev8_64_raw               =                      "0110101 11000 rs1[4:0] 101 rd[4:0] 0010011"
rev8_64 rd rs1            = encode rev8_64_raw                  rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
bclri_raw                 =                      "010010 shamt[5:0] rs1[4:0] 001 rd[4:0] 0010011"
bclri rd rs1 shamt        = encode bclri_raw             shamt      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
bexti_raw                 =                      "010010 shamt[5:0] rs1[4:0] 101 rd[4:0] 0010011"
bexti rd rs1 shamt        = encode bexti_raw             shamt      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
binvi_raw                 =                      "011010 shamt[5:0] rs1[4:0] 001 rd[4:0] 0010011"
binvi rd rs1 shamt        = encode binvi_raw             shamt      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
bseti_raw                 =                      "0010100 shamt[4:0] rs1[4:0] 001 rd[4:0] 0010011"
bseti rd rs1 shamt        = encode bseti_raw              shamt      rs1          rd

rv64_b_disass :: [DecodeBranch String]
rv64_b_disass = [ sh1add_uw_raw --> prettyR "sh1add.uw"
                , sh2add_uw_raw --> prettyR "sh2add.uw"
                , sh3add_uw_raw --> prettyR "sh3add.uw"
                , slli_uw_raw   --> prettyI "slli.uw"
                , clzw_raw      --> prettyR_nors2 "clzw"
                , ctzw_raw      --> prettyR_nors2 "ctzw"
                , cpopw_raw     --> prettyR_nors2 "cpopw"
                , zext_h_64_raw --> prettyR_nors2 "zext.h.64"
                , rori_raw      --> prettyI "rori"
                , rolw_raw      --> prettyR "rolw"
                , roriw_raw     --> prettyI "roriw"
                , rorw_raw      --> prettyR "rorw"
                , rev8_64_raw   --> prettyR_nors2 "rev8.64"
                , bclri_raw     --> prettyI "bclri"
                , bexti_raw     --> prettyI "bexti"
                , binvi_raw     --> prettyI "binvi"
                , bseti_raw     --> prettyI "bseti"
                ]
