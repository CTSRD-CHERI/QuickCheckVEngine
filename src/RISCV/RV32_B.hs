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
    Module      : RISCV.RV32_B
    Description : RISC-V RV32 bit manipulation extensions

    The 'RISCV.RV32_B' module provides the description of the RISC-V RV32
    Bit-Manipulation extension
-}

module RISCV.RV32_B (
  -- * RV32 bitmanip, instruction definitions
  sh1add
, sh2add
, sh3add
, andn
, orn
, xnor
, clz
, ctz
, cpop
, max
, maxu
, min
, minu
, sext_b
, sext_h
, zext_h
, rol
, ror
, rori
, orc_b
, rev8
, clmul
, clmulh
, clmulr
, bclr
, bclri
, bext
, bexti
, binv
, binvi
, bset
, bseti
, -- * RV32 bitmanip, others
  rv32_b_disass
) where

import RISCV.Helpers (prettyR, prettyI, prettyR_nors2)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)
import Prelude hiding (min, max)

-- Zba
sh1add_raw             =                   "0010000 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011"
sh1add rd rs1 rs2      = encode sh1add_raw          rs2      rs1          rd
sh2add_raw             =                   "0010000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
sh2add rd rs1 rs2      = encode sh2add_raw          rs2      rs1          rd
sh3add_raw             =                   "0010000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
sh3add rd rs1 rs2      = encode sh3add_raw          rs2      rs1          rd

-- Zbb
andn_raw               =                   "0100000 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
andn rd rs1 rs2        = encode andn_raw            rs2      rs1          rd
orn_raw                =                   "0100000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
orn rd rs1 rs2         = encode orn_raw             rs2      rs1          rd
xnor_raw               =                   "0100000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
xnor rd rs1 rs2        = encode xnor_raw            rs2      rs1          rd

clz_raw                =                   "0110000 00000 rs1[4:0] 001 rd[4:0] 0010011"
clz rd rs1             = encode clz_raw                   rs1          rd
ctz_raw                =                   "0110000 00001 rs1[4:0] 001 rd[4:0] 0010011"
ctz rd rs1             = encode ctz_raw                   rs1          rd

cpop_raw               =                   "0110000 00010 rs1[4:0] 001 rd[4:0] 0010011"
cpop rd rs1            = encode cpop_raw                  rs1          rd

max_raw                =                   "0000101 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
max rd rs1 rs2         = encode max_raw             rs2      rs1          rd
maxu_raw               =                   "0000101 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
maxu rd rs1 rs2        = encode maxu_raw            rs2      rs1          rd

min_raw                =                   "0000101 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
min rd rs1 rs2         = encode min_raw             rs2      rs1          rd
minu_raw               =                   "0000101 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
minu rd rs1 rs2        = encode minu_raw            rs2      rs1          rd

sext_b_raw             =                   "0110000 00100 rs1[4:0] 001 rd[4:0] 0010011"
sext_b rd rs1          = encode sext_b_raw                rs1          rd
sext_h_raw             =                   "0110000 00101 rs1[4:0] 001 rd[4:0] 0010011"
sext_h rd rs1          = encode sext_h_raw                rs1          rd
zext_h_raw             =                   "0000100 00000 rs1[4:0] 100 rd[4:0] 0110011"
zext_h rd rs1          = encode zext_h_raw                rs1          rd

-- Bitwise rotation

rol_raw                =                   "0110000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
rol rd rs1 rs2         = encode rol_raw             rs2      rs1          rd
ror_raw                =                   "0110000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
ror rd rs1 rs2         = encode ror_raw             rs2      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
rori_raw               =                   "0110000 shamt[4:0] rs1[4:0] 101 rd[4:0] 0010011"
rori rd rs1 shamt      = encode rori_raw            shamt      rs1          rd

orc_b_raw              =                   "0010100 00111 rs1[4:0] 101 rd[4:0] 0010011"
orc_b rd rs1           = encode orc_b_raw                 rs1          rd
rev8_raw               =                   "0110100 11000 rs1[4:0] 101 rd[4:0] 0010011"
rev8 rd rs1            = encode rev8_raw                  rs1          rd

-- Zbc

clmul_raw              =                   "0000101 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
clmul rd rs1 rs2       = encode clmul_raw           rs2      rs1          rd
clmulh_raw             =                   "0000101 rs2[4:0] rs1[4:0] 011 rd[4:0] 0110011"
clmulh rd rs1 rs2      = encode clmulh_raw          rs2      rs1          rd
clmulr_raw             =                   "0000101 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011"
clmulr rd rs1 rs2      = encode clmulr_raw          rs2      rs1          rd

-- Zbs

bclr_raw               =                   "0100100 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
bclr rd rs1 rs2        = encode bclr_raw            rs2      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
bclri_raw              =                   "0100100 shamt[4:0] rs1[4:0] 001 rd[4:0] 0010011"
bclri rd rs1 shamt     = encode bclri_raw           shamt      rs1          rd

bext_raw               =                   "0100100 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
bext rd rs1 rs2        = encode bext_raw            rs2      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
bexti_raw              =                   "0100100 shamt[4:0] rs1[4:0] 101 rd[4:0] 0010011"
bexti rd rs1 shamt     = encode bexti_raw           shamt      rs1          rd

binv_raw               =                   "0110100 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
binv rd rs1 rs2        = encode binv_raw            rs2      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
binvi_raw              =                   "0110100 shamt[4:0] rs1[4:0] 001 rd[4:0] 0010011"
binvi rd rs1 shamt     = encode binvi_raw           shamt      rs1          rd

bset_raw               =                   "0010100 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
bset rd rs1 rs2        = encode bset_raw            rs2      rs1          rd

-- The RV64 and RV32 encodings differ by the width of the shamt field, with
-- RV32 specifying that shamt[5] is 0. Import qualified if needed.
bseti_raw              =                   "0010100 shamt[4:0] rs1[4:0] 001 rd[4:0] 0010011"
bseti rd rs1 shamt     = encode bseti_raw           shamt      rs1          rd

rv32_b_disass :: [DecodeBranch String]
rv32_b_disass = [ sh1add_raw --> prettyR "sh1add"
                , sh2add_raw --> prettyR "sh2add"
                , sh3add_raw --> prettyR "sh3add"
                , andn_raw   --> prettyR "andn"
                , orn_raw    --> prettyR "orn"
                , xnor_raw   --> prettyR "xnor"
                , clz_raw    --> prettyR_nors2 "clz"
                , ctz_raw    --> prettyR_nors2 "ctz"
                , cpop_raw   --> prettyR_nors2 "cpop"
                , max_raw    --> prettyR "max"
                , maxu_raw   --> prettyR "maxu"
                , min_raw    --> prettyR "min"
                , minu_raw   --> prettyR "minu"
                , sext_b_raw --> prettyR_nors2 "sext.b"
                , sext_h_raw --> prettyR_nors2 "sext.h"
                , zext_h_raw --> prettyR_nors2 "zext.h"
                , rol_raw    --> prettyR "rol"
                , ror_raw    --> prettyR "ror"
                , rori_raw   --> prettyI "rori"
                , orc_b_raw  --> prettyR_nors2 "orc.b"
                , rev8_raw   --> prettyR_nors2 "rev8.b"
                , clmul_raw  --> prettyR "clmul"
                , clmulh_raw --> prettyR "clmulh"
                , clmulr_raw --> prettyR "clmulr"
                , bclr_raw   --> prettyR "bclr"
                , bclri_raw  --> prettyI "bclri"
                , bext_raw   --> prettyR "bext"
                , bexti_raw  --> prettyI "bexti"
                , binv_raw   --> prettyR "binv"
                , binvi_raw  --> prettyI "binvi"
                , bset_raw   --> prettyR "bset"
                , bseti_raw  --> prettyI "bseti"
                ]
