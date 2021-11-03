--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019-2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
-- All rights reserved.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory (Department of Computer Science and
-- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
-- DARPA SSITH research programme.
--
-- This software was partly developed by the University of Cambridge
-- Computer Laboratory as part of the Partially-Ordered Event-Triggered
-- Systems (POETS) project, funded by EPSRC grant EP/N031768/1.
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
    Module      : RISCV.RV64_I
    Description : RISC-V RV64 base integer instruction set

    The 'RISCV.RV64_I' module provides the description of the RISC-V RV64
    base integer instruction set
-}

module RISCV.RV64_I (
-- * RV64 base integer instruction set, instruction definitions
  lwu
, ld
, sd
, addiw
, slli64
, srli64
, srai64
, slliw
, srliw
, sraiw
, addw
, subw
, sllw
, srlw
, sraw
-- * RV64 base integer instruction set, others
, rv64_i_disass
, rv64_i
, rv64_i_arith
, rv64_i_load
, rv64_i_store
, rv64_i_mem
) where

import RISCV.Helpers (prettyI, prettyI_sig, prettyR, prettyL, prettyS)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

lwu_raw            =                   "imm[11:0] rs1[4:0] 110 rd[4:0] 0000011"
lwu rd rs1 imm     = encode lwu_raw     imm       rs1          rd
ld_raw             =                   "imm[11:0] rs1[4:0] 011 rd[4:0] 0000011"
ld rd rs1 imm      = encode ld_raw      imm       rs1          rd
sd_raw             =                   "imm[11:5] rs2[4:0] rs1[4:0] 011 imm[4:0] 0100011"
sd rs1 rs2 imm     = encode sd_raw      imm       rs2      rs1          imm
addiw_raw          =                   "imm[11:0] rs1[4:0] 000 rd[4:0] 0011011"
addiw rd rs1 imm   = encode addiw_raw   imm       rs1          rd
slli64_raw         =                   "000000 imm[5:0] rs1[4:0] 001 rd[4:0] 0010011"
slli64 rd rs1 imm  = encode slli64_raw         imm      rs1          rd
srli64_raw         =                   "000000 imm[5:0] rs1[4:0] 101 rd[4:0] 0010011"
srli64 rd rs1 imm  = encode srli64_raw         imm      rs1          rd
srai64_raw         =                   "010000 imm[5:0] rs1[4:0] 101 rd[4:0] 0010011"
srai64 rd rs1 imm  = encode srai64_raw         imm      rs1          rd
slliw_raw          =                   "0000000 shamt[4:0] rs1[4:0] 001 rd[4:0] 0011011"
slliw rd rs1 shamt = encode slliw_raw           shamt      rs1          rd
srliw_raw          =                   "0000000 shamt[4:0] rs1[4:0] 101 rd[4:0] 0011011"
srliw rd rs1 shamt = encode srliw_raw           shamt      rs1          rd
sraiw_raw          =                   "0100000 shamt[4:0] rs1[4:0] 101 rd[4:0] 0011011"
sraiw rd rs1 shamt = encode sraiw_raw           shamt      rs1          rd
addw_raw           =                   "0000000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
addw rd rs1 rs2    = encode addw_raw            rs2      rs1          rd
subw_raw           =                   "0100000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0111011"
subw rd rs1 rs2    = encode subw_raw            rs2      rs1          rd
sllw_raw           =                   "0000000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0111011"
sllw rd rs1 rs2    = encode sllw_raw            rs2      rs1          rd
srlw_raw           =                   "0000000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"
srlw rd rs1 rs2    = encode srlw_raw            rs2      rs1          rd
sraw_raw           =                   "0100000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0111011"
sraw rd rs1 rs2    = encode sraw_raw            rs2      rs1          rd

-- | Dissassembly of RV64 base integer instructions
rv64_i_disass :: [DecodeBranch String]
rv64_i_disass = [ lwu_raw    --> prettyL "lwu"
                , ld_raw     --> prettyL "ld"
                , sd_raw     --> prettyS "sd"
                , addiw_raw  --> prettyI_sig "addiw"
                , slli64_raw --> prettyI "slli"
                , srli64_raw --> prettyI "srli"
                , srai64_raw --> prettyI "srai"
                , slliw_raw  --> prettyI "slliw"
                , srliw_raw  --> prettyI "srliw"
                , sraiw_raw  --> prettyI "sraiw"
                , addw_raw   --> prettyR "addw"
                , subw_raw   --> prettyR "subw"
                , sllw_raw   --> prettyR "sllw"
                , srlw_raw   --> prettyR "srlw"
                , sraw_raw   --> prettyR "sraw"
                ]

-- | List of RV64 base integer arithmetic instructions
rv64_i_arith :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv64_i_arith src1 src2 dest imm = [ addiw  dest src1      imm
                                  , slli64 dest src1      imm
                                  , srli64 dest src1      imm
                                  , srai64 dest src1      imm
                                  , slliw  dest src1      imm
                                  , srliw  dest src1      imm
                                  , sraiw  dest src1      imm
                                  , addw   dest src1 src2
                                  , subw   dest src1 src2
                                  , sllw   dest src1 src2
                                  , srlw   dest src1 src2
                                  , sraw   dest src1 src2
                                  ]

-- | List of RV64 base integer load instructions
rv64_i_load :: Integer -> Integer -> Integer -> [Instruction]
rv64_i_load src dest imm = [ lwu dest src imm
                           , ld  dest src imm
                           ]

-- | List of RV64 base integer store instructions
rv64_i_store :: Integer -> Integer -> Integer -> [Instruction]
rv64_i_store srcAddr srcData imm = [sd srcAddr srcData imm]

-- | List of RV64 base integer memory instructions
rv64_i_mem :: Integer -> Integer -> Integer -> Integer -> [Instruction] --TODO alignment
rv64_i_mem srcAddr srcData dest imm =
  (rv64_i_load srcAddr dest imm) ++ (rv64_i_store srcAddr srcData imm)

-- | List of RV64 base integer instructions
rv64_i :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv64_i srcAddr srcData dest imm =
     rv64_i_arith srcAddr srcData dest imm
  ++ rv64_i_mem srcAddr srcData dest imm
