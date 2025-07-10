--
---- SPDX-License-Identifier: BSD-2-Clause
----
---- Copyright (c) 2025 Franz Fuchs
---- All rights reserved.
----
---- This software was developed by SRI International and the University of
---- Cambridge Computer Laboratory (Department of Computer Science and
---- Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
---- DARPA SSITH research programme.
----
---- This software was partly developed by the University of Cambridge
---- Computer Laboratory as part of the Partially-Ordered Event-Triggered
---- Systems (POETS) project, funded by EPSRC grant EP/N031768/1.
----
---- Redistribution and use in source and binary forms, with or without
---- modification, are permitted provided that the following conditions
---- are met:
---- 1. Redistributions of source code must retain the above copyright
----    notice, this list of conditions and the following disclaimer.
---- 2. Redistributions in binary form must reproduce the above copyright
----    notice, this list of conditions and the following disclaimer in the
----    documentation and/or other materials provided with the distribution.
----
---- THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
---- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
---- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
---- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
---- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
---- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
---- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
---- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
---- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
---- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
---- SUCH DAMAGE.
----

{-|
    Module      : RISCV.RV32_Zicond
    Description : RISC-V Integer conditional operations

    The 'RISCV.RV32_Zifencei' module provides the description of the RISC-V RV32
    Integer conditional operations
-}



module RISCV.RV32_Zicond (
  -- * RV32 integer conditional operations, instruction definitions
  czero_eqz
, czero_nez
  -- * RV32 integer conditional operations, others
, rv32_zicond_disass
, rv32_zicond
) where

import RISCV.Helpers (prettyR)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)

czero_eqz_raw           =                      "0000111 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
czero_eqz rd rs1 rs2    = encode czero_eqz_raw          rs2      rs1          rd
czero_nez_raw           =                      "0000111 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
czero_nez rd rs1 rs2    = encode czero_nez_raw          rs2      rs1          rd

rv32_zicond_disass :: [DecodeBranch String]
rv32_zicond_disass = [ czero_eqz_raw --> prettyR "czero.eqz"
                     , czero_nez_raw --> prettyR "czero.nez"
                     ]


-- | List of RV32 integer conditional operations
rv32_zicond :: Integer -> Integer -> Integer -> [Instruction]
rv32_zicond src1 src2 dest = [ czero_eqz dest src1 src2
                             , czero_nez dest src1 src2
                             ]
