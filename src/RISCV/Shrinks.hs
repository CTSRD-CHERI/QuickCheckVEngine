--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Alexandre Joannou
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2018 Matthew Naylor
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
    Module      : RISCV.Shrinks
    Description : RISC-V Instruction Shrink

    The 'RISCV.Shrinks' module provides the description of how instructions
    can be shrunk
-}

module RISCV.Shrinks (
  rv_shrink
) where

import RISCV.RV32_Xcheri
import RISCV.RV32_I
import InstrCodec

rv_shrink instr = case decode 32 instr shrinkList of
  Nothing -> []
  Just i -> i
  where shrinkList = rv32_xcheri_shrink ++ rv32_shrink

shrink_arith :: Integer -> Integer -> Integer -> [Integer]
shrink_arith rs2 rs1 rd = [encode addi 0 0 rd, encode addi 1 0 rd, encode addi 0xfff 0 rd, encode addi 0 rs1 rd, encode addi 0 rs2 rd]

shrink_addi :: Integer -> Integer -> Integer -> [Integer]
shrink_addi imm rs rd = if imm == 0 then [] else [encode addi 0 rs rd]

shrink_imm :: Integer -> Integer -> Integer -> [Integer]
shrink_imm imm rs rd = [encode addi 0 0 rd, encode addi 1 0 rd, encode addi imm rs rd, encode addi imm 0 rd, encode addi 0 rs rd]

shrink_uimm :: Integer -> Integer -> [Integer]
shrink_uimm uimm rd = [encode addi 0 0 rd, encode addi 0xfff 0 rd]

shrink_branch :: Integer -> Integer -> Integer -> [Integer]
shrink_branch imm rs2 rs1 = [encode sltu rs2 rs1 1, encode sltu rs1 rs2 1, encode slt rs2 rs1 1, encode slt rs1 rs2 1, encode jal imm 0]

shrink_load :: Integer -> Integer -> Integer -> [Integer]
shrink_load imm rs rd = [encode ecall, encode addi 0 0 rd]

shrink_store :: Integer -> Integer -> Integer -> [Integer]
shrink_store imm rs2 rs1 = [encode ecall]

shrink_illegal :: [Integer]
shrink_illegal = [encode ecall]

rv32_shrink :: [DecodeBranch [Integer]]
rv32_shrink = [ add    --> shrink_arith
              , slt    --> shrink_arith
              , sltu   --> shrink_arith
              , RISCV.RV32_I.and --> shrink_arith
              , RISCV.RV32_I.or  --> shrink_arith
              , xor    --> shrink_arith
              , sll    --> shrink_arith
              , srl    --> shrink_arith
              , sub    --> shrink_arith
              , sra    --> shrink_arith
              , addi   --> shrink_addi
              , slti   --> shrink_imm
              , sltiu  --> shrink_imm
              , andi   --> shrink_imm
              , ori    --> shrink_imm
              , xori   --> shrink_imm
              , slli   --> shrink_imm
              , srli   --> shrink_imm
              , srai   --> shrink_imm
              , lui    --> shrink_uimm
              , auipc  --> shrink_uimm
--            , jal    --> noshrink
--            , jalr   --> noshrink
              , beq    --> shrink_branch
              , bne    --> shrink_branch
              , blt    --> shrink_branch
              , bltu   --> shrink_branch
              , bge    --> shrink_branch
              , bgeu   --> shrink_branch
              , lb     --> shrink_load
              , lbu    --> shrink_load
              , lh     --> shrink_load
              , lhu    --> shrink_load
              , lw     --> shrink_load
              , sb     --> shrink_store
              , sh     --> shrink_store
              , sw     --> shrink_store
--            , fence  --> noshrink
              , resrvd --> shrink_illegal
              , mret   --> shrink_illegal
              , sret   --> shrink_illegal
              , uret   --> shrink_illegal
--            , ecall  --> noshrink
              , ebreak --> shrink_illegal ]

shrink_cgetperm :: Integer -> Integer -> [Integer]
shrink_cgetperm cs rd = [encode addi 0 0 rd, encode addi 0x7ff 0 rd]

shrink_cgettype :: Integer -> Integer -> [Integer]
shrink_cgettype cs rd = [encode addi 0 0 rd, encode addi 6 0 rd, encode addi 0xfff 0 rd]

shrink_cgetbase :: Integer -> Integer -> [Integer]
shrink_cgetbase cs rd = [encode addi 0 0 rd]

shrink_cgetlen :: Integer -> Integer -> [Integer]
shrink_cgetlen cs rd = [encode addi 0 0 rd, encode addi 0xfff 0 rd, encode cgetbase cs rd]

shrink_cgettag :: Integer -> Integer -> [Integer]
shrink_cgettag cs rd = [encode addi 1 0 rd, encode addi 0 0 rd]

shrink_cgetsealed :: Integer -> Integer -> [Integer]
shrink_cgetsealed cs rd = [encode addi 1 0 rd, encode addi 0 0 rd, encode cgettype cs rd]

shrink_cgetoffset :: Integer -> Integer -> [Integer]
shrink_cgetoffset cs rd = [encode addi 0 0 rd, encode cgetaddr cs rd]

shrink_cgetflags :: Integer -> Integer -> [Integer]
shrink_cgetflags cs rd = [encode addi 0 0 rd, encode addi 1 0 rd]

shrink_cgetaddr :: Integer -> Integer -> [Integer]
shrink_cgetaddr cs rd = [encode addi 0 cs rd]

shrink_cap :: Integer -> Integer -> [Integer]
shrink_cap cs cd = [encode ecall,
                    encode cmove cs cd,
                    encode cgetaddr cs cd,
                    encode cgetperm cs cd,
                    encode cgettype cs cd,
                    encode cgetbase cs cd,
                    encode cgetlen cs cd,
                    encode cgettag cs cd,
                    encode cgetoffset cs cd,
                    encode cgetflags cs cd
                   ]

shrink_capcap :: Integer -> Integer -> Integer -> [Integer]
shrink_capcap cs1 cs2 cd = (shrink_cap cs1 cd) ++ (shrink_cap cs2 cd)

shrink_capint :: Integer -> Integer -> Integer -> [Integer]
shrink_capint rs cs cd = shrink_cap cs cd

shrink_capimm :: Integer -> Integer -> Integer -> [Integer]
shrink_capimm imm cs cd = shrink_cap cs cd ++ [encode addi imm 0 cd, encode addi imm cs cd]

shrink_cmove :: Integer -> Integer -> [Integer]
shrink_cmove cs cd = [encode cgetaddr cs cd]

shrink_ccall :: Integer -> Integer -> [Integer]
shrink_ccall cs1 cs2 = shrink_capcap cs1 cs2 31

-- | Shrinking of CHERI instructions
rv32_xcheri_shrink :: [DecodeBranch [Integer]]
rv32_xcheri_shrink = [ cgetperm            --> shrink_cgetperm
                     , cgettype            --> shrink_cgettype
                     , cgetbase            --> shrink_cgetbase
                     , cgetlen             --> shrink_cgetlen
                     , cgettag             --> shrink_cgettag
                     , cgetsealed          --> shrink_cgetsealed
                     , cgetoffset          --> shrink_cgetoffset
                     , cgetflags           --> shrink_cgetflags
                     , cgetaddr            --> shrink_cgetaddr
                     , cseal               --> shrink_capcap
                     , cunseal             --> shrink_capcap
                     , candperm            --> shrink_capint
                     , csetoffset          --> shrink_capint
                     , csetaddr            --> shrink_capint
                     , cincoffset          --> shrink_capint
                     , csetbounds          --> shrink_capint
                     , csetboundsexact     --> shrink_capint
                     , cbuildcap           --> shrink_capcap
                     , ccopytype           --> shrink_capcap
                     , ccseal              --> shrink_capcap
                     , ccleartag           --> shrink_cap
                     , cincoffsetimmediate --> shrink_capimm
                     , csetboundsimmediate --> shrink_capimm
                     , ctoptr              --> shrink_capcap
                     , cfromptr            --> shrink_capint
                     , csub                --> shrink_capcap
--                     , cspecialrw          --> noshrink
                     , cmove               --> shrink_cmove
--                     , cjalr               --> noshrink
                     , ccall               --> shrink_ccall
--                     , ctestsubset         --> noshrink
--                     , clear               --> noshrink
--                     , fpclear             --> noshrink
--                     , croundrepresentablelength   --> noshrink
--                     , crepresentablealignmentmask --> noshrink
--                     , cload               --> noshrink
--                     , cstore              --> noshrink
                     , csetflags           --> shrink_capcap
--                     , sq                  --> noshrink
--                     , lq                  --> noshrink
                     ]
