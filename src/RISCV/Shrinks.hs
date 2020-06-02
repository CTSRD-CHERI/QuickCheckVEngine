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
  rv_shrink,
  extract_regs
) where

import RISCV.RV32_Xcheri
import RISCV.RV32_I
import InstrCodec

rv_shrink instr = case decode 32 instr shrinkList of
  Nothing -> []
  Just i -> i
  where shrinkList = rv32_xcheri_shrink ++ rv32_shrink

extract_regs instr = case decode 32 instr extractList of
  Nothing -> (False, Nothing, Nothing, Nothing, (\a b c -> instr))
  Just i -> i
  where extractList = rv32_xcheri_extract ++ rv32_extract

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

type ExtractedRegs = (Bool, Maybe Integer, Maybe Integer, Maybe Integer, Integer -> Integer -> Integer -> Integer)

extract_2op :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_2op instr rs2 rs1 rd = (False, Just rs2, Just rs1, Just rd, \x y z -> encode instr x y z)

extract_imm :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_imm instr imm rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode instr y z)

extract_addi :: Integer -> Integer -> Integer -> ExtractedRegs
extract_addi imm rs1 rd = if imm == 0 then (True, Nothing, Just rs1, Just rd, \x y z -> encode addi 0 y z)
                                    else extract_imm addi imm rs1 rd

extract_uimm :: String -> Integer -> Integer -> ExtractedRegs
extract_uimm instr uimm rd = (False, Nothing, Nothing, Just rd, \x y z -> encode instr uimm z)

extract_nodst :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_nodst instr imm rs2 rs1 = (False, Just rs2, Just rs1, Nothing, \x y z -> encode instr imm x y)

rv32_extract :: [DecodeBranch ExtractedRegs]
rv32_extract = [ add    --> extract_2op add
               , slt    --> extract_2op slt
               , sltu   --> extract_2op sltu
               , RISCV.RV32_I.and --> extract_2op RISCV.RV32_I.and
               , RISCV.RV32_I.or  --> extract_2op RISCV.RV32_I.or
               , xor    --> extract_2op xor
               , sll    --> extract_2op sll
               , srl    --> extract_2op srl
               , sub    --> extract_2op sub
               , sra    --> extract_2op sra
               , addi   --> extract_addi
               , slti   --> extract_imm slti
               , sltiu  --> extract_imm sltiu
               , andi   --> extract_imm andi
               , ori    --> extract_imm ori
               , xori   --> extract_imm xori
               , slli   --> extract_imm slli
               , srli   --> extract_imm srli
               , srai   --> extract_imm srai
               , lui    --> extract_uimm lui
               , auipc  --> extract_uimm auipc
               , jal    --> extract_uimm jal
               , jalr   --> extract_imm jalr
               , beq    --> extract_nodst beq
               , bne    --> extract_nodst bne
               , blt    --> extract_nodst blt
               , bltu   --> extract_nodst bltu
               , bge    --> extract_nodst bge
               , bgeu   --> extract_nodst bgeu
               , lb     --> extract_imm lb
               , lbu    --> extract_imm lbu
               , lh     --> extract_imm lh
               , lhu    --> extract_imm lhu
               , lw     --> extract_imm lw
               , sb     --> extract_nodst sb
               , sh     --> extract_nodst sh
               , sw     --> extract_nodst sw
--             , fence  --> noextract
--             , resrvd --> noextract
--             , mret   --> noextract
--             , sret   --> noextract
--             , uret   --> noextract
--             , ecall  --> noextract
--             , ebreak --> noextract
               ]

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

extract_1op :: String -> Integer -> Integer -> ExtractedRegs
extract_1op instr rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode instr y z)

extract_cspecialrw :: Integer -> Integer -> Integer -> ExtractedRegs
extract_cspecialrw idx rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode cspecialrw idx y z)

extract_cmove :: Integer -> Integer -> ExtractedRegs
extract_cmove rs1 rd = (True, Nothing, Just rs1, Just rd, \x y z -> encode cmove y z)

extract_ccall :: Integer -> Integer -> ExtractedRegs
extract_ccall rs2 rs1 = (False, Just rs2, Just rs1, Just 31, \x y z -> encode ccall x y)

extract_cstore :: Integer -> Integer -> Integer -> ExtractedRegs
extract_cstore imm rs2 rs1 = (False, Just rs2, Just rs1, Nothing, \x y z -> encode cstore imm x y)

rv32_xcheri_extract :: [DecodeBranch ExtractedRegs]
rv32_xcheri_extract = [ cgetperm            --> extract_1op cgetperm
                      , cgettype            --> extract_1op cgettype
                      , cgetbase            --> extract_1op cgetbase
                      , cgetlen             --> extract_1op cgetlen
                      , cgettag             --> extract_1op cgettag
                      , cgetsealed          --> extract_1op cgetsealed
                      , cgetoffset          --> extract_1op cgetoffset
                      , cgetflags           --> extract_1op cgetflags
                      , cgetaddr            --> extract_1op cgetaddr
                      , cseal               --> extract_2op cseal
                      , cunseal             --> extract_2op cunseal
                      , candperm            --> extract_2op candperm
                      , csetoffset          --> extract_2op csetoffset
                      , csetaddr            --> extract_2op csetaddr
                      , cincoffset          --> extract_2op cincoffset
                      , csetbounds          --> extract_2op csetbounds
                      , csetboundsexact     --> extract_2op csetboundsexact
                      , cbuildcap           --> extract_2op cbuildcap
                      , ccopytype           --> extract_2op ccopytype
                      , ccseal              --> extract_2op ccseal
                      , ccleartag           --> extract_1op ccleartag
                      , cincoffsetimmediate --> extract_imm cincoffsetimmediate
                      , csetboundsimmediate --> extract_imm csetboundsimmediate
                      , ctoptr              --> extract_2op ctoptr
                      , cfromptr            --> extract_2op cfromptr
                      , csub                --> extract_2op csub
                      , cspecialrw          --> extract_cspecialrw
                      , cmove               --> extract_cmove
                      , cjalr               --> extract_imm cjalr
                      , ccall               --> extract_ccall
                      , ctestsubset         --> extract_2op ctestsubset
--                      , clear               --> noextract -- TODO
--                      , fpclear             --> noextract -- TODO
                      , croundrepresentablelength   --> extract_1op croundrepresentablelength
                      , crepresentablealignmentmask --> extract_1op crepresentablealignmentmask
                      , cload               --> extract_imm cload
                      , cstore              --> extract_cstore
                      , csetflags           --> extract_2op csetflags
                      , sq                  --> extract_nodst sq
                      , lq                  --> extract_imm lq
                      ]
