--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019-2020 Alexandre Joannou
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
    Module      : RISCV.RV32_I
    Description : RISC-V RV32 base integer instruction set

    The 'RISCV.RV32_I' module provides the description of the RISC-V RV32
    base integer instruction set
-}

module RISCV.RV32_I (
-- * RV32 base integer instruction set, instruction definitions
  add
, slt
, sltu
, and
, or
, xor
, sll
, srl
, sub
, sra
, addi
, slti
, sltiu
, andi
, ori
, xori
, slli
, srli
, srai
, lui
, auipc
, jal
, jalr
, beq
, bne
, blt
, bltu
, bge
, bgeu
, lb
, lbu
, lh
, lhu
, lw
, sb
, sh
, sw
, fence
, resrvd
, mret
, sret
, uret
, ecall
, ebreak
, sfence
-- * Extract and shrink functions
, extract_1op
, extract_2op
, extract_imm
, extract_addi
, extract_uimm
, extract_nodst
, shrink_arith
, shrink_addi
, shrink_imm
, shrink_uimm
, shrink_branch
, shrink_load
, shrink_store
, shrink_illegal
-- * RV32 base integer instruction set, others
, rv32_i_disass
, rv32_i_extract
, rv32_i_shrink
, rv32_i
, rv32_i_arith
, rv32_i_ctrl
, rv32_i_ctrl_jumps
, rv32_i_ctrl_branches
, rv32_i_exc
, rv32_i_load
, rv32_i_store
, rv32_i_fence
, rv32_i_mem
) where

import RISCV.Helpers (prettyR, prettyI, prettyI_sig, prettyU, prettyU_jal, prettyB, prettyF
                     ,prettySfence, prettyS, prettyL, ExtractedRegs)
import InstrCodec (DecodeBranch, (-->), encode)
import Prelude hiding (and, or)

add_raw          =                   "0000000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
add rd rs1 rs2   = encode add_raw             rs2      rs1          rd
slt_raw          =                   "0000000 rs2[4:0] rs1[4:0] 010 rd[4:0] 0110011"
slt rd rs1 rs2   = encode slt_raw             rs2      rs1          rd
sltu_raw         =                   "0000000 rs2[4:0] rs1[4:0] 011 rd[4:0] 0110011"
sltu rd rs1 rs2  = encode sltu_raw            rs2      rs1          rd
and_raw          =                   "0000000 rs2[4:0] rs1[4:0] 111 rd[4:0] 0110011"
and rd rs1 rs2   = encode and_raw             rs2      rs1          rd
or_raw           =                   "0000000 rs2[4:0] rs1[4:0] 110 rd[4:0] 0110011"
or rd rs1 rs2    = encode or_raw              rs2      rs1          rd
xor_raw          =                   "0000000 rs2[4:0] rs1[4:0] 100 rd[4:0] 0110011"
xor rd rs1 rs2   = encode xor_raw             rs2      rs1          rd
sll_raw          =                   "0000000 rs2[4:0] rs1[4:0] 001 rd[4:0] 0110011"
sll rd rs1 rs2   = encode sll_raw             rs2      rs1          rd
srl_raw          =                   "0000000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
srl rd rs1 rs2   = encode srl_raw             rs2      rs1          rd
sub_raw          =                   "0100000 rs2[4:0] rs1[4:0] 000 rd[4:0] 0110011"
sub rd rs1 rs2   = encode sub_raw             rs2      rs1          rd
sra_raw          =                   "0100000 rs2[4:0] rs1[4:0] 101 rd[4:0] 0110011"
sra rd rs1 rs2   = encode sra_raw             rs2      rs1          rd
addi_raw         =                   "imm[11:0] rs1[4:0] 000 rd[4:0] 0010011"
addi rd rs1 imm  = encode addi_raw    imm       rs1          rd
slti_raw         =                   "imm[11:0] rs1[4:0] 010 rd[4:0] 0010011"
slti rd rs1 imm  = encode slti_raw    imm       rs1          rd
sltiu_raw        =                   "imm[11:0] rs1[4:0] 011 rd[4:0] 0010011"
sltiu rd rs1 imm = encode sltiu_raw   imm       rs1          rd
andi_raw         =                   "imm[11:0] rs1[4:0] 111 rd[4:0] 0010011"
andi rd rs1 imm  = encode andi_raw    imm       rs1          rd
ori_raw          =                   "imm[11:0] rs1[4:0] 110 rd[4:0] 0010011"
ori rd rs1 imm   = encode ori_raw     imm       rs1          rd
xori_raw         =                   "imm[11:0] rs1[4:0] 100 rd[4:0] 0010011"
xori rd rs1 imm  = encode xori_raw    imm       rs1          rd
slli_raw         =                   "0000000 imm[4:0] rs1[4:0] 001 rd[4:0] 0010011"
slli rd rs1 imm  = encode slli_raw            imm      rs1          rd
srli_raw         =                   "0000000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011"
srli rd rs1 imm  = encode srli_raw            imm      rs1          rd
srai_raw         =                   "0100000 imm[4:0] rs1[4:0] 101 rd[4:0] 0010011"
srai rd rs1 imm  = encode srai_raw            imm      rs1          rd
lui_raw          =                   "imm[19:0] rd[4:0] 0110111"
lui rd imm       = encode lui_raw     imm       rd
auipc_raw        =                   "imm[19:0] rd[4:0] 0010111"
auipc rd imm     = encode auipc_raw   imm       rd
jal_raw          =                   "imm[19:19] imm[9:0] imm[10:10] imm[18:11] rd[4:0] 1101111"
jal rd imm       = encode jal_raw     imm                                       rd
jalr_raw         =                   "imm[11:0] rs1[4:0] 000 rd[4:0] 1100111"
jalr rd rs1 imm  = encode jalr_raw    imm       rs1          rd
beq_raw          =                   "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 000 im[3:0] im[10:10] 1100011"
beq rs1 rs2 im   = encode beq_raw     im                rs2      rs1
bne_raw          =                   "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 001 im[3:0] im[10:10] 1100011"
bne rs1 rs2 im   = encode bne_raw     im                rs2      rs1
blt_raw          =                   "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 100 im[3:0] im[10:10] 1100011"
blt rs1 rs2 im   = encode blt_raw     im                rs2      rs1
bltu_raw         =                   "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 110 im[3:0] im[10:10] 1100011"
bltu rs1 rs2 im  = encode bltu_raw    im                rs2      rs1
bge_raw          =                   "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 101 im[3:0] im[10:10] 1100011"
bge rs1 rs2 im   = encode bge_raw     im                rs2      rs1
bgeu_raw         =                   "im[11:11] im[9:4] rs2[4:0] rs1[4:0] 111 im[3:0] im[10:10] 1100011"
bgeu rs1 rs2 im  = encode bgeu_raw    im                rs2      rs1
lb_raw           =                   "imm[11:0] rs1[4:0] 000 rd[4:0] 0000011"
lb rd rs1 imm    = encode lb_raw      imm       rs1          rd
lbu_raw          =                   "imm[11:0] rs1[4:0] 100 rd[4:0] 0000011"
lbu rd rs1 imm   = encode lbu_raw     imm       rs1          rd
lh_raw           =                   "imm[11:0] rs1[4:0] 001 rd[4:0] 0000011"
lh rd rs1 imm    = encode lh_raw      imm       rs1          rd
lhu_raw          =                   "imm[11:0] rs1[4:0] 101 rd[4:0] 0000011"
lhu rd rs1 imm   = encode lhu_raw     imm       rs1          rd
lw_raw           =                   "imm[11:0] rs1[4:0] 010 rd[4:0] 0000011"
lw rd rs1 imm    = encode lw_raw      imm       rs1          rd
sb_raw           =                   "imm[11:5] rs2[4:0] rs1[4:0] 000 imm[4:0] 0100011"
sb rs1 rs2 imm   = encode sb_raw      imm       rs2      rs1
sh_raw           =                   "imm[11:5] rs2[4:0] rs1[4:0] 001 imm[4:0] 0100011"
sh rs1 rs2 imm   = encode sh_raw      imm       rs2      rs1
sw_raw           =                   "imm[11:5] rs2[4:0] rs1[4:0] 010 imm[4:0] 0100011"
sw rs1 rs2 imm   = encode sw_raw      imm       rs2      rs1
fence_raw        =                   "0000 pred[3:0] succ[3:0] 00000 000 00000 0001111"
fence pred succ  = encode fence_raw        pred      succ
resrvd_raw       =                   "0000 0000 0000 00000 000 00000 0000000"
resrvd           = encode resrvd_raw
mret_raw         =                   "0011 0000 0010 00000 000 00000 1110011"
mret             = encode mret_raw
sret_raw         =                   "0001 0000 0010 00000 000 00000 1110011"
sret             = encode sret_raw
uret_raw         =                   "0000 0000 0010 00000 000 00000 1110011"
uret             = encode uret_raw
ecall_raw        =                   "000000000000 00000 000 00000 1110011"
ecall            = encode ecall_raw
ebreak_raw       =                   "000000000001 00000 000 00000 1110011"
ebreak           = encode ebreak_raw
sfence_raw       =                   "0001001 rs2[4:0] rs1[4:0] 000 00000 1110011"
sfence rs1 rs2   = encode sfence_raw          rs2      rs1

-- | Dissassembly of RV32 base integer instructions
rv32_i_disass :: [DecodeBranch String]
rv32_i_disass = [ add_raw    --> prettyR "add"
                , slt_raw    --> prettyR "slt"
                , sltu_raw   --> prettyR "sltu"
                , and_raw    --> prettyR "and"
                , or_raw     --> prettyR "or"
                , xor_raw    --> prettyR "xor"
                , sll_raw    --> prettyR "sll"
                , srl_raw    --> prettyR "srl"
                , sub_raw    --> prettyR "sub"
                , sra_raw    --> prettyR "sra"
                , addi_raw   --> prettyI_sig "addi"
                , slti_raw   --> prettyI_sig "slti"
                , sltiu_raw  --> prettyI "sltiu"
                , andi_raw   --> prettyI "andi"
                , ori_raw    --> prettyI "ori"
                , xori_raw   --> prettyI "xori"
                , slli_raw   --> prettyI "slli"
                , srli_raw   --> prettyI "srli"
                , srai_raw   --> prettyI "srai"
                , lui_raw    --> prettyU "lui"
                , auipc_raw  --> prettyU "auipc"
                , jal_raw    --> prettyU_jal "jal"
                , jalr_raw   --> prettyI_sig "jalr"
                , beq_raw    --> prettyB "beq"
                , bne_raw    --> prettyB "bne"
                , blt_raw    --> prettyB "blt"
                , bltu_raw   --> prettyB "bltu"
                , bge_raw    --> prettyB "bge"
                , bgeu_raw   --> prettyB "bgeu"
                , lb_raw     --> prettyL "lb"
                , lbu_raw    --> prettyL "lbu"
                , lh_raw     --> prettyL "lh"
                , lhu_raw    --> prettyL "lhu"
                , lw_raw     --> prettyL "lw"
                , sb_raw     --> prettyS "sb"
                , sh_raw     --> prettyS "sh"
                , sw_raw     --> prettyS "sw"
                , fence_raw  --> prettyF
                , resrvd_raw --> "reserved"
                , mret_raw   --> "mret"
                , sret_raw   --> "sret"
                , uret_raw   --> "uret"
                , ecall_raw  --> "ecall"
                , ebreak_raw --> "ebreak"
                , sfence_raw --> prettySfence ]

extract_1op :: String -> Integer -> Integer -> ExtractedRegs
extract_1op instr rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode instr y z)

extract_2op :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_2op instr rs2 rs1 rd = (False, Just rs2, Just rs1, Just rd, \x y z -> encode instr x y z)

extract_imm :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_imm instr imm rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode instr imm y z)

extract_addi :: Integer -> Integer -> Integer -> ExtractedRegs
extract_addi imm rs1 rd = if imm == 0 then (True, Nothing, Just rs1, Just rd, \x y z -> encode addi_raw 0 y z)
                                    else extract_imm addi_raw imm rs1 rd

extract_uimm :: String -> Integer -> Integer -> ExtractedRegs
extract_uimm instr uimm rd = (False, Nothing, Nothing, Just rd, \x y z -> encode instr uimm z)

extract_nodst :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_nodst instr imm rs2 rs1 = (False, Just rs2, Just rs1, Nothing, \x y z -> encode instr imm x y)

rv32_i_extract :: [DecodeBranch ExtractedRegs]
rv32_i_extract = [ add_raw    --> extract_2op add_raw
                 , slt_raw    --> extract_2op slt_raw
                 , sltu_raw   --> extract_2op sltu_raw
                 , and_raw    --> extract_2op and_raw
                 , or_raw     --> extract_2op or_raw
                 , xor_raw    --> extract_2op xor_raw
                 , sll_raw    --> extract_2op sll_raw
                 , srl_raw    --> extract_2op srl_raw
                 , sub_raw    --> extract_2op sub_raw
                 , sra_raw    --> extract_2op sra_raw
                 , addi_raw   --> extract_addi
                 , slti_raw   --> extract_imm slti_raw
                 , sltiu_raw  --> extract_imm sltiu_raw
                 , andi_raw   --> extract_imm andi_raw
                 , ori_raw    --> extract_imm ori_raw
                 , xori_raw   --> extract_imm xori_raw
                 , slli_raw   --> extract_imm slli_raw
                 , srli_raw   --> extract_imm srli_raw
                 , srai_raw   --> extract_imm srai_raw
                 , lui_raw    --> extract_uimm lui_raw
                 , auipc_raw  --> extract_uimm auipc_raw
                 , jal_raw    --> extract_uimm jal_raw
                 , jalr_raw   --> extract_imm jalr_raw
                 , beq_raw    --> extract_nodst beq_raw
                 , bne_raw    --> extract_nodst bne_raw
                 , blt_raw    --> extract_nodst blt_raw
                 , bltu_raw   --> extract_nodst bltu_raw
                 , bge_raw    --> extract_nodst bge_raw
                 , bgeu_raw   --> extract_nodst bgeu_raw
                 , lb_raw     --> extract_imm lb_raw
                 , lbu_raw    --> extract_imm lbu_raw
                 , lh_raw     --> extract_imm lh_raw
                 , lhu_raw    --> extract_imm lhu_raw
                 , lw_raw     --> extract_imm lw_raw
                 , sb_raw     --> extract_nodst sb_raw
                 , sh_raw     --> extract_nodst sh_raw
                 , sw_raw     --> extract_nodst sw_raw
--               , fence_raw  --> noextract
--               , resrvd_raw --> noextract
--               , mret_raw   --> noextract
--               , sret_raw   --> noextract
--               , uret_raw   --> noextract
--               , ecall_raw  --> noextract
--               , ebreak_raw --> noextract
--               , sfence_raw --> noextract
                 ]

shrink_arith :: Integer -> Integer -> Integer -> [Integer]
shrink_arith rs2 rs1 rd = [addi rd 0 0, addi rd 0 1, addi rd 0 0xfff, addi rd rs1 0, addi rd rs2 0]

shrink_addi :: Integer -> Integer -> Integer -> [Integer]
shrink_addi imm rs rd = if imm == 0 then [] else [addi rd rs 0]

shrink_imm :: Integer -> Integer -> Integer -> [Integer]
shrink_imm imm rs rd = [addi rd 0 0, addi rd 0 1, addi rd rs imm, addi rd 0 imm, addi rd rs 0]

shrink_uimm :: Integer -> Integer -> [Integer]
shrink_uimm uimm rd = [addi rd 0 0, addi rd 0 0xfff]

shrink_branch :: Integer -> Integer -> Integer -> [Integer]
shrink_branch imm rs2 rs1 = [sltu 1 rs1 rs2 , sltu 1 rs2 rs1, slt 1 rs1 rs2, slt 1 rs2 rs1, jal 0 imm]

shrink_load :: Integer -> Integer -> Integer -> [Integer]
shrink_load imm rs rd = []
--shrink_load imm rs rd = [ecall, addi rd 0 0]

shrink_store :: Integer -> Integer -> Integer -> [Integer]
shrink_store imm rs2 rs1 = []
--shrink_store imm rs2 rs1 = [ecall]

shrink_illegal :: [Integer]
shrink_illegal = [ecall]

rv32_i_shrink :: [DecodeBranch [Integer]]
rv32_i_shrink = [ add_raw    --> shrink_arith
                , slt_raw    --> shrink_arith
                , sltu_raw   --> shrink_arith
                , and_raw    --> shrink_arith
                , or_raw     --> shrink_arith
                , xor_raw    --> shrink_arith
                , sll_raw    --> shrink_arith
                , srl_raw    --> shrink_arith
                , sub_raw    --> shrink_arith
                , sra_raw    --> shrink_arith
                , addi_raw   --> shrink_addi
                , slti_raw   --> shrink_imm
                , sltiu_raw  --> shrink_imm
                , andi_raw   --> shrink_imm
                , ori_raw    --> shrink_imm
                , xori_raw   --> shrink_imm
                , slli_raw   --> shrink_imm
                , srli_raw   --> shrink_imm
                , srai_raw   --> shrink_imm
                , lui_raw    --> shrink_uimm
                , auipc_raw  --> shrink_uimm
--              , jal_raw    --> noshrink
--              , jalr_raw   --> noshrink
                , beq_raw    --> shrink_branch
                , bne_raw    --> shrink_branch
                , blt_raw    --> shrink_branch
                , bltu_raw   --> shrink_branch
                , bge_raw    --> shrink_branch
                , bgeu_raw   --> shrink_branch
                , lb_raw     --> shrink_load
                , lbu_raw    --> shrink_load
                , lh_raw     --> shrink_load
                , lhu_raw    --> shrink_load
                , lw_raw     --> shrink_load
                , sb_raw     --> shrink_store
                , sh_raw     --> shrink_store
                , sw_raw     --> shrink_store
--              , fence_raw  --> noshrink
                , resrvd_raw --> shrink_illegal
                , mret_raw   --> shrink_illegal
                , sret_raw   --> shrink_illegal
                , uret_raw   --> shrink_illegal
--              , ecall_raw  --> noshrink
                , ebreak_raw --> shrink_illegal ]

-- | List of RV32 base integer arithmetic instructions
rv32_i_arith :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_i_arith src1 src2 dest imm longImm = [ add   dest  src1 src2
                                          , slt   dest  src1 src2
                                          , sltu  dest  src1 src2
                                          , and   dest  src1 src2
                                          , or    dest  src1 src2
                                          , xor   dest  src1 src2
                                          , sll   dest  src1 src2
                                          , srl   dest  src1 src2
                                          , sub   dest  src1 src2
                                          , sra   dest  src1 src2
                                          , addi  dest  src1      imm
                                          , slti  dest  src1      imm
                                          , sltiu dest  src1      imm
                                          , andi  dest  src1      imm
                                          , ori   dest  src1      imm
                                          , xori  dest  src1      imm
                                          , slli  dest  src1      imm
                                          , srli  dest  src1      imm
                                          , srai  dest  src1      imm
                                          , lui   dest            longImm ]

-- | List of RV32 base integer control instructions
rv32_i_ctrl :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_i_ctrl src1 src2 dest imm longImm = [ auipc dest           longImm ]
                                         ++ (rv32_i_ctrl_jumps src1 dest imm longImm)
                                         ++ (rv32_i_ctrl_branches src1 src2 imm)

-- | List of RV32 base integer control instructions: jumps
rv32_i_ctrl_jumps :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_i_ctrl_jumps src1 dest imm longImm = [ jal  dest longImm
                                          , jalr dest src1 imm ]

-- | List of RV32 base integer control instructions: branches
rv32_i_ctrl_branches :: Integer -> Integer -> Integer -> [Integer]
rv32_i_ctrl_branches src1 src2 imm = [ beq  src1 src2 imm
                                     , bne  src1 src2 imm
                                     , bge  src1 src2 imm
                                     , bgeu src1 src2 imm
                                     , blt  src1 src2 imm
                                     , bltu src1 src2 imm ]


-- | List of RV32 base integer exception-related instructions
rv32_i_exc :: [Integer]
rv32_i_exc = [ ecall
             , mret
             , sret
             , uret
             , ebreak
             , resrvd ]

-- | List of RV32 base integer load instructions
rv32_i_load :: Integer -> Integer -> Integer -> [Integer]
rv32_i_load src dest imm = [ lb  dest src imm
                           , lbu dest src imm
                           , lh  dest src imm
                           , lhu dest src imm
                           , lw  dest src imm ]

-- | List of RV32 base integer store instructions
rv32_i_store :: Integer -> Integer -> Integer -> [Integer]
rv32_i_store srcAddr srcData imm = [ sb srcAddr srcData imm
                                   , sh srcAddr srcData imm
                                   , sw srcAddr srcData imm ]

-- | List of RV32 base integer fence instructions
rv32_i_fence :: Integer -> Integer -> [Integer]
rv32_i_fence fenceOp1 fenceOp2 = [fence fenceOp1 fenceOp2]

-- | List of RV32 base integer memory instructions
rv32_i_mem :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
           -> [Integer] --TODO alignment
rv32_i_mem srcAddr srcData dest imm fenceOp1 fenceOp2 =
     (rv32_i_load srcAddr dest imm)
  ++ (rv32_i_store srcAddr srcData imm)
  ++ (rv32_i_fence fenceOp1 fenceOp2)

-- | List of RV32 base integer instructions
rv32_i :: Integer -> Integer -> Integer -> Integer
       -> Integer -> Integer -> Integer
       -> [Integer]
rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2 =
     (rv32_i_arith srcAddr srcData dest imm longImm)
  ++ rv32_i_exc
  ++ (rv32_i_mem srcAddr srcData dest imm fenceOp1 fenceOp2)
  ++ (rv32_i_ctrl srcAddr srcData dest imm longImm)
