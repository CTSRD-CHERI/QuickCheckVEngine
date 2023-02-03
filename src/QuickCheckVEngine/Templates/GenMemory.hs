{-# LANGUAGE BlockArguments #-}
--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019, 2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.GenMemory (
  gen_rv32_i_memory
, gen_rv32_i_a_memory
, gen_rv32_i_zifencei_memory
, gen_rv32_i_a_zifencei_memory
, gen_rv32_i_cache
, gen_rv64_i_memory
, gen_rv64_i_a_memory
, gen_rv64_i_zifencei_memory
, gen_rv64_i_a_zifencei_memory
, gen_rv64_i_cache
, gen_rv32_Xcheri_cache
, gen_rv64_Xcheri_cache
, gen_pte_perms
, gen_pte39_trans_core
, gen_pte48_trans_core
, gen_pte_trans
) where

import Test.QuickCheck
import RISCV.RV32_I
import RISCV.RV32_A
import RISCV.RV32_Zifencei
import RISCV.RV32_Zicsr
import RISCV.RV64_I
import RISCV.RV32_Xcheri
import RISCV.RV_CSRs
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import Data.Bits

data GenConf = GenConf { use_a        :: Bool
                       , use_zifencei :: Bool
                       , use_xlen_64  :: Bool
                       , use_caplen   :: Bool
                       }

gen_rv32_i_memory :: Template
gen_rv32_i_memory = gen_memory $ GenConf False False False False

gen_rv32_i_a_memory :: Template
gen_rv32_i_a_memory = gen_memory $ GenConf True False False False

gen_rv32_i_zifencei_memory :: Template
gen_rv32_i_zifencei_memory = gen_memory $ GenConf False True False False

gen_rv32_i_a_zifencei_memory :: Template
gen_rv32_i_a_zifencei_memory = gen_memory $ GenConf True True False False

gen_rv32_i_cache :: Bool -> Template
gen_rv32_i_cache with_zifencei =
  gen_cache $ GenConf False with_zifencei False False

gen_rv64_i_memory :: Template
gen_rv64_i_memory = gen_memory $ GenConf False False True False

gen_rv64_i_a_memory :: Template
gen_rv64_i_a_memory = gen_memory $ GenConf True False True False

gen_rv64_i_zifencei_memory :: Template
gen_rv64_i_zifencei_memory = gen_memory $ GenConf False True True False

gen_rv64_i_a_zifencei_memory :: Template
gen_rv64_i_a_zifencei_memory = gen_memory $ GenConf True True True False

gen_rv64_i_cache :: Bool -> Template
gen_rv64_i_cache with_zifencei =
  gen_cache $ GenConf False with_zifencei True False

gen_rv32_Xcheri_cache :: Bool -> Template
gen_rv32_Xcheri_cache with_zifencei =
  gen_cache $ GenConf False with_zifencei False True

gen_rv64_Xcheri_cache :: Bool -> Template
gen_rv64_Xcheri_cache with_zifencei =
  gen_cache $ GenConf False with_zifencei True True

gen_memory :: GenConf -> Template
gen_memory conf = random do
  src1     <- src
  src2     <- src
  dst      <- dest
  fenceOp1 <- bits 4
  fenceOp2 <- bits 4
  aq       <- bits 1
  rl       <- bits 1
  offset   <- geomBits 11 0
  let insts = [[(8,  inst $ addi dst src1 offset)
              , (8,  inst $ ori dst src1 offset)
              , (16, instSeq [ lui 0x40004 dst
                             , slli dst dst 1 ])
              , (8, instUniform $ rv32_i_load  src1 dst offset)
              , (8, instUniform $ rv32_i_store src1 src2 offset)
              , (2, instUniform $ rv32_i_fence fenceOp1 fenceOp2)]]
              ++ [[(2, instUniform $ rv32_a src1 src2 dst aq rl)] | use_a conf]
              ++ [[(2,  inst fence_i)] | use_zifencei conf]
              ++ [[(8, instUniform $ rv64_i_load  src1 dst offset)
                 , (8, instUniform $ rv64_i_store src1 src2 offset)]
                 | use_xlen_64 conf]
  return . dist . concat $ insts

gen_cache :: GenConf -> Template
gen_cache conf = random do
  src1     <- elements [1, 2, 3]
  src2     <- elements [1, 2, 3]
  dst      <- elements [1, 2, 3]

  aq       <- bits 1
  rl       <- bits 1
  offseta  <- elements [1, 4, 16, 64, 65]
  offsetb  <- elements [1, 4, 16, 64, 65]
  offsetc  <- elements [1, 4, 16, 64, 65]
  upperIa  <- elements [0x40000, 0x40100, 0x40200, 0x40300, 0x40400, 0x40500]
  upperIb  <- elements [0x40000, 0x40100, 0x40200, 0x40300, 0x40400, 0x40500]
  upperIc  <- elements [0x40000, 0x40100, 0x40200, 0x40300, 0x40400, 0x40500]
  let prologue_list = [ lui 1 upperIa
                      , slli 1 1 1
                      , addi 1 1 offseta
                      , lui 2 upperIb
                      , slli 2 2 2
                      , addi 2 2 offsetb
                      , lui 3 upperIc
                      , slli 3 3 1
                      , addi 3 3 offsetc]
  let insts = [[ (8, random $ do src' <- elements [1, 2, 3]
                                 return . instUniform $ rv32_i_load  src' 13 0)
               , (8, random $ do src1' <- elements [1, 2, 3]
                                 src2' <- elements [1, 2, 3]
                                 return . instUniform $
                                   rv32_i_store src1' src2' 0)
               , (2, random $ do fenceOp1 <- bits 4
                                 fenceOp2 <- bits 4
                                 return . instUniform $
                                   rv32_i_fence fenceOp1 fenceOp2)
               ]]
           ++ [[(2, instUniform $ rv32_a src1 src2 dst aq rl)] | use_a conf]
           ++ [[(2, inst fence_i)] | use_zifencei conf]
           ++ [[ (2, random $ do src' <- elements [1, 2, 3]
                                 return . instUniform $ rv64_i_load  src' 13 0)
               , (2, random $ do src1' <- elements [1, 2, 3]
                                 src2' <- elements [1, 2, 3]
                                 return . instUniform $
                                   rv64_i_store src1' src2' 0)
               ] | use_xlen_64 conf]
           ++ [[ (2, random $ do srcAddr <- elements [1, 2, 3]
                                 return . instSeq $
                                   [lq 13 srcAddr 0, cgettag 13 13])
               , (2, random $ do srcData <- elements [1, 2, 3, 4, 5]
                                 srcAddr <- elements [1, 2, 3]
                                 return . inst $ sq srcAddr srcData 0)
               ] | use_caplen conf]
  let prologue = instSeq prologue_list
  return $ prologue <> (repeatTillEnd . dist . concat $ insts)

gen_pte_perms :: Template
gen_pte_perms = random do
  lperms <- bits 10
  uperms <- bits 5
  clg0 <- bits 2
  clg1 <- bits 2
  return . shrinkScope $
    instSeq [ ori 1 0 uperms -- two cheri pte bits
            , slli 1 1 16
            , ori 1 1 0x000 -- 11 msbs of PA
            , slli 1 1 11
            , ori 1 1 0x000 -- next 11 bits of PA
            , slli 1 1 11
            , ori 1 1 0x100 -- next 11 bits of PA
            , slli 1 1 11
            , ori 1 1 0x000 -- next 11 bits of PA
            , slli 1 1 10
            , ori 1 1 lperms -- Permissions bits
            , addi 5 0 1
            , slli 5 5 31
            , slli 5 5 31
            , slli 5 5 1
            , lui 6 0x80
            , add 5 5 6
            , lui 7 0x40000
            , slli 7 7 1
            , sd 7 1 0
            , csrrw 0 (unsafe_csrs_indexFromName "satp") 5
            , csrrwi 0 (unsafe_csrs_indexFromName "sccsr") (clg0 * 4) ]
    <> (noShrink $ inst $ sfence 0 0)
    <> mconcat [ inst sret
               , instUniform [ccleartag 3 3, cmove 3 3]
               , instUniform [sw 0 3 16, sq 0 3 16]
               , instUniform [lw 4 0 16, lq 4 0 16]
               , inst $ csrrwi 0 (unsafe_csrs_indexFromName "sccsr") (clg1 * 4)
               , instUniform [lw 4 0 16, lq 4 0 16]
               , inst $ cgettag 5 4
               , inst ecall ]

gen_pte39_trans_core :: Integer -> Integer -> Integer -> Template
gen_pte39_trans_core lxReg addrReg pteReg = random do
  let leafperms = 0xFF
  let parentperms = 0xF1
  l2perms <- elements [leafperms, parentperms]
  l1perms <- elements [leafperms, parentperms]
  l0perms <- elements [leafperms, parentperms]
  let satpa = 0x803ff000
  let l2pa  = 0x80000000
  let l1pa  = 0x80200000
  let l0pa  = 0x80001000
  let satp = (shiftL 0x8 60) + (shiftR satpa 12)
  let l2pte = (shiftR l2pa 2) + l2perms
  let l1pte = (shiftR l1pa 2) + l1perms
  let l0pte = (shiftR l0pa 2) + l0perms
  addrInitial <- elements [0x00000000, 0x00000800, 0x00100000]
  return . shrinkScope $
    mconcat [ li64 pteReg l2pte
            , li64 lxReg  satpa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg l1pte
            , li64 lxReg  l2pa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg l0pte
            , li64 lxReg  l1pa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg satp
            , inst $ csrrw 0 (unsafe_csrs_indexFromName "satp") pteReg
            , li64 addrReg addrInitial ]
    <> (noShrink . inst $ sfence 0 0)
    <> inst sret

gen_pte48_trans_core :: Integer -> Integer -> Integer -> Template
gen_pte48_trans_core lxReg addrReg pteReg = random do
  let leafperms = 0xFF
  let parentperms = 0xF1
  l3perms <- elements [leafperms, parentperms]
  l2perms <- elements [leafperms, parentperms]
  l1perms <- elements [leafperms, parentperms]
  l0perms <- elements [leafperms, parentperms]
  let satpa = 0x803ff000
  let l3pa  = 0x80000000
  let l2pa  = 0x80180000
  let l1pa  = 0x80200000
  let l0pa  = 0x80001000
  let satp = (shiftL 0x9 60) + (shiftR satpa 12)
  let l3pte = (shiftR l3pa 2) + l3perms
  let l2pte = (shiftR l2pa 2) + l2perms
  let l1pte = (shiftR l1pa 2) + l1perms
  let l0pte = (shiftR l0pa 2) + l0perms
  addrInitial <- elements [0x00000000, 0x00000800, 0x00100000]
  return . shrinkScope $
    mconcat [ li64 pteReg l3pte
            , li64 lxReg  satpa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg l2pte
            , li64 lxReg  l3pa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg l1pte
            , li64 lxReg  l2pa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg l0pte
            , li64 lxReg  l1pa
            , inst $ sd lxReg pteReg 0
            , li64 pteReg satp
            , inst $ csrrw 0 (unsafe_csrs_indexFromName "satp") pteReg
            , li64 addrReg addrInitial ]
    <> (noShrink . inst $ sfence 0 0)
    <> inst sret

gen_pte_trans :: Template
gen_pte_trans = random do
  let lxreg = 1
  let addrReg = 2
  let ptereg = 30
  return . shrinkScope $
       gen_pte39_trans_core lxreg addrReg ptereg
    <> (repeatTillEnd $ random do
         imm <- elements [0x10, 0x100, 0x0]
         datReg <- elements [ptereg, lxreg]
         return $ dist [ (8, instUniform $ rv64_i_load  addrReg datReg imm)
                       , (8, instUniform $ rv64_i_store addrReg datReg imm)
                       , (4, inst $ addi addrReg addrReg imm)
                       , (1, inst $ fence_i)
                       , (1, inst $ fence 0 0) ])
    <> (noShrink . inst $ ecall)
