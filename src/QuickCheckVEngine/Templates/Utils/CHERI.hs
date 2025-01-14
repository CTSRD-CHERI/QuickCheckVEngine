--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2020 Alexandre Joannou
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

module QuickCheckVEngine.Templates.Utils.CHERI (
  boundPCC
, clearASR
, makeCap
, makeCap_core
, makeShortCap
, legalCapLoad
, legalCapStore
, loadRegion
, switchEncodingMode
, cspecialRWChain
, tagCacheTest
, genCHERIinspection
, genCHERIarithmetic
, genCHERImisc
, genCHERIcontrol
) where

import Test.QuickCheck
import Data.Bits
import RISCV
import InstrCodec
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General

boundPCC :: Integer -> Integer -> Integer -> Integer -> Template
boundPCC tmp1 tmp2 offset size =
  mconcat [ inst $ cspecialrw tmp1 0 0, -- Get PCC
            li64 tmp2 offset,
            inst $ cadd tmp1 tmp1 tmp2, -- increment PCC
            li64 tmp2 size,
            inst $ scbndsr tmp1 tmp1 tmp2, -- reduce bounds
            inst $ modeswcap,
            inst $ jalr tmp1 tmp1 0 ] -- jump to new PCC

clearASR :: Integer -> Integer -> Template
clearASR tmp1 tmp2 = instSeq [ cspecialrw tmp1 0 0, -- Get PCC
                               addi tmp2 0 0xbff, -- Load immediate without ASR set
                               acperm tmp1 tmp1 tmp2, -- Mask out ASR
                               cspecialrw 0 28 tmp1, -- Clear ASR in trap vector
                               modeswcap,
                               jalr tmp1 0 0 ]

makeCap :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Template
makeCap dst source tmp base len offset =
  mconcat [ makeCap_core dst source tmp base
          , li64 tmp len
          , inst $ scbndsr dst dst tmp
          , li64 tmp offset
          , inst $ cadd dst dst tmp ]

makeCap_core :: Integer -> Integer -> Integer -> Integer -> Template
makeCap_core dst source tmp base =
  mconcat [ li64 tmp base
          , inst $ scaddr dst source tmp]

makeShortCap :: Template
makeShortCap = random $ do
  dst <- dest
  source <- src
  tmp <- src
  len <- choose (0, 31)
  offset <- oneof [choose (0,32), bits 14]
  return $ instSeq [ scbndsi dst source 0 len,
                     addi tmp 0 (Data.Bits.shift offset (-12)),
                     gcbase dst tmp,
                     addi dst dst 12,
                     scaddr dst tmp dst,
                     caddi dst dst (offset Data.Bits..&. 0xfff)]

legalCapLoad :: Integer -> Integer -> Template
legalCapLoad addrReg targetReg = random $ do
  tmpReg <- src
  return $ instSeq [ andi addrReg addrReg 0xff
                   , lui tmpReg 0x40004
                   , slli tmpReg tmpReg 1
                   , add addrReg tmpReg addrReg
                   , lc targetReg addrReg 0 ]

legalCapStore :: Integer -> Template
legalCapStore addrReg = random $ do
  tmpReg  <- src
  dataReg <- dest
  return $ instSeq [ andi addrReg addrReg 0xff
                   , lui tmpReg 0x40004
                   , slli tmpReg tmpReg 1
                   , add addrReg tmpReg addrReg
                   , sc dataReg addrReg 0 ]

loadRegion ::  Integer -> Integer -> Integer -> Integer -> Template -> Template
loadRegion numLines capReg cacheLSize tmpReg insts =
   if numLines == 0 then insts
   else if numLines == 1 then mconcat [insts, inst (lc tmpReg capReg 0)]
   else loadRegion (numLines - 1) capReg cacheLSize tmpReg (mconcat [insts, inst (lc tmpReg capReg 0), inst (caddi capReg capReg cacheLSize)])

switchEncodingMode :: Template
switchEncodingMode = random $ do
  tmpReg1 <- sbcRegs
  let tmpReg2 = tmpReg1 + 1
  mode    <- elements [0, 1]
  return $ instSeq [ cspecialrw tmpReg1 0 0
                   , addi tmpReg2 0 mode
                   , scmode tmpReg1 tmpReg1 tmpReg2
                   , cspecialrw 0 28 tmpReg1 --Also write trap vector so we stay in cap mode
                   , if mode == 1 then modeswint else modeswcap  ]

cspecialRWChain :: Template
cspecialRWChain = random $ do
  tmpReg1 <- src
  tmpReg2 <- src
  tmpReg3 <- src
  tmpReg4 <- src
  tmpReg5 <- src
  tmpReg6 <- src
  return $ instSeq [ cspecialrw tmpReg2 30 tmpReg1
                   , modeswcap
                   , jalr tmpReg2 0 0
                   , cspecialrw tmpReg4 30 tmpReg3
                   , cspecialrw tmpReg6 30 tmpReg5 ]

tagCacheTest :: Template
tagCacheTest = random $ do
  addrReg   <- src
  targetReg <- dest
  return $     legalCapStore addrReg
            <> legalCapLoad addrReg targetReg
            <> inst (gctag targetReg targetReg)

genCHERIinspection :: Template
genCHERIinspection = random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  csrAddr  <- frequency [ (1, return (unsafe_csrs_indexFromName "mccsr"))
                        , (1, return (unsafe_csrs_indexFromName "mcause"))
                        , (1, bits 12) ]
  return $ dist [ (1, instUniform $ rv32_xcheri_inspection srcAddr dest)
                , (1, instUniform $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) ] -- TODO add csr

genCHERIarithmetic :: Template
genCHERIarithmetic = random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  csrAddr  <- frequency [ (1, return (unsafe_csrs_indexFromName "mccsr"))
                        , (1, return (unsafe_csrs_indexFromName "mcause"))
                        , (1, bits 12) ]
  return $ dist [ (1, instUniform $ rv32_xcheri_arithmetic srcAddr srcData imm dest)
                , (1, instUniform $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) ] -- TODO add csr

genCHERImisc :: Template
genCHERImisc = random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  srcScr   <- elements [0, 1, 28, 29, 30, 31]
  csrAddr  <- frequency [ (1, return (unsafe_csrs_indexFromName "mccsr"))
                        , (1, return (unsafe_csrs_indexFromName "mcause"))
                        , (1, bits 12) ]
  return $ dist [ (1, instUniform $ rv32_xcheri_misc srcAddr srcData srcScr imm dest)
                , (1, instUniform $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) ] -- TODO add csr

genCHERIcontrol :: Template
genCHERIcontrol = random $ do
  srcAddr  <- src
  srcData  <- src
  dest     <- dest
  imm      <- bits 12
  longImm  <- bits 20
  fenceOp1 <- bits 3
  fenceOp2 <- bits 3
  csrAddr  <- frequency [ (1, return (unsafe_csrs_indexFromName "mccsr"))
                        , (1, return (unsafe_csrs_indexFromName "mcause"))
                        , (1, bits 12) ]
  return $ dist [ (2, instUniform $ rv32_xcheri_control srcAddr srcData dest)
                , (1, inst (scbndsr dest srcData srcAddr))
                , (2, instUniform $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) ] -- TODO add csr
