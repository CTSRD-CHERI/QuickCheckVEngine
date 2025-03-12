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

module QuickCheckVEngine.Templates.GenCHERI (
  capDecodeTest,
  randomCHERITest,
  randomCHERIRVCTest,
  genCHERIinspection,
  genCHERIarithmetic,
  genCHERImisc,
  genCHERIcontrol
) where

import Test.QuickCheck
import Control.Monad
import RISCV
import InstrCodec
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.CHERI
import QuickCheckVEngine.Templates.Utils.FP
import QuickCheckVEngine.Templates.Utils.General
import QuickCheckVEngine.Templates.GenArithmetic
import QuickCheckVEngine.Templates.GenFP
import QuickCheckVEngine.Templates.GenCompressed
import Data.Bits

capDecodeTest :: Template
capDecodeTest = random $ do
  let bitAppend x (a,b) = (shift x b +) <$> a b
  cap <- oneof [bits 128, -- completely random cap
                foldM bitAppend 0 [(bits,16),(bits,3),(const $ elements [0x00000,0x00001,0x00002,0x00003],18),(bits,27),(bits,64)], -- reserved otypes
                choose(40,63) >>= \exp -> foldM bitAppend 0 [(bits,16),(bits,3),(bits,18),(const $ return 1,1),(bits,9),(const $ return $ shift exp (-3),3),(bits,11),(const $ return $ exp Data.Bits..&. 0x3,3),(bits,64)] -- tricky exponents
                ]
  return $ mconcat [inst $ lui 1 0x40004,
                    inst $ slli 1 1 1,
                    li32 2 (cap Data.Bits..&. 0xffffffff),
                    inst $ sw 1 2 0,
                    li32 2 ((shift cap (-32)) Data.Bits..&. 0xffffffff),
                    inst $ sw 1 2 4,
                    li32 2 ((shift cap (-64)) Data.Bits..&. 0xffffffff),
                    inst $ sw 1 2 8,
                    li32 2 ((shift cap (-96)) Data.Bits..&. 0xffffffff),
                    inst $ sw 1 2 12,
                    inst $ lc 2 1 0,
                    inst $ gclen 6 2,
                    inst $ gcbase 6 2,
                    inst $ gchigh 6 2,
                    inst $ gctype 6 2,
                    inst $ gcperm 6 2,
                    inst $ cbld 2 3 2,
                    inst $ gctype 4 2,
                    inst $ gctag 5 2]


genRandomCHERITest :: Template
genRandomCHERITest = readParams $ \param -> random $ do
  let arch = archDesc param
  srcAddr   <- src
  srcData   <- src
  tmpReg    <- src
  tmpReg2   <- src
  dest      <- dest
  imm       <- bits 12
  mop       <- elements [ 0x0, 0x1, 0x2, 0x3, 0x4, 0x5, 0x6, 0x7,
                          0x8, 0x9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf,
                          -- Skip LR,SC since these have non-determinism which is problematic for TestRIG
                          0x17,0x1f]
  longImm   <- (bits 20)
  fenceOp1  <- (bits 4)
  fenceOp2  <- (bits 4)
  uimm5     <- (bits 5)
  csrAddr   <- frequency [ (1, return (unsafe_csrs_indexFromName "mcause"))
                         , (1, return (unsafe_csrs_indexFromName "mseccfg"))
                         , (1, return (unsafe_csrs_indexFromName "scause"))
                         , (1, return (unsafe_csrs_indexFromName "mseccfg"))
                         , (1, return (unsafe_csrs_indexFromName "menvcfg"))
                         , (1, return (unsafe_csrs_indexFromName "senvcfg"))
                         , (1, return (unsafe_csrs_indexFromName "mepc"))
                         , (1, return (unsafe_csrs_indexFromName "sepc"))
                         , (1, return (unsafe_csrs_indexFromName "mtvec"))
                         , (1, return (unsafe_csrs_indexFromName "stvec"))
                         , (1, return (unsafe_csrs_indexFromName "mscratch"))
                         , (1, return (unsafe_csrs_indexFromName "sscratch"))
                         ]
  return $ dist [ (5, legalLoad)
                , (5, legalStore)
                , (5, legalCapLoad srcAddr dest)
                , (5, legalCapStore srcAddr)
                , (10, instUniform $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2)
                , (10, instUniform $ rv32_xcheri arch srcAddr srcData imm dest)
                , (5, instUniform $ rv32_zicsr srcData dest csrAddr uimm5)
                , (10, switchEncodingMode)
                , (10, cspecialRWChain)
                , (10, makeShortCap)
                , (5, clearASR tmpReg tmpReg2)
                , (5, boundPCC tmpReg tmpReg2 imm longImm)
                , (20, inst $ gctag dest dest)
                ]

randomCHERIRVCTest :: Template
randomCHERIRVCTest = random $ do
  rvcInst <- bits 16
  return $ mconcat [ switchEncodingMode
                   , genRandomCHERITest
                   , uniform [inst $ MkInstruction rvcInst, gen_rv_c]
                   , repeatN 5 genCHERIinspection
                   ]

randomCHERITest :: Template
randomCHERITest = fp_prologue $ repeatTillEnd genRandomCHERITest

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
  return $ dist [ (1, instUniform $ rv32_xcheri_misc srcAddr srcData imm dest)
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
