--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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
  buildCapTest,
  randomCHERITest
) where

import Test.QuickCheck
import Control.Monad
import RISCV
import InstrCodec
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils
import Data.Bits

buildCapTest :: ArchDesc -> Template
buildCapTest arch = Random $ do
  let bitAppend x (a,b) = (shift x b +) <$> a b
  cap <- oneof [bits 128, foldM bitAppend 0 [(bits,16),(bits,3),(const $ elements [0x00000,0x00001,0x00002,0x00003],18),(bits,27),(bits,64)]]
  return $ Sequence [Single $ encode lui 0x40004 1,
                     Single $ encode slli 1 1 1,
                     li32 2 (cap Data.Bits..&. 0xffffffff),
                     Single $ encode sw 0 2 1,
                     li32 2 ((shift cap (-32)) Data.Bits..&. 0xffffffff),
                     Single $ encode sw 4 2 1,
                     li32 2 ((shift cap (-64)) Data.Bits..&. 0xffffffff),
                     Single $ encode sw 8 2 1,
                     li32 2 ((shift cap (-96)) Data.Bits..&. 0xffffffff),
                     Single $ encode sw 12 2 1,
                     Single $ encode lq 0 1 2,
                     Single $ encode cbuildcap 2 3 2,
                     Single $ encode cgettype 2 4,
                     Single $ encode cgettag 2 5]


genRandomCHERITest :: ArchDesc -> Template
genRandomCHERITest arch = Random $ do
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
  csrAddr   <- frequency [(1, return 0xbc0), (1, return 0x342)]
  srcScr    <- elements $ [0, 1, 28, 29, 30, 31] ++ (if has_s arch then [12, 13, 14, 15] else []) ++ [2]
  srcCsr    <- elements [0x141, 0x142, 0x341, 0x342]
  return $ Distribution [
                          (5, legalLoad arch)
                        , (5, legalStore arch)
                        , (5, legalCapLoad srcAddr dest)
                        , (5, legalCapStore srcAddr)
                        , (10, uniformTemplate $ rv32_i srcAddr srcData dest imm longImm fenceOp1 fenceOp2) -- TODO add csr
                        , (10, uniformTemplate $ rv32_xcheri srcAddr srcData srcScr imm mop dest)
                        , (10, Single $ encode cspecialrw srcScr srcAddr dest)
                        , (10, uniformTemplate $ rv32_zicsr srcData dest srcCsr mop)
                        , (10, switchEncodingMode)
                        , (10, cspecialRWChain)
                        , (10, randomCCall srcAddr srcData tmpReg tmpReg2)
                        , (10, makeShortCap)
                        , (10, clearASR tmpReg tmpReg2)
                        ]

randomCHERITest :: ArchDesc -> Template
randomCHERITest arch = let temp = repeatTemplateTillEnd $ genRandomCHERITest arch
                       in if has_f arch || has_d arch then NoShrink (fp_prologue arch) <> temp
                                                      else temp
