--
-- SPDX-License-Identifier: BSD-2-Clause
--
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

module QuickCheckVEngine.Templates.GenAtomics (
  gen_rv32_a
, gen_rv64_a
, gen_cheri_a
) where

import Test.QuickCheck
import RISCV
import RISCV.RV32_A
import RISCV.RV64_A
import RISCV.RV32_Xcheri
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils

gen_rv32_a :: Bool -> Template
gen_rv32_a has_cap = genAtomics False has_cap

gen_rv64_a :: Bool -> Template
gen_rv64_a has_cap = genAtomics True has_cap

genAtomics :: Bool -> Bool -> Template
genAtomics has_xlen_64 has_cap = Random $ do
  aq   <- bits 1
  rl   <- bits 1
  src1 <- src
  src2 <- src
  dest <- dest
  let insts = rv32_a src1 src2 dest aq rl
              ++ if has_xlen_64 then rv64_a src1 src2 dest aq rl else []
              ++ if has_cap then rv32_a_xcheri src1 src2 dest else []
  return $ uniformTemplate insts

gen_cheri_a :: Template
gen_cheri_a = Random $ do
  let addrReg = 2
  let dataReg = 1
  addr <- elements [0x80000000, 0x80001000, 0x80004000]
  return $ (NoShrink $ li64 addrReg addr)
           <>
           Sequence [ NoShrink $ Single $ fence_i -- fence
                    , Single $ cload dataReg addrReg 0x14 -- lr.q.ddc
                    , Single $ cstore dataReg addrReg 0x14 -- sc.q.ddc
                    , NoShrink $ Single $ fence_i -- fence
                    , Single $ cload dataReg addrReg 0x17 ] -- lq.ddc
