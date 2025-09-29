--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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

module QuickCheckVEngine.Templates.GenCSRs (
  gen_rv32_i_zicsr
, gen_hpm_violation
) where

--import InstrCodec
import RISCV
import RISCV.RV32_I
import RISCV.RV32_Zicsr
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General

{-
gen_rv32_i_zicsr :: Template
gen_rv32_i_zicsr = readParams $ \param -> random $
  do any_csr   <- csr $ csrFilter param
     --valid_csr <- csr
     uimm      <- bits 5
     src1      <- src
     dest      <- dest
     -- TODO mix csr instructions with some i instructions
     let insts = maybe mempty (\idx -> rv32_zicsr src1 dest idx uimm) any_csr
     return $ instUniform insts
-}

gen_rand_csr :: Template
gen_rand_csr = readParams $ \param -> random $
  do any_csr   <- bits 12
     uimm      <- bits 5
     src1      <- src
     dest      <- dest
     -- TODO mix csr instructions with some i instructions
     let insts = maybe mempty (\idx -> rv32_zicsr src1 dest idx uimm) (Just any_csr)
--                 ++ rv32_i_exc
     return $ instUniform insts

{-
gen_rv32_i_zicsr :: Template
gen_rv32_i_zicsr =
    shrinkScope ((noShrink . mconcat) [    inst $ auipc 17 0
					   , inst $ csrrw 0 (unsafe_csrs_indexFromName "mepc") 17
                   			   , inst $ addi 9 0 7
                   			   , inst $ csrrw 0  (unsafe_csrs_indexFromName "mcounteren") 9
                   			   , inst $ csrrw 0  (unsafe_csrs_indexFromName "scounteren") 9
                   			   --, inst $ csrrw 9 (unsafe_csrs_indexFromName "scounteren") 0
					   , inst $ mret
					   , inst $ add 0 0 0
					   , inst $ csrrs 1 (unsafe_csrs_indexFromName "minstret") 0
                                      ])
-}

{-
gen_hpm_violation :: Template
gen_hpm_violation = random $ do
  let a0 = 10
  let t0 = 6
  return $ mconcat [ li64 a0 0xEFFFF
                   , inst $ modeswcap
                   , inst $ auipc t0 0
                   , inst $ acperm t0 t0 a0
                   , inst $ csrrw 11 (unsafe_csrs_indexFromName "mepc") t0
                   , inst $ jalr 0 t0 0
                   , gen_rand_csr
                   ]
-}

gen_hpm_violation :: Template
gen_hpm_violation = random $ do
  let a0 = 10
  let t0 = 6
  return $ mconcat [ changePrivMode
                   , inst $ add 1 2 3
                   ]

setUpPageTable :: Template
setUpPageTable = random $ do
  let a0 = 10
  let t0 = 6
  return $ mconcat [ li64 a0 0x80002000
                   , li64 t0 0x20000c01
                   , inst $ sd a0 t0 0
                   , li64 t0 0x20000801
                   , inst $ sd a0 t0 16
                   , li64 a0 0x80003000
                   , li64 t0 0x2000004b
                   , inst $ sd a0 t0 0
                   , li64 t0 0x20000447
                   , inst $ sd a0 t0 8
                   , li64 t0 0x2000105b
                   , inst $ sd a0 t0 32
                   , li64 t0 0x20001457
                   , inst $ sd a0 t0 40
                   ]

gen_rv32_i_zicsr :: Template
gen_rv32_i_zicsr = random $ do
  let s0 = 8
  let s1 = 9
  let s2 = 18
  let counterReg = 30
  let hpmCntIdx = 3
  let evt = 0x31
  let mstatus = unsafe_csrs_indexFromName "mstatus"
  let sstatus = unsafe_csrs_indexFromName "sstatus"
  let mepc = unsafe_csrs_indexFromName "mepc"
  let sepc = unsafe_csrs_indexFromName "sepc"
  let satp = unsafe_csrs_indexFromName "satp"
  let medeleg = unsafe_csrs_indexFromName "medeleg"
  let mcounteren = unsafe_csrs_indexFromName "mcounteren"
  let scounteren = unsafe_csrs_indexFromName "scounteren"
  let sedeleg = unsafe_csrs_indexFromName "sedeleg"
  let stval = unsafe_csrs_indexFromName "stval"
  return $ mconcat [ inst $ lui s1 0x100
                   ,        csrc mstatus s1
                   , inst $ addi s1 0 7
                   ,        csrs mcounteren s1
                   --, inst $ csrrw s1 mcounteren 0
                   --, inst $ csrrw s1 scounteren 0
                   , inst $ lui s1 0x1
                   ,        csrc mstatus s1
                   --, inst $ lui s1 0x1
                   , inst $ addi s1 0 1
                   , inst $ slli s1 s1 11
                   ,        csrs mstatus s1
                   , inst $ auipc s2 0
                   , inst $ addi s2 s2 16
                   ,        csrw mepc s2
                   , inst $ lui s2 0xa
                   ,        csrw medeleg s2
                   --,        setUpPageTable
                   , inst $ csrrw 1 mstatus 0
                   , inst $ mret
                   , inst $ add 0 0 0
                   , inst $ csrrs 1 (unsafe_csrs_indexFromName "instret") 0
                   ]

