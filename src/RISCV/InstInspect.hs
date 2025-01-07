--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2019-2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
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
    Module      : RISCV.InstInspect
    Description : Inspection helper for RISC-V instructions

    The 'RISCV.InstInspect' module provides a function for RISC-V instruction
    pretty-printing, extracting encoded fields, and providing shrunk versions
    of the instructions.
-}

module RISCV.InstInspect (
  rv_pretty,
  rv_shrink,
  rv_extract
) where

import InstrCodec

import RISCV.RV32_I
import RISCV.RV32_M
import RISCV.RV32_A
import RISCV.RV32_F
import RISCV.RV32_D
import RISCV.RV32_Zicsr
import RISCV.RV32_Zifencei
import RISCV.RV32_Zcheri
import RISCV.RV64_I
import RISCV.RV64_M
import RISCV.RV64_A
import RISCV.RV64_F
import RISCV.RV64_D
import RISCV.RV_C
import RISCV.Helpers

import Text.Printf

-- | RISC-V instruction pretty printer
rv_pretty :: Instruction -> Maybe XLen-> String
rv_pretty instr ixl = case decode 32 instr instList of
  Nothing -> "Unknown instruction"
  Just i -> i
  where instList =    rv32_i_disass ++ rv64_i_disass
                   ++ rv32_m_disass ++ rv64_m_disass
                   ++ rv32_a_disass ++ rv64_a_disass
                   ++ rv32_f_disass ++ rv64_f_disass
                   ++ rv32_d_disass ++ rv64_d_disass
                   ++ rv32_zicsr_disass
                   ++ rv32_zifencei_disass
                   ++ rv32_xcheri_disass
                   ++ rv_c_disass ixl

instance Show Instruction where
  show i@(MkInstruction v) = printf ".4byte 0x%08x # %s" v (rv_pretty i Nothing)

rv_extract :: Instruction -> ExtractedRegs
rv_extract instr = case decode 32 instr extractList of
  Nothing -> (False, Nothing, Nothing, Nothing, (\a b c -> instr))
  Just i -> i
  where extractList = rv32_i_extract ++ rv32_xcheri_extract

rv_shrink :: Instruction -> [Instruction]
rv_shrink instr = case decode 32 instr shrinkList of
  Nothing -> []
  Just i -> i
  where shrinkList = rv32_i_shrink ++ rv32_xcheri_shrink
