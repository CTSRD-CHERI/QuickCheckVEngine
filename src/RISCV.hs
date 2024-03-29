--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Matthew Naylor
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
    Module      : RISCV
    Description : A description of the RISC-V instruction encodings
-}

module RISCV (
  module RISCV.ArchDesc
, module RISCV.InstInspect
, module RISCV.RV32_I
, module RISCV.RV32_M
, module RISCV.RV32_A
, module RISCV.RV32_F
, module RISCV.RV32_D
, module RISCV.RV32_Zicsr
, module RISCV.RV32_Zifencei
, module RISCV.RV32_Xcheri
, module RISCV.RV64_I
, module RISCV.RV64_M
, module RISCV.RV64_A
, module RISCV.RV64_F
, module RISCV.RV64_D
, module RISCV.RV_C
, module RISCV.RV_CSRs
, module RISCV.HPMEvents
, PrivMode
, XLen
, Instruction (..)
) where

import RISCV.ArchDesc
import RISCV.Helpers (PrivMode, XLen)
import RISCV.InstInspect
import RISCV.RV32_I
import RISCV.RV32_M
import RISCV.RV32_A
import RISCV.RV32_F
import RISCV.RV32_D
import RISCV.RV32_Zicsr
import RISCV.RV32_Zifencei
import RISCV.RV32_Xcheri
import RISCV.RV64_I
import RISCV.RV64_M
import RISCV.RV64_A
import RISCV.RV64_F
import RISCV.RV64_D
import RISCV.RV_C
import RISCV.RV_CSRs
import RISCV.HPMEvents
import InstrCodec (Instruction (..))
