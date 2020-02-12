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

module QuickCheckVEngine.Templates.Utils.FP (
  fp_prologue
, fp_prologue_length
) where

import InstrCodec
import RISCV
import QuickCheckVEngine.Template

prologue_list :: ArchDesc -> [Integer]
prologue_list arch = [ encode lui 2 1
                     , encode csrrs 0x300 1 0 -- mstatus
                     , encode csrrs 0x003 0 0 -- fcsr
                     ]
                  ++ (if has_f arch || has_d arch then
                     [ encode fmv_w_x 0 0
                     , encode fmv_w_x 0 1
                     , encode fmv_w_x 0 2
                     , encode fmv_w_x 0 3
                     , encode fmv_w_x 0 4
                     --, encode fmv_w_x 0 5
                     --, encode fmv_w_x 0 6
                     --, encode fmv_w_x 0 7
                     --, encode fmv_w_x 0 8
                     --, encode fmv_w_x 0 9
                     --, encode fmv_w_x 0 10
                     --, encode fmv_w_x 0 11
                     --, encode fmv_w_x 0 12
                     --, encode fmv_w_x 0 13
                     --, encode fmv_w_x 0 14
                     --, encode fmv_w_x 0 15
                     , encode op 0 16
                     , encode op 0 17
                     , encode op 0 18
                     , encode op 0 19
                     , encode op 0 20
                     --, encode op 0 21
                     --, encode op 0 22
                     --, encode op 0 23
                     --, encode op 0 24
                     --, encode op 0 25
                     --, encode op 0 26
                     --, encode op 0 27
                     --, encode op 0 28
                     --, encode op 0 29
                     --, encode op 0 30
                     --, encode op 0 31
                     --, encode op 0 32
                     ] else [])
  where op = if has_d arch then fmv_d_x else fmv_w_x

fp_prologue :: ArchDesc -> Template
fp_prologue = instSeq . prologue_list

fp_prologue_length :: ArchDesc -> Int
fp_prologue_length = length . prologue_list
