--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019, 2020 Alexandre Joannou
-- Copyright (c) 2019-2020 Peter Rugg
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
prologue_list arch = [ lui 1 2
                     , csrrs 0 0x300 1 -- mstatus
                     , csrrs 0 0x003 0 -- fcsr
                     ]
                  ++ (if has_f arch || has_d arch then
                     [ fmv_w_x 0 0
                     , fmv_w_x 1 0
                     , fmv_w_x 2 0
                     , fmv_w_x 3 0
                     , fmv_w_x 4 0
                     --, fmv_w_x 5  0
                     --, fmv_w_x 6  0
                     --, fmv_w_x 7  0
                     --, fmv_w_x 8  0
                     --, fmv_w_x 9  0
                     --, fmv_w_x 10 0
                     --, fmv_w_x 11 0
                     --, fmv_w_x 12 0
                     --, fmv_w_x 13 0
                     --, fmv_w_x 14 0
                     --, fmv_w_x 15 0
                     , op 16 0
                     , op 17 0
                     , op 18 0
                     , op 19 0
                     , op 20 0
                     --, op 21 0
                     --, op 22 0
                     --, op 23 0
                     --, op 24 0
                     --, op 25 0
                     --, op 26 0
                     --, op 27 0
                     --, op 28 0
                     --, op 29 0
                     --, op 30 0
                     --, op 31 0
                     --, op 32 0
                     ] else [])
  where op = if has_d arch then fmv_d_x else fmv_w_x

fp_prologue :: ArchDesc -> Template
fp_prologue = instSeq . prologue_list

fp_prologue_length :: ArchDesc -> Int
fp_prologue_length = length . prologue_list
