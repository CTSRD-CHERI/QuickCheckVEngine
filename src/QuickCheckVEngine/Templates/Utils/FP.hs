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
) where

import InstrCodec
import RISCV
import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General

fp_prologue :: Template -> Template
fp_prologue t = readParams $ \p ->
    if has_f (archDesc p) || has_d (archDesc p)
    then shrinkScope ((noShrink . mconcat) [ inst $ lui 1 2
                                           , csrs (unsafe_csrs_indexFromName "mstatus") 1
                                           , csrs (unsafe_csrs_indexFromName "fcsr") 0
                                           , mconcat $ [inst $ fmv_w_x i 0 | i <- [0..4]]
                                                    ++ [inst $ (if has_d (archDesc p) then fmv_d_x else fmv_w_x) i 0 | i <- [16..20]]
                                           ] <> t)
    else t
