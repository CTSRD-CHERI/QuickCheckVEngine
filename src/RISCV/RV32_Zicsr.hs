--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2020 Peter Rugg
-- Copyright (c) 2019-2020 Alexandre Joannou
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

{-|
    Module      : RISCV.RV32_Zicsr
    Description : RISC-V control and status register instructions extension

    The 'RISCV.RV32_Zicsr' module provides the description of the RISC-V
    control and status register instructions extension
-}

module RISCV.RV32_Zicsr (
-- * RISC-V control and status register, instruction definitions
  csrrw
, csrrs
, csrrc
, csrrwi
, csrrsi
, csrrci
-- * RISC-V control and status register, others
, rv32_zicsr_disass
, rv32_zicsr
) where

import RISCV.Helpers (prettyCSR, prettyCSR_imm)
import InstrCodec (DecodeBranch, (-->), encode)

csrrw_raw          =                   "imm[11:0]  rs1[4:0] 001 rd[4:0] 1110011"
csrrw rd rs1 imm   = encode csrrw_raw   imm        rs1          rd
csrrs_raw          =                   "imm[11:0]  rs1[4:0] 010 rd[4:0] 1110011"
csrrs rd rs1 imm   = encode csrrs_raw   imm        rs1          rd
csrrc_raw          =                   "imm[11:0]  rs1[4:0] 011 rd[4:0] 1110011"
csrrc rd rs1 imm   = encode csrrc_raw   imm        rs1          rd
csrrwi_raw         =                   "imm[11:0] uimm[4:0] 101 rd[4:0] 1110011"
csrrwi rd uimm imm = encode csrrwi_raw  imm       uimm          rd
csrrsi_raw         =                   "imm[11:0] uimm[4:0] 110 rd[4:0] 1110011"
csrrsi rd uimm imm = encode csrrsi_raw  imm       uimm          rd
csrrci_raw         =                   "imm[11:0] uimm[4:0] 111 rd[4:0] 1110011"
csrrci rd uimm imm = encode csrrci_raw  imm       uimm          rd

-- | Dissassembly of RISC-V control and status register instructions
rv32_zicsr_disass :: [DecodeBranch String]
rv32_zicsr_disass = [ csrrw_raw  --> prettyCSR     "csrrw"
                    , csrrs_raw  --> prettyCSR     "csrrs"
                    , csrrc_raw  --> prettyCSR     "csrrc"
                    , csrrwi_raw --> prettyCSR_imm "csrrwi"
                    , csrrsi_raw --> prettyCSR_imm "csrrsi"
                    , csrrci_raw --> prettyCSR_imm "csrrci" ]

-- | List of RISC-V control and status register instructions
rv32_zicsr :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_zicsr src dest imm uimm = [ csrrw  dest src  imm
                               , csrrs  dest src  imm
                               , csrrc  dest src  imm
                               , csrrwi dest uimm imm
                               , csrrsi dest uimm imm
                               , csrrci dest uimm imm ]
