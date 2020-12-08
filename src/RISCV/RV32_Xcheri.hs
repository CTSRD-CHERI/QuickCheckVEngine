--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Hesham Almatary
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
    Module      : RISCV.RV32_Xcheri
    Description : RISC-V CHERI extension

    The 'RISCV.RV32_Xcheri' module provides the description of the RISC-V CHERI
    extension
-}

module RISCV.RV32_Xcheri (
-- * RISC-V CHERI, instruction definitions
  cgetperm
, cgettype
, cgetbase
, cgetlen
, cgettag
, cgetsealed
, cgetoffset
, cgetflags
, cgetaddr
, cseal
, cunseal
, candperm
, csetflags
, csetoffset
, csetaddr
, cincoffset
, cincoffsetimmediate
, csetbounds
, csetboundsexact
, csetboundsimmediate
, ccleartag
, cbuildcap
, ccopytype
, ccseal
, csealentry
, ctoptr
, cfromptr
, csub
, cmove
, cjalr
, ccall
, ctestsubset
, cspecialrw
, clear
, fpclear
, croundrepresentablelength
, crepresentablealignmentmask
, cload
, cstore
, lq
, sq
-- * RISC-V CHERI, others
, rv32_xcheri_disass
, rv32_xcheri_extract
, rv32_xcheri_shrink
, rv32_xcheri
, rv32_xcheri_inspection
, rv32_xcheri_arithmetic
, rv32_xcheri_misc
, rv32_xcheri_mem
, rv32_xcheri_control
) where

import RISCV.Helpers (reg, int, prettyR, prettyI, prettyL, prettyS, prettyR_2op, ExtractedRegs)
import InstrCodec (DecodeBranch, (-->), encode)
import RISCV.RV32_I

-- Capability Inspection
cgetperm_raw                       =                                        "1111111 00000 cs1[4:0] 000 rd[4:0] 1011011"
cgetperm rd cs1                    = encode cgetperm_raw                                   cs1          rd
cgettype_raw                       =                                        "1111111 00001 cs1[4:0] 000 rd[4:0] 1011011"
cgettype rd cs1                    = encode cgettype_raw                                   cs1          rd
cgetbase_raw                       =                                        "1111111 00010 cs1[4:0] 000 rd[4:0] 1011011"
cgetbase rd cs1                    = encode cgetbase_raw                                   cs1          rd
cgetlen_raw                        =                                        "1111111 00011 cs1[4:0] 000 rd[4:0] 1011011"
cgetlen rd cs1                     = encode cgetlen_raw                                    cs1          rd
cgettag_raw                        =                                        "1111111 00100 cs1[4:0] 000 rd[4:0] 1011011"
cgettag rd cs1                     = encode cgettag_raw                                    cs1          rd
cgetsealed_raw                     =                                        "1111111 00101 cs1[4:0] 000 rd[4:0] 1011011"
cgetsealed rd cs1                  = encode cgetsealed_raw                                 cs1          rd
cgetoffset_raw                     =                                        "1111111 00110 cs1[4:0] 000 rd[4:0] 1011011"
cgetoffset rd cs1                  = encode cgetoffset_raw                                 cs1          rd
cgetflags_raw                      =                                        "1111111 00111 cs1[4:0] 000 rd[4:0] 1011011"
cgetflags rd cs1                   = encode cgetflags_raw                                  cs1          rd
cgetaddr_raw                       =                                        "1111111 01111 cs1[4:0] 000 rd[4:0] 1011011"
cgetaddr rd cs1                    = encode cgetaddr_raw                                   cs1          rd

-- Capability Modification
cseal_raw                          =                                        "0001011 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cseal cd cs1 cs2                   = encode cseal_raw                                cs2      cs1          cd
cunseal_raw                        =                                        "0001100 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cunseal cd cs1 cs2                 = encode cunseal_raw                              cs2      cs1          cd
candperm_raw                       =                                        "0001101 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
candperm cd cs1 rs2                = encode candperm_raw                             rs2      cs1          cd
csetflags_raw                      =                                        "0001110 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetflags cd cs1 rs2               = encode csetflags_raw                            rs2      cs1          cd
csetoffset_raw                     =                                        "0001111 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetoffset cd cs1 rs2              = encode csetoffset_raw                           rs2      cs1          cd
csetaddr_raw                       =                                        "0010000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetaddr cd cs1 rs2                = encode csetaddr_raw                             rs2      cs1          cd
cincoffset_raw                     =                                        "0010001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cincoffset cd cs1 rs2              = encode cincoffset_raw                           rs2      cs1          cd
cincoffsetimmediate_raw            =                                        "imm[11:0] cs1[4:0] 001 cd[4:0] 1011011"
cincoffsetimmediate imm cd cs1     = encode cincoffsetimmediate_raw          imm       cs1          cd
csetbounds_raw                     =                                        "0001000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetbounds cd cs1 rs2              = encode csetbounds_raw                           rs2      cs1          cd
csetboundsexact_raw                =                                        "0001001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csetboundsexact cd cs1 rs2         = encode csetboundsexact_raw                      rs2      cs1          cd
csetboundsimmediate_raw            =                                        "imm[11:0] cs1[4:0] 010 cd[4:0] 1011011"
csetboundsimmediate cd cs1 imm     = encode csetboundsimmediate_raw          imm       cs1          cd
ccleartag_raw                      =                                        "1111111 01011 cs1[4:0] 000 cd[4:0] 1011011"
ccleartag cd cs1                   = encode ccleartag_raw                                  cs1          cd
cbuildcap_raw                      =                                        "0011101 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cbuildcap cd cs1 cs2               = encode cbuildcap_raw                            cs2      cs1          cd
ccopytype_raw                      =                                        "0011110 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccopytype cd cs1 cs2               = encode ccopytype_raw                            cs2      cs1          cd
ccseal_raw                         =                                        "0011111 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ccseal cd cs1 cs2                  = encode ccseal_raw                               cs2      cs1          cd
csealentry_raw                     =                                        "1111111 10001 cs1[4:0] 000 cd[4:0] 1011011"
csealentry cd cs1                  = encode csealentry_raw                                 cs1          cd


-- Capability Pointer Arithmetic
ctoptr_raw                         =                                        "0010010 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
ctoptr cd cs1 cs2                  = encode ctoptr_raw                               cs2      cs1          cd
cfromptr_raw                       =                                        "0010011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cfromptr cd cs1 rs2                = encode cfromptr_raw                             rs2      cs1          cd
csub_raw                           =                                        "0010100 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
csub cd cs1 cs2                    = encode csub_raw                                 cs2      cs1          cd
cmove_raw                          =                                        "1111111 01010 cs1[4:0] 000 cd[4:0] 1011011"
cmove cd cs1                       = encode cmove_raw                                      cs1          cd
cspecialrw_raw                     =                                        "0000001 cSP[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cspecialrw cd cs1 cSP              = encode cspecialrw_raw                           cSP      cs1          cd


-- Control Flow
cjalr_raw                          =                                        "1111111 01100 cs1[4:0] 000 cd[4:0] 1011011"
cjalr cd cs1                       = encode cjalr_raw                                      cs1          cd
ccall_raw                          =                                        "1111110 pcc[4:0] idc[4:0] 000 00001 1011011"
ccall pcc idc                      = encode ccall_raw                                pcc      idc

-- Assertion
ctestsubset_raw                    =                                        "0100000 cs2[4:0] cs1[4:0] 000 rd[4:0] 1011011"
ctestsubset rd cs1 cs2             = encode ctestsubset_raw                          cs2      cs1          rd

-- Register Clearing
clear_raw                          =                                        "1111111 01101 q[1:0] imm[7:5] 000 imm[4:0] 1011011"
clear q imm                        = encode clear_raw                                      q      imm
fpclear_raw                        =                                        "1111111 10000 q[1:0] imm[7:5] 000 imm[4:0] 1011011"
fpclear q imm                      = encode fpclear_raw                                    q      imm

-- Adjusting to Compressed Capability Precision
croundrepresentablelength_raw      =                                        "1111111 01000 rs1[4:0] 000 rd[4:0] 1011011"
croundrepresentablelength rd rs1   = encode croundrepresentablelength_raw                  rs1          rd
crepresentablealignmentmask_raw    =                                        "1111111 01001 rs1[4:0] 000 rd[4:0] 1011011"
crepresentablealignmentmask rd rs1 = encode crepresentablealignmentmask_raw                rs1          rd

-- Memory -- Needs further refinement
cload_raw                          =                                        "1111101 mop[4:0] cb[4:0] 000 cd[4:0] 1011011"
cload cd cb mop                    = encode cload_raw                                mop      cb          cd
cstore_raw                         =                                        "1111100 rs1[4:0] rs2[4:0] 000 mop[4:0] 1011011"
cstore rs1 rs2 mop                 = encode cstore_raw                               rs1      rs2          mop
lq_raw                             =                                        "imm[11:0] rs1[4:0] 010 cd[4:0] 0001111"
lq cd rs1 imm                      = encode lq_raw                           imm       rs1          cd
sq_raw                             =                                        "imm[11:5] cs2[4:0] rs1[4:0] 100 imm[4:0] 0100011"
sq rs1 cs2 imm                     = encode sq_raw                           imm       cs2      rs1

-- | Pretty-print a capability load instruction
prettyCLoad :: Integer -> Integer -> Integer -> String
prettyCLoad mop rs1 rd =
  concat [instr, " ", reg rd, ", ", reg rs1, "[0]"]
    where instr = case mop of 0x00 -> "lb.ddc"
                              0x01 -> "lh.ddc"
                              0x02 -> "lw.ddc"
                              0x03 -> "ld.ddc"
                              0x04 -> "lbu.ddc"
                              0x05 -> "lhu.ddc"
                              0x06 -> "lwu.ddc"
                              0x07 -> "ldu.ddc"  -- TODO clarify meaning...
                              0x08 -> "lb.cap"
                              0x09 -> "lh.cap"
                              0x0a -> "lw.cap"
                              0x0b -> "ld.cap"
                              0x0c -> "lbu.cap"
                              0x0d -> "lhu.cap"
                              0x0e -> "lwu.cap"
                              0x0f -> "ldu.cap"  -- TODO clarify meaning...
                              0x10 -> "lr.b.ddc"
                              0x11 -> "lr.h.ddc"
                              0x12 -> "lr.w.ddc"
                              0x13 -> "lr.d.ddc"
                              0x14 -> "lr.q.ddc" -- TODO only valid in rv64
                              0x15 -> "INVALID"
                              0x16 -> "INVALID"
                              0x17 -> "lq.ddc"   -- TODO only valid in rv64
                              0x18 -> "lr.b.cap"
                              0x19 -> "lr.h.cap"
                              0x1a -> "lr.w.cap"
                              0x1b -> "lr.d.cap"
                              0x1c -> "lr.q.cap" -- TODO only valid in rv64
                              0x1d -> "INVALID"
                              0x1e -> "INVALID"
                              0x1f -> "lq.cap"   -- TODO only valid in rv64
                              _    -> "INVALID"

-- | Pretty-print a capability store instruction
prettyCStore :: Integer -> Integer -> Integer -> String
prettyCStore rs2 rs1 mop =
  concat [instr, " ", reg rs2, ", ", reg rs1, "[0]"]
    where instr = case mop of 0x00 -> "sb.ddc"
                              0x01 -> "sh.ddc"
                              0x02 -> "sw.ddc"
                              0x03 -> "sd.ddc"
                              0x04 -> "sq.ddc"   -- TODO only valid in rv64
                              0x08 -> "sb.cap"
                              0x09 -> "sh.cap"
                              0x0a -> "sw.cap"
                              0x0b -> "sd.cap"
                              0x0c -> "sq.cap"   -- TODO only valid in rv64
                              0x10 -> "sc.b.ddc"
                              0x11 -> "sc.h.ddc"
                              0x12 -> "sc.w.ddc"
                              0x13 -> "sc.d.ddc"
                              0x14 -> "sc.q.ddc" -- TODO only valid in rv64
                              0x18 -> "sc.b.cap"
                              0x19 -> "sc.h.cap"
                              0x1a -> "sc.w.cap"
                              0x1b -> "sc.d.cap"
                              0x1c -> "sc.q.cap" -- TODO only valid in rv64
                              _ -> "INVALID"

-- | Pretty-print a register clear instruction
pretty_reg_clear instr imm qt = concat [instr, " ", int qt, ", ", int imm]

-- | Pretty-print a 2 sources instruction
pretty_2src instr idc pcc = concat [instr, " ", reg pcc, ", ", reg idc]

-- | Pretty-print a special capability read/write instruction
pretty_cspecialrw instr idx cs1 cd =
  concat [instr, " ", reg cd, ", ", name_scr idx, ", ", reg cs1]
  where name_scr 0 = "pcc"
        name_scr 1 = "ddc"
        name_scr 4 = "utcc"
        name_scr 5 = "utdc"
        name_scr 6 = "uscratchc"
        name_scr 7 = "uepcc"
        name_scr 12 = "stcc"
        name_scr 13 = "stdc"
        name_scr 14 = "sscratchc"
        name_scr 15 = "sepcc"
        name_scr 28 = "mtcc"
        name_scr 29 = "mtdc"
        name_scr 30 = "mscratchc"
        name_scr 31 = "mepcc"
        name_scr idx = int idx

-- | Dissassembly of CHERI instructions
rv32_xcheri_disass :: [DecodeBranch String]
rv32_xcheri_disass = [ cgetperm_raw                    --> prettyR_2op "cgetperm"
                     , cgettype_raw                    --> prettyR_2op "cgettype"
                     , cgetbase_raw                    --> prettyR_2op "cgetbase"
                     , cgetlen_raw                     --> prettyR_2op "cgetlen"
                     , cgettag_raw                     --> prettyR_2op "cgettag"
                     , cgetsealed_raw                  --> prettyR_2op "cgetsealed"
                     , cgetoffset_raw                  --> prettyR_2op "cgetoffset"
                     , cgetaddr_raw                    --> prettyR_2op "cgetaddr"
                     , cseal_raw                       --> prettyR "cseal"
                     , cunseal_raw                     --> prettyR "cunseal"
                     , candperm_raw                    --> prettyR "candperm"
                     , csetoffset_raw                  --> prettyR "csetoffset"
                     , csetaddr_raw                    --> prettyR "csetaddr"
                     , cincoffset_raw                  --> prettyR "cincoffset"
                     , csetbounds_raw                  --> prettyR "csetbounds"
                     , csetboundsexact_raw             --> prettyR "csetboundsexact"
                     , cbuildcap_raw                   --> prettyR "cbuildcap"
                     , ccopytype_raw                   --> prettyR "ccopytype"
                     , ccseal_raw                      --> prettyR "ccseal"
                     , csealentry_raw                  --> prettyR_2op "csealentry"
                     , ccleartag_raw                   --> prettyR_2op "ccleartag"
                     , cincoffsetimmediate_raw         --> prettyI "cincoffsetimmediate"
                     , csetboundsimmediate_raw         --> prettyI "csetboundsimmediate"
                     , ctoptr_raw                      --> prettyR "ctoptr"
                     , cfromptr_raw                    --> prettyR "cfromptr"
                     , csub_raw                        --> prettyR "csub"
                     , cspecialrw_raw                  --> pretty_cspecialrw "cspecialrw"
                     , cmove_raw                       --> prettyR_2op "cmove"
                     , cjalr_raw                       --> prettyR_2op "cjalr"
                     , ccall_raw                       --> pretty_2src "ccall"
                     , ctestsubset_raw                 --> prettyR "ctestsubset"
                     , clear_raw                       --> pretty_reg_clear "clear"
                     , fpclear_raw                     --> pretty_reg_clear "fpclear"
                     , croundrepresentablelength_raw   --> prettyR_2op "croundrepresentablelength"
                     , crepresentablealignmentmask_raw --> prettyR_2op "crepresentablealignmentmask"
                     , cload_raw                       --> prettyCLoad
                     , cstore_raw                      --> prettyCStore
                     , cgetflags_raw                   --> prettyR_2op "cgetflags"
                     , csetflags_raw                   --> prettyR "csetflags"
                     , sq_raw                          --> prettyS "sq"
                     , lq_raw                          --> prettyL "lq" ]

extract_cspecialrw :: Integer -> Integer -> Integer -> ExtractedRegs
extract_cspecialrw idx rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode cspecialrw_raw idx y z)

extract_cmove :: Integer -> Integer -> ExtractedRegs
extract_cmove rs1 rd = (True, Nothing, Just rs1, Just rd, \x y z -> encode cmove_raw y z)

extract_ccall :: Integer -> Integer -> ExtractedRegs
extract_ccall rs2 rs1 = (False, Just rs2, Just rs1, Just 31, \x y z -> encode ccall_raw x y)

extract_cstore :: Integer -> Integer -> Integer -> ExtractedRegs
extract_cstore imm rs2 rs1 = (False, Just rs2, Just rs1, Nothing, \x y z -> encode cstore_raw imm x y)

rv32_xcheri_extract :: [DecodeBranch ExtractedRegs]
rv32_xcheri_extract = [ cgetperm_raw                    --> extract_1op cgetperm_raw
                      , cgettype_raw                    --> extract_1op cgettype_raw
                      , cgetbase_raw                    --> extract_1op cgetbase_raw
                      , cgetlen_raw                     --> extract_1op cgetlen_raw
                      , cgettag_raw                     --> extract_1op cgettag_raw
                      , cgetsealed_raw                  --> extract_1op cgetsealed_raw
                      , cgetoffset_raw                  --> extract_1op cgetoffset_raw
                      , cgetflags_raw                   --> extract_1op cgetflags_raw
                      , cgetaddr_raw                    --> extract_1op cgetaddr_raw
                      , cseal_raw                       --> extract_2op cseal_raw
                      , cunseal_raw                     --> extract_2op cunseal_raw
                      , candperm_raw                    --> extract_2op candperm_raw
                      , csetoffset_raw                  --> extract_2op csetoffset_raw
                      , csetaddr_raw                    --> extract_2op csetaddr_raw
                      , cincoffset_raw                  --> extract_2op cincoffset_raw
                      , csetbounds_raw                  --> extract_2op csetbounds_raw
                      , csetboundsexact_raw             --> extract_2op csetboundsexact_raw
                      , cbuildcap_raw                   --> extract_2op cbuildcap_raw
                      , ccopytype_raw                   --> extract_2op ccopytype_raw
                      , ccseal_raw                      --> extract_2op ccseal_raw
                      , csealentry_raw                  --> extract_1op csealentry_raw
                      , ccleartag_raw                   --> extract_1op ccleartag_raw
                      , cincoffsetimmediate_raw         --> extract_imm cincoffsetimmediate_raw
                      , csetboundsimmediate_raw         --> extract_imm csetboundsimmediate_raw
                      , ctoptr_raw                      --> extract_2op ctoptr_raw
                      , cfromptr_raw                    --> extract_2op cfromptr_raw
                      , csub_raw                        --> extract_2op csub_raw
                      , cspecialrw_raw                  --> extract_cspecialrw
                      , cmove_raw                       --> extract_cmove
                      , cjalr_raw                       --> extract_imm cjalr_raw
                      , ccall_raw                       --> extract_ccall
                      , ctestsubset_raw                 --> extract_2op ctestsubset_raw
--                    , clear_raw                       --> noextract -- TODO
--                    , fpclear_raw                     --> noextract -- TODO
                      , croundrepresentablelength_raw   --> extract_1op croundrepresentablelength_raw
                      , crepresentablealignmentmask_raw --> extract_1op crepresentablealignmentmask_raw
                      , cload_raw                       --> extract_imm cload_raw
                      , cstore_raw                      --> extract_cstore
                      , csetflags_raw                   --> extract_2op csetflags_raw
                      , sq_raw                          --> extract_nodst sq_raw
                      , lq_raw                          --> extract_imm lq_raw
                      ]

shrink_cgetperm :: Integer -> Integer -> [Integer]
shrink_cgetperm cs rd = [addi rd 0 0, addi rd 0 0x7ff]

shrink_cgettype :: Integer -> Integer -> [Integer]
shrink_cgettype cs rd = [addi rd 0 0, addi rd 0 6, addi rd 0 0xfff]

shrink_cgetbase :: Integer -> Integer -> [Integer]
shrink_cgetbase cs rd = [addi rd 0 0]

shrink_cgetlen :: Integer -> Integer -> [Integer]
shrink_cgetlen cs rd = [addi rd 0 0, addi rd 0 0xfff, cgetbase rd cs]

shrink_cgettag :: Integer -> Integer -> [Integer]
shrink_cgettag cs rd = [addi rd 0 1, addi rd 0 0]

shrink_cgetsealed :: Integer -> Integer -> [Integer]
shrink_cgetsealed cs rd = [addi rd 0 1, addi rd 0 0, cgettype rd cs]

shrink_cgetoffset :: Integer -> Integer -> [Integer]
shrink_cgetoffset cs rd = [addi rd 0 0, cgetaddr rd cs]

shrink_cgetflags :: Integer -> Integer -> [Integer]
shrink_cgetflags cs rd = [addi rd 0 0, addi rd 0 1]

shrink_cgetaddr :: Integer -> Integer -> [Integer]
shrink_cgetaddr cs rd = [addi rd cs 0]

shrink_cap :: Integer -> Integer -> [Integer]
shrink_cap cs cd = [ecall,
                    cmove cd cs,
                    cgetaddr cd cs,
                    cgetperm cd cs,
                    cgettype cd cs,
                    cgetbase cd cs,
                    cgetlen cd cs,
                    cgettag cd cs,
                    cgetoffset cd cs,
                    cgetflags cd cs
                   ]

shrink_capcap :: Integer -> Integer -> Integer -> [Integer]
shrink_capcap cs2 cs1 cd = (shrink_cap cs2 cd) ++ (shrink_cap cs1 cd)

shrink_capint :: Integer -> Integer -> Integer -> [Integer]
shrink_capint rs cs cd = shrink_cap cs cd

shrink_capimm :: Integer -> Integer -> Integer -> [Integer]
shrink_capimm imm cs cd = shrink_cap cs cd ++ [addi cd 0 imm, addi cd cs imm]

shrink_cmove :: Integer -> Integer -> [Integer]
shrink_cmove cs cd = [cgetaddr cd cs]

shrink_ccall :: Integer -> Integer -> [Integer]
shrink_ccall cs2 cs1 = shrink_capcap cs2 cs1 31

shrink_ctestsubset cs2 cs1 rd = [addi rd 0 0, addi rd 0 1] ++ shrink_capcap cs2 cs1 rd

shrink_cfromptr rs cs cd = [csetoffset cd cs rs] ++ shrink_capint rs cs cd

rv32_xcheri_shrink :: [DecodeBranch [Integer]]
rv32_xcheri_shrink = [ cgetperm_raw                    --> shrink_cgetperm
                     , cgettype_raw                    --> shrink_cgettype
                     , cgetbase_raw                    --> shrink_cgetbase
                     , cgetlen_raw                     --> shrink_cgetlen
                     , cgettag_raw                     --> shrink_cgettag
                     , cgetsealed_raw                  --> shrink_cgetsealed
                     , cgetoffset_raw                  --> shrink_cgetoffset
                     , cgetflags_raw                   --> shrink_cgetflags
                     , cgetaddr_raw                    --> shrink_cgetaddr
                     , cseal_raw                       --> shrink_capcap
                     , cunseal_raw                     --> shrink_capcap
                     , candperm_raw                    --> shrink_capint
                     , csetoffset_raw                  --> shrink_capint
                     , csetaddr_raw                    --> shrink_capint
                     , cincoffset_raw                  --> shrink_capint
                     , csetbounds_raw                  --> shrink_capint
                     , csetboundsexact_raw             --> shrink_capint
                     , cbuildcap_raw                   --> shrink_capcap
                     , ccopytype_raw                   --> shrink_capcap
                     , ccseal_raw                      --> shrink_capcap
                     , csealentry_raw                  --> shrink_cap
                     , ccleartag_raw                   --> shrink_cap
                     , cincoffsetimmediate_raw         --> shrink_capimm
                     , csetboundsimmediate_raw         --> shrink_capimm
                     , ctoptr_raw                      --> shrink_capcap
                     , cfromptr_raw                    --> shrink_cfromptr
                     , csub_raw                        --> shrink_capcap
--                   , cspecialrw_raw                  --> noshrink
                     , cmove_raw                       --> shrink_cmove
--                   , cjalr_raw                       --> noshrink
                     , ccall_raw                       --> shrink_ccall
                     , ctestsubset_raw                 --> shrink_ctestsubset
--                   , clear_raw                       --> noshrink
--                   , fpclear_raw                     --> noshrink
--                   , croundrepresentablelength_raw   --> noshrink
--                   , crepresentablealignmentmask_raw --> noshrink
--                   , cload_raw                       --> noshrink
--                   , cstore_raw                      --> noshrink
                     , csetflags_raw                   --> shrink_capcap
--                   , sq_raw                          --> noshrink
--                   , lq_raw                          --> noshrink
                     ]

-- | List of cheri inspection instructions
rv32_xcheri_inspection :: Integer -> Integer -> [Integer]
rv32_xcheri_inspection src dest = [ cgetperm                    dest src
                                  , cgettype                    dest src
                                  , cgetbase                    dest src
                                  , cgetlen                     dest src
                                  , cgettag                     dest src
                                  , cgetsealed                  dest src
                                  , cgetoffset                  dest src
                                  , cgetaddr                    dest src
                                  , cgetflags                   dest src
                                  , croundrepresentablelength   dest src
                                  , crepresentablealignmentmask dest src]

-- | List of cheri arithmetic instructions
rv32_xcheri_arithmetic :: Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_arithmetic src1 src2 imm dest =
  [ csetoffset          dest src1 src2
  , csetaddr            dest src1 src2
  , cincoffset          dest src1 src2
  , csetbounds          dest src1 src2
  , csetboundsexact     dest src1 src2
  , csetboundsimmediate dest src1      imm
  , cincoffsetimmediate dest src1      imm
  , ctoptr              dest src1 src2
  , cfromptr            dest src1 src2
  , csub                dest src1 src2
  , ctestsubset         dest src1 src2 ]

-- | List of cheri miscellaneous instructions
rv32_xcheri_misc :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_misc src1 src2 srcScr imm dest =
  [ cseal             dest src1 src2
  , cunseal           dest src1 src2
  , candperm          dest src1 src2
  , cbuildcap         dest src1 src2
  , csetflags         dest src1 src2
  , ccopytype         dest src1 src2
  , ccseal            dest src1 src2
  , csealentry        dest src1
  , ccleartag         dest src1
  , cspecialrw srcScr dest src1      ]

-- | List of cheri control instructions
rv32_xcheri_control :: Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_control src1 src2 dest = [ cjalr dest src1
                                     , ccall      src1 src2 ]

-- | List of cheri memory instructions
rv32_xcheri_mem :: Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri_mem srcAddr srcData imm mop dest =
  [ cload  dest srcAddr         mop
  , cstore      srcAddr srcData mop
  --, ld     dest srcAddr dest        imm
  --, sd          srcAddr srcData     imm
  --, lq     dest srcAddr dest        imm
  --, sq          srcAddr srcData     imm
  ]

-- | List of cheri instructions
rv32_xcheri :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
rv32_xcheri src1 src2 srcScr imm mop dest =
     rv32_xcheri_inspection src1 dest
  ++ rv32_xcheri_arithmetic src1 src2 imm dest
  ++ rv32_xcheri_misc src1 src2 srcScr imm dest
  ++ rv32_xcheri_control src1 src2 dest
  ++ rv32_xcheri_mem src1 src2 imm mop dest
