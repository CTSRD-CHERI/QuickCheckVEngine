--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2018 Jonathan Woodruff
-- Copyright (c) 2018 Hesham Almatary
-- Copyright (c) 2018 Matthew Naylor
-- Copyright (c) 2019-2020 Alexandre Joannou
-- Copyright (c) 2020 Peter Rugg
-- Copyright (c) 2021-2022 Franz Fuchs
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
-- This software was developed by the University of  Cambridge
-- Department of Computer Science and Technology under the
-- SIPP (Secure IoT Processor Platform with Remote Attestation)
-- project funded by EPSRC: EP/S030868/1
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
    Module      : RISCV.RV32_Zcheri
    Description : RISC-V CHERI extension

    The 'RISCV.RV32_Zcheri' module provides the description of the RISC-V CHERI
    extension
-}

module RISCV.RV32_Zcheri (
-- * RISC-V CHERI, instruction definitions
  gcperm
, gctype
, gcbase
, gclen
, gctag
, gchigh
, gcmode
, acperm
, scmode
, scaddr
, schi
, cadd
, caddi
, scbndsr
, scbnds
, scbndsi
, cbld
, sentry
, cmv
, modeswcap
, modeswint
, ctestsubset
, cspecialrw
, crepresentablealignmentmask
, cload
, cstore
, lq
, sq
, lr_q
, sc_q
, amoswap_q
-- * RISC-V CHERI, others
, rv32_xcheri_disass
, rv32_xcheri_extract
, rv32_xcheri_shrink
, rv32_xcheri
, rv32_xcheri_inspection
, rv32_xcheri_arithmetic
, rv32_xcheri_misc
, rv32_xcheri_mem
, rv32_a_xcheri
, rv32_xcheri_control
) where

import RISCV.Helpers (reg, int, prettyR, prettyI, prettyL, prettyS, prettyR_2op, prettyR_A_1op, prettyR_A, ExtractedRegs)
import InstrCodec (DecodeBranch, (-->), encode, Instruction)
import RISCV.RV32_I
import RISCV.ArchDesc

-- Capability Inspection
gcperm_raw                       =                                        "0001000 00001 cs1[4:0] 000 rd[4:0] 0110011"
gcperm rd cs1                    = encode gcperm_raw                                   cs1          rd
gctype_raw                       =                                        "0001000 00010 cs1[4:0] 000 rd[4:0] 0110011"
gctype rd cs1                    = encode gctype_raw                                   cs1          rd
gcbase_raw                       =                                        "0001000 00101 cs1[4:0] 000 rd[4:0] 0110011"
gcbase rd cs1                    = encode gcbase_raw                                   cs1          rd
gclen_raw                        =                                        "0001000 00110 cs1[4:0] 000 rd[4:0] 0110011"
gclen rd cs1                     = encode gclen_raw                                    cs1          rd
gctag_raw                        =                                        "0001000 00000 cs1[4:0] 000 rd[4:0] 0110011"
gctag rd cs1                     = encode gctag_raw                                    cs1          rd
gchigh_raw                       =                                        "0001000 00100 cs1[4:0] 000 rd[4:0] 0110011"
gchigh rd cs1                    = encode gchigh_raw                                   cs1          rd
gcmode_raw                       =                                        "0001000 00011 cs1[4:0] 000 rd[4:0] 0110011"
gcmode rd cs1                    = encode gcmode_raw                                   cs1          rd

-- Capability Modification
acperm_raw                         =                                        "0000110 rs2[4:0] cs1[4:0] 010 cd[4:0] 0110011"
acperm cd cs1 rs2                  = encode acperm_raw                               rs2      cs1          cd
scmode_raw                         =                                        "0000110 rs2[4:0] cs1[4:0] 111 cd[4:0] 0110011"
scmode cd cs1 rs2                  = encode scmode_raw                               rs2      cs1          cd
scaddr_raw                         =                                        "0000110 rs2[4:0] cs1[4:0] 001 cd[4:0] 0110011"
scaddr cd cs1 rs2                  = encode scaddr_raw                               rs2      cs1          cd
schi_raw                           =                                        "0000110 rs2[4:0] cs1[4:0] 011 cd[4:0] 0110011"
schi cd cs1 rs2                    = encode schi_raw                                 rs2      cs1          cd
cadd_raw                           =                                        "0000110 rs2[4:0] cs1[4:0] 000 cd[4:0] 0110011"
cadd cd cs1 rs2                    = encode cadd_raw                                 rs2      cs1          cd
caddi_raw                          =                                        "imm[11:0] cs1[4:0] 010 cd[4:0] 0011011"
caddi cd cs1 imm                   = encode caddi_raw                       imm        cs1          cd
scbndsr_raw                        =                                        "0001000 rs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
scbndsr cd cs1 rs2                 = encode scbndsr_raw                              rs2      cs1          cd
scbnds_raw                         =                                        "0000111 rs2[4:0] cs1[4:0] 000 cd[4:0] 0110011"
scbnds cd cs1 rs2                  = encode scbnds_raw                      rs2      cs1          cd
scbndsi_raw                        =                                        "000001 s[0:0] imm[4:0] cs1[4:0] 101 cd[4:0] 0010011"
scbndsi cd cs1 s imm               = encode scbndsi_raw                             s      imm      cs1          cd
cbld_raw                           =                                        "0011101 cs2[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cbld cd cs1 cs2                    = encode cbld_raw                                 cs2      cs1          cd
sentry_raw                         =                                        "0001000 01000 cs1[4:0] 000 cd[4:0] 0110011"
sentry cd cs1                      = encode sentry_raw                                     cs1          cd


-- Capability Pointer Arithmetic
cmv_raw                            =                                        "0000110 00000 cs1[4:0] 000 cd[4:0] 0110011"
cmv cd cs1                         = encode cmv_raw                                        cs1          cd
cspecialrw_raw                     =                                        "0000001 cSP[4:0] cs1[4:0] 000 cd[4:0] 1011011"
cspecialrw cd cSP cs1              = encode cspecialrw_raw                           cSP      cs1          cd


-- Control Flow
modeswcap_raw                      =                                        "0001001 00000 00000 001 00000 0110011"
modeswcap                          = encode modeswcap_raw
modeswint_raw                      =                                        "0001010 00000 00000 001 00000 0110011"
modeswint                          = encode modeswint_raw

-- Assertion
ctestsubset_raw                    =                                        "0100000 cs2[4:0] cs1[4:0] 000 rd[4:0] 1011011"
ctestsubset rd cs1 cs2             = encode ctestsubset_raw                          cs2      cs1          rd

-- Adjusting to Compressed Capability Precision
crepresentablealignmentmask_raw    =                                        "1111111 01001 rs1[4:0] 000 rd[4:0] 1011011"
crepresentablealignmentmask rd rs1 = encode crepresentablealignmentmask_raw                rs1          rd

-- Memory -- Needs further refinement
cload_raw                          =                                        "1111101 mop[4:0] cb[4:0] 000 cd[4:0] 1011011"
cload cd cb mop                    = encode cload_raw                                mop      cb          cd
cstore_raw                         =                                        "1111100 rs2[4:0] cs1[4:0] 000 mop[4:0] 1011011"
cstore rs2 cs1 mop                 = encode cstore_raw                               rs2      cs1          mop
lq_raw                             =                                        "imm[11:0] rs1[4:0] 010 cd[4:0] 0001111"
lq cd rs1 imm                      = encode lq_raw                           imm       rs1          cd
sq_raw                             =                                        "imm[11:5] cs2[4:0] rs1[4:0] 100 imm[4:0] 0100011"
sq rs1 cs2 imm                     = encode sq_raw                           imm       cs2      rs1
lr_q_raw                           =                                        "00010 aq[0] rl[0]    00000 rs1[4:0] 100 rd[4:0] 0101111"
lr_q rd rs1 aq rl                  = encode lr_q_raw                               aq    rl             rs1          rd
sc_q_raw                           =                                        "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 100 rd[4:0] 0101111"
sc_q rd rs1 rs2 aq rl              = encode sc_q_raw                               aq    rl    rs2      rs1          rd
amoswap_q_raw                      =                                        "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 100 rd[4:0] 0101111"
amoswap_q rd rs1 rs2 aq rl         = encode amoswap_q_raw                               aq    rl    rs2      rs1          rd

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

-- | Pretty-print a 2 sources instruction
pretty_2src instr src2 src1 = concat [instr, " ", reg src1, ", ", reg src2]

-- | Pretty-print a special capability read/write instruction
pretty_cspecialrw instr idx cs1 cd =
  concat [instr, " ", reg cd, ", ", name_scr idx, ", ", reg cs1]
  where name_scr 0 = "pcc"
        name_scr 1 = "ddc"
        name_scr 3 = "utidc"
        name_scr 4 = "utcc"
        name_scr 5 = "utdc"
        name_scr 6 = "uscratchc"
        name_scr 7 = "uepcc"
        name_scr 11 = "stidc"
        name_scr 12 = "stcc"
        name_scr 13 = "stdc"
        name_scr 14 = "sscratchc"
        name_scr 15 = "sepcc"
        name_scr 27 = "mtidc"
        name_scr 28 = "mtcc"
        name_scr 29 = "mtdc"
        name_scr 30 = "mscratchc"
        name_scr 31 = "mepcc"
        name_scr idx = int idx

-- | Scaled I-type instruction pretty printer
pretty_scbndsi instr s imm cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1, ", ", int s, ", ", int imm]

-- | Dissassembly of CHERI instructions
rv32_xcheri_disass :: [DecodeBranch String]
rv32_xcheri_disass = [ gcperm_raw                      --> prettyR_2op "gcperm"
                     , gctype_raw                      --> prettyR_2op "gctype"
                     , gcbase_raw                      --> prettyR_2op "gcbase"
                     , gclen_raw                       --> prettyR_2op "gclen"
                     , gctag_raw                       --> prettyR_2op "gctag"
                     , gchigh_raw                      --> prettyR_2op "gchigh"
                     , gcmode_raw                      --> prettyR_2op "gcmode"
                     , acperm_raw                      --> prettyR "acperm"
                     , scaddr_raw                      --> prettyR "csetaddr"
                     , schi_raw                        --> prettyR "schi"
                     , cadd_raw                        --> prettyR "cadd"
                     , scbndsr_raw                     --> prettyR "scbndsr"
                     , scbnds_raw                      --> prettyR "scbnds"
                     , cbld_raw                        --> prettyR "cbld"
                     , sentry_raw                      --> prettyR_2op "sentry"
                     , caddi_raw                       --> prettyI "caddi"
                     , scbndsi_raw                     --> pretty_scbndsi "scbndsi"
                     , cspecialrw_raw                  --> pretty_cspecialrw "cspecialrw"
                     , cmv_raw                         --> prettyR_2op "cmv"
                     , modeswcap_raw                   --> "modesw.cap"
                     , modeswint_raw                   --> "modesw.int"
                     , ctestsubset_raw                 --> prettyR "ctestsubset"
                     , crepresentablealignmentmask_raw --> prettyR_2op "crepresentablealignmentmask"
                     , cload_raw                       --> prettyCLoad
                     , cstore_raw                      --> prettyCStore
                     , scmode_raw                      --> prettyR "scmode"
                     , sq_raw                          --> prettyS "sq"
                     , lq_raw                          --> prettyL "lq"
                     , lr_q_raw                        --> prettyR_A_1op "lr.q"
                     , sc_q_raw                        --> prettyR_A "sc.q" ]

extract_cspecialrw :: Integer -> Integer -> Integer -> ExtractedRegs
extract_cspecialrw idx rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode cspecialrw_raw idx y z)

extract_cmv :: Integer -> Integer -> ExtractedRegs
extract_cmv rs1 rd = (True, Nothing, Just rs1, Just rd, \x y z -> encode cmv_raw y z)

extract_cstore :: Integer -> Integer -> Integer -> ExtractedRegs
extract_cstore rs2 rs1 mop = (False, Just rs2, Just rs1, Nothing, \x y z -> encode cstore_raw x y mop)

rv32_xcheri_extract :: [DecodeBranch ExtractedRegs]
rv32_xcheri_extract = [ gcperm_raw                      --> extract_1op gcperm_raw
                      , gctype_raw                      --> extract_1op gctype_raw
                      , gcbase_raw                      --> extract_1op gcbase_raw
                      , gclen_raw                       --> extract_1op gclen_raw
                      , gctag_raw                       --> extract_1op gctag_raw
                      , gchigh_raw                      --> extract_1op gchigh_raw
                      , gcmode_raw                      --> extract_1op gcmode_raw
                      , acperm_raw                      --> extract_2op acperm_raw
                      , scaddr_raw                      --> extract_2op scaddr_raw
                      , schi_raw                        --> extract_2op schi_raw
                      , cadd_raw                        --> extract_2op cadd_raw
                      , scbndsr_raw                     --> extract_2op scbndsr_raw
                      , scbnds_raw                      --> extract_2op scbnds_raw
                      , cbld_raw                        --> extract_2op cbld_raw
                      , sentry_raw                      --> extract_1op sentry_raw
                      , caddi_raw                       --> extract_imm caddi_raw
                      , scbndsi_raw                     --> extract_imm scbndsi_raw
                      , cspecialrw_raw                  --> extract_cspecialrw
                      , cmv_raw                         --> extract_cmv
                      , crepresentablealignmentmask_raw --> extract_1op crepresentablealignmentmask_raw
                      , cload_raw                       --> extract_imm cload_raw
                      , cstore_raw                      --> extract_cstore
                      , scmode_raw                      --> extract_2op scmode_raw
                      , sq_raw                          --> extract_nodst sq_raw
                      , lq_raw                          --> extract_imm lq_raw
                      ]

shrink_gcperm :: Integer -> Integer -> [Instruction]
shrink_gcperm cs rd = [addi rd 0 0, addi rd 0 0x7ff]

shrink_gctype :: Integer -> Integer -> [Instruction]
shrink_gctype cs rd = [addi rd 0 0, addi rd 0 6, addi rd 0 0xfff]

shrink_gcbase :: Integer -> Integer -> [Instruction]
shrink_gcbase cs rd = [addi rd 0 0]

shrink_gclen :: Integer -> Integer -> [Instruction]
shrink_gclen cs rd = [addi rd 0 0, addi rd 0 0xfff, gcbase rd cs]

shrink_gctag :: Integer -> Integer -> [Instruction]
shrink_gctag cs rd = [addi rd 0 1, addi rd 0 0]

shrink_gchigh :: Integer -> Integer -> [Instruction]
shrink_gchigh cs rd = [addi rd cs 0, addi rd cs 0xfff]

shrink_gcmode :: Integer -> Integer -> [Instruction]
shrink_gcmode cs rd = [addi rd 0 1, addi rd 0 0]

shrink_cap :: Integer -> Integer -> [Instruction]
shrink_cap cs cd = [ecall,
                    cmv cd cs,
                    gchigh cd cs,
                    gcmode cd cs,
                    gcperm cd cs,
                    gctype cd cs,
                    gcbase cd cs,
                    gclen cd cs,
                    gctag cd cs
                   ]

shrink_capcap :: Integer -> Integer -> Integer -> [Instruction]
shrink_capcap cs2 cs1 cd = (shrink_cap cs2 cd) ++ (shrink_cap cs1 cd)

shrink_capint :: Integer -> Integer -> Integer -> [Instruction]
shrink_capint rs cs cd = shrink_cap cs cd

shrink_capimm :: Integer -> Integer -> Integer -> [Instruction]
shrink_capimm imm cs cd = shrink_cap cs cd ++ [addi cd 0 imm, addi cd cs imm]

shrink_ctestsubset cs2 cs1 rd = [addi rd 0 0, addi rd 0 1] ++ shrink_capcap cs2 cs1 rd

shrink_cload :: Integer -> Integer -> Integer -> [Instruction]
shrink_cload cb cd mop = [addi 0 0 0];

shrink_cstore :: Integer -> Integer -> Integer -> [Instruction]
shrink_cstore rs2 cs1 mop = [addi 0 0 0];

rv32_xcheri_shrink :: [DecodeBranch [Instruction]]
rv32_xcheri_shrink = [ gcperm_raw                      --> shrink_gcperm
                     , gctype_raw                      --> shrink_gctype
                     , gcbase_raw                      --> shrink_gcbase
                     , gclen_raw                       --> shrink_gclen
                     , gctag_raw                       --> shrink_gctag
                     , gchigh_raw                      --> shrink_gchigh
                     , gcmode_raw                      --> shrink_gcmode
                     , acperm_raw                      --> shrink_capint
                     , scaddr_raw                      --> shrink_capint
                     , schi_raw                        --> shrink_capint
                     , cadd_raw                        --> shrink_capint
                     , scbndsr_raw                     --> shrink_capint
                     , scbnds_raw                      --> shrink_capint
                     , cbld_raw                        --> shrink_capcap
                     , sentry_raw                      --> shrink_cap
                     , caddi_raw                       --> shrink_capimm
                     , scbndsi_raw                     --> shrink_capimm
--                   , cspecialrw_raw                  --> noshrink
--                   , cmv_raw                         --> noshrink
                     , ctestsubset_raw                 --> shrink_ctestsubset
--                   , crepresentablealignmentmask_raw --> noshrink
                     , cload_raw                       --> shrink_cload
                     , cstore_raw                      --> shrink_cstore
                     , scmode_raw                      --> shrink_capcap
--                   , sq_raw                          --> noshrink
--                   , lq_raw                          --> noshrink
                     ]

-- | List of cheri inspection instructions
rv32_xcheri_inspection :: Integer -> Integer -> [Instruction]
rv32_xcheri_inspection src dest = [ gcperm                      dest src
                                  , gctype                      dest src
                                  , gcbase                      dest src
                                  , gclen                       dest src
                                  , gctag                       dest src
                                  , gchigh                      dest src
                                  , gcmode                      dest src
                                  , crepresentablealignmentmask dest src]

-- | List of cheri arithmetic instructions
rv32_xcheri_arithmetic :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_arithmetic src1 src2 imm dest =
  [ scaddr              dest src1 src2
  , schi                dest src1 src2
  , cadd                dest src1 src2
  , scbndsr             dest src1 src2
  , scbnds              dest src1 src2
  , scbndsi             dest src1 0 imm
  , caddi               dest src1 imm
  , ctestsubset         dest src1 src2 ]

-- | List of cheri miscellaneous instructions
rv32_xcheri_misc :: Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_misc src1 src2 srcScr imm dest =
  [ acperm      dest src1 src2
  , scmode      dest src1 src2
  , sentry      dest src1
  , cmv         dest src1
  , cspecialrw  dest srcScr src1 ]

-- | List of cheri control instructions
rv32_xcheri_control :: Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_control src1 src2 dest = [ modeswcap
                                     , modeswint]

-- | List of cheri memory instructions
rv32_xcheri_mem :: ArchDesc -> Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_mem    arch srcAddr srcData imm mop dest =
  [ cload  dest    srcAddr         mop
  , cstore         srcData srcAddr mop
  --, ld     dest srcAddr dest        imm
  --, sd          srcAddr srcData     imm
  --, lq     dest srcAddr dest        imm
  --, sq          srcAddr srcData     imm
  ]
  ++ [cload dest srcAddr mop
  ,   gctag dest dest]

-- | List of cheri memory instructions
rv32_a_xcheri :: Integer -> Integer -> Integer -> [Instruction]
rv32_a_xcheri      srcAddr srcData dest =
  [ cload  dest    srcAddr         0x10 -- lr.b.ddc
  , cload  dest    srcAddr         0x11 -- lr.h.ddc
  , cload  dest    srcAddr         0x12 -- lr.w.ddc
  , cload  dest    srcAddr         0x13 -- lr.d.ddc
  , cload  dest    srcAddr         0x14 -- lr.q.ddc
  , cload  dest    srcAddr         0x18 -- lr.b.cap
  , cload  dest    srcAddr         0x19 -- lr.h.cap
  , cload  dest    srcAddr         0x1a -- lr.w.cap
  , cload  dest    srcAddr         0x1b -- lr.d.cap
  , cload  dest    srcAddr         0x1c -- lr.q.cap
  , cstore         srcData srcAddr 0x10 -- sc.b.ddc
  , cstore         srcData srcAddr 0x11 -- sc.h.ddc
  , cstore         srcData srcAddr 0x12 -- sc.w.ddc
  , cstore         srcData srcAddr 0x13 -- sc.d.ddc
  , cstore         srcData srcAddr 0x14 -- sc.q.ddc
  , cstore         srcData srcAddr 0x18 -- sc.b.cap
  , cstore         srcData srcAddr 0x19 -- sc.h.cap
  , cstore         srcData srcAddr 0x1a -- sc.w.cap
  , cstore         srcData srcAddr 0x1b -- sc.d.cap
  , cstore         srcData srcAddr 0x1c -- sc.q.cap
  ]

-- | List of cheri instructions
rv32_xcheri :: ArchDesc -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri arch src1 src2 srcScr imm mop dest =
     rv32_xcheri_inspection src1 dest
  ++ rv32_xcheri_arithmetic src1 src2 imm dest
  ++ rv32_xcheri_misc src1 src2 srcScr imm dest
  ++ rv32_xcheri_control src1 src2 dest
  ++ rv32_xcheri_mem arch src1 src2 imm mop dest
