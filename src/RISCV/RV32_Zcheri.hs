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
, sceq
, scss
, cram
, lc
, sc
, lr_b
, sc_b
, lr_h
, sc_h
, lr_c
, sc_c
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
gcperm_raw               =                              "0001000 00001 cs1[4:0] 000 rd[4:0] 0110011"
gcperm rd cs1            = encode gcperm_raw                           cs1          rd
gctype_raw               =                              "0001000 00010 cs1[4:0] 000 rd[4:0] 0110011"
gctype rd cs1            = encode gctype_raw                           cs1          rd
gcbase_raw               =                              "0001000 00101 cs1[4:0] 000 rd[4:0] 0110011"
gcbase rd cs1            = encode gcbase_raw                           cs1          rd
gclen_raw                =                              "0001000 00110 cs1[4:0] 000 rd[4:0] 0110011"
gclen rd cs1             = encode gclen_raw                            cs1          rd
gctag_raw                =                              "0001000 00000 cs1[4:0] 000 rd[4:0] 0110011"
gctag rd cs1             = encode gctag_raw                            cs1          rd
gchigh_raw               =                              "0001000 00100 cs1[4:0] 000 rd[4:0] 0110011"
gchigh rd cs1            = encode gchigh_raw                           cs1          rd
gcmode_raw               =                              "0001000 00011 cs1[4:0] 000 rd[4:0] 0110011"
gcmode rd cs1            = encode gcmode_raw                           cs1          rd

-- Capability Modification
acperm_raw                 =                            "0000110 rs2[4:0] cs1[4:0] 010 cd[4:0] 0110011"
acperm cd cs1 rs2          = encode acperm_raw                   rs2      cs1          cd
scmode_raw                 =                            "0000110 rs2[4:0] cs1[4:0] 111 cd[4:0] 0110011"
scmode cd cs1 rs2          = encode scmode_raw                   rs2      cs1          cd
scaddr_raw                 =                            "0000110 rs2[4:0] cs1[4:0] 001 cd[4:0] 0110011"
scaddr cd cs1 rs2          = encode scaddr_raw                   rs2      cs1          cd
schi_raw                   =                            "0000110 rs2[4:0] cs1[4:0] 011 cd[4:0] 0110011"
schi cd cs1 rs2            = encode schi_raw                     rs2      cs1          cd
cadd_raw                   =                            "0000110 rs2[4:0] cs1[4:0] 000 cd[4:0] 0110011"
cadd cd cs1 rs2            = encode cadd_raw                     rs2      cs1          cd
caddi_raw                  =                            "imm[11:0] cs1[4:0] 010 cd[4:0] 0011011"
caddi cd cs1 imm           = encode caddi_raw            imm        cs1          cd
scbndsr_raw                =                            "0000111 rs2[4:0] cs1[4:0] 001 cd[4:0] 0110011"
scbndsr cd cs1 rs2         = encode scbndsr_raw                  rs2      cs1          cd
scbnds_raw                 =                            "0000111 rs2[4:0] cs1[4:0] 000 cd[4:0] 0110011"
scbnds cd cs1 rs2          = encode scbnds_raw                   rs2      cs1          cd
scbndsi_raw                =                            "000001 s[0:0] imm[4:0] cs1[4:0] 101 cd[4:0] 0010011"
scbndsi cd cs1 s imm       = encode scbndsi_raw                 s      imm      cs1          cd
cbld_raw                   =                            "0000110 cs2[4:0] cs1[4:0] 101 cd[4:0] 0110011"
cbld cd cs1 cs2            = encode cbld_raw                     cs2      cs1          cd
sentry_raw                 =                            "0001000 01000 cs1[4:0] 000 cd[4:0] 0110011"
sentry cd cs1              = encode sentry_raw                         cs1          cd


-- Capability Pointer Arithmetic
cmv_raw                    =                            "0000110 00000 cs1[4:0] 000 cd[4:0] 0110011"
cmv cd cs1                 = encode cmv_raw                            cs1          cd


-- Control Flow
modeswcap_raw              =                            "0001001 00000 00000 001 00000 0110011"
modeswcap                  = encode modeswcap_raw
modeswint_raw              =                            "0001010 00000 00000 001 00000 0110011"
modeswint                  = encode modeswint_raw

-- Assertion
sceq_raw                   =                            "0000110 cs2[4:0] cs1[4:0] 100 rd[4:0] 0110011"
sceq rd cs1 cs2            = encode sceq_raw                     cs2      cs1          rd
scss_raw                   =                            "0000110 cs2[4:0] cs1[4:0] 110 rd[4:0] 0110011"
scss rd cs1 cs2            = encode scss_raw                     cs2      cs1          rd

-- Adjusting to Compressed Capability Precision
cram_raw    =                                           "0001000 00111 rs1[4:0] 000 rd[4:0] 0110011"
cram rd rs1 = encode cram_raw                                          rs1          rd

-- Memory -- Needs further refinement
lc_raw                     =                             "imm[11:0] rs1[4:0] 100 cd[4:0] 0001111"
lc cd rs1 imm              = encode lc_raw                imm       rs1          cd
sc_raw                     =                             "imm[11:5] cs2[4:0] rs1[4:0] 100 imm[4:0] 0100011"
sc rs1 cs2 imm             = encode sc_raw                imm       cs2      rs1
lr_b_raw                   =                             "00010 aq[0] rl[0]    00000 rs1[4:0] 000 rd[4:0] 0101111"
lr_b rd rs1 aq rl          = encode lr_b_raw                    aq    rl             rs1          rd
sc_b_raw                   =                             "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 000 rd[4:0] 0101111"
sc_b rd rs1 rs2 aq rl      = encode sc_h_raw                    aq    rl    rs2      rs1          rd
lr_h_raw                   =                             "00010 aq[0] rl[0]    00000 rs1[4:0] 001 rd[4:0] 0101111"
lr_h rd rs1 aq rl          = encode lr_b_raw                    aq    rl             rs1          rd
sc_h_raw                   =                             "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 001 rd[4:0] 0101111"
sc_h rd rs1 rs2 aq rl      = encode sc_h_raw                    aq    rl    rs2      rs1          rd
lr_c_raw                   =                             "00010 aq[0] rl[0]    00000 rs1[4:0] 100 rd[4:0] 0101111"
lr_c rd rs1 aq rl          = encode lr_c_raw                    aq    rl             rs1          rd
sc_c_raw                   =                             "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 100 rd[4:0] 0101111"
sc_c rd rs1 rs2 aq rl      = encode sc_c_raw                    aq    rl    rs2      rs1          rd
amoswap_q_raw              =                             "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 100 rd[4:0] 0101111"
amoswap_q rd rs1 rs2 aq rl = encode amoswap_q_raw               aq    rl    rs2      rs1          rd

-- | Pretty-print a 2 sources instruction
pretty_2src instr src2 src1 = concat [instr, " ", reg src1, ", ", reg src2]

-- | Scaled I-type instruction pretty printer
pretty_scbndsi instr s imm cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1, ", ", int s, ", ", int imm]

-- | Dissassembly of CHERI instructions
rv32_xcheri_disass :: [DecodeBranch String]
rv32_xcheri_disass = [ gcperm_raw     --> prettyR_2op "gcperm"
                     , gctype_raw     --> prettyR_2op "gctype"
                     , gcbase_raw     --> prettyR_2op "gcbase"
                     , gclen_raw      --> prettyR_2op "gclen"
                     , gctag_raw      --> prettyR_2op "gctag"
                     , gchigh_raw     --> prettyR_2op "gchigh"
                     , gcmode_raw     --> prettyR_2op "gcmode"
                     , acperm_raw     --> prettyR "acperm"
                     , scaddr_raw     --> prettyR "scaddr"
                     , schi_raw       --> prettyR "schi"
                     , cmv_raw        --> prettyR_2op "cmv" -- Ensure this is above cadd
                     , cadd_raw       --> prettyR "cadd"
                     , scbndsr_raw    --> prettyR "scbndsr"
                     , scbnds_raw     --> prettyR "scbnds"
                     , cbld_raw       --> prettyR "cbld"
                     , sentry_raw     --> prettyR_2op "sentry"
                     , caddi_raw      --> prettyI "caddi"
                     , scbndsi_raw    --> pretty_scbndsi "scbndsi"
                     , modeswcap_raw  --> "modesw.cap"
                     , modeswint_raw  --> "modesw.int"
                     , sceq_raw       --> prettyR "sceq"
                     , scss_raw       --> prettyR "scss"
                     , cram_raw       --> prettyR_2op "cram"
                     , scmode_raw     --> prettyR "scmode"
                     , sc_raw         --> prettyS "sc"
                     , lc_raw         --> prettyL "lc"
                     , lr_b_raw       --> prettyR_A_1op "lr.b"
                     , sc_b_raw       --> prettyR_A "sc.b"
                     , lr_h_raw       --> prettyR_A_1op "lr.h"
                     , sc_h_raw       --> prettyR_A "sc.h"
                     , lr_c_raw       --> prettyR_A_1op "lr.c"
                     , sc_c_raw       --> prettyR_A "sc.c" ]

extract_cmv :: Integer -> Integer -> ExtractedRegs
extract_cmv rs1 rd = (True, Nothing, Just rs1, Just rd, \x y z -> encode cmv_raw y z)

rv32_xcheri_extract :: [DecodeBranch ExtractedRegs]
rv32_xcheri_extract = [ gcperm_raw      --> extract_1op gcperm_raw
                      , gctype_raw      --> extract_1op gctype_raw
                      , gcbase_raw      --> extract_1op gcbase_raw
                      , gclen_raw       --> extract_1op gclen_raw
                      , gctag_raw       --> extract_1op gctag_raw
                      , gchigh_raw      --> extract_1op gchigh_raw
                      , gcmode_raw      --> extract_1op gcmode_raw
                      , acperm_raw      --> extract_2op acperm_raw
                      , scaddr_raw      --> extract_2op scaddr_raw
                      , schi_raw        --> extract_2op schi_raw
                      , cmv_raw         --> extract_cmv -- Ensure this is above cadd
                      , cadd_raw        --> extract_2op cadd_raw
                      , scbndsr_raw     --> extract_2op scbndsr_raw
                      , scbnds_raw      --> extract_2op scbnds_raw
                      , cbld_raw        --> extract_2op cbld_raw
                      , sentry_raw      --> extract_1op sentry_raw
                      , caddi_raw       --> extract_imm caddi_raw
                      , scbndsi_raw     --> extract_imm scbndsi_raw
                      , cram_raw        --> extract_1op cram_raw
                      , scmode_raw      --> extract_2op scmode_raw
                      , sc_raw          --> extract_nodst sc_raw
                      , lc_raw          --> extract_imm lc_raw
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

noshrink_capcap :: Integer -> Integer -> Integer -> [Instruction]
noshrink_capcap cs2 cs1 cd = []

shrink_capint :: Integer -> Integer -> Integer -> [Instruction]
shrink_capint rs cs cd = shrink_cap cs cd

shrink_capimm :: Integer -> Integer -> Integer -> [Instruction]
shrink_capimm imm cs cd = shrink_cap cs cd ++ [addi cd 0 imm, addi cd cs imm]

shrink_scbndsi :: Integer -> Integer -> Integer -> Integer -> [Instruction]
shrink_scbndsi s imm cs cd = shrink_cap cs cd ++ [addi cd 0 imm, addi cd cs imm]

shrink_sceq cs2 cs1 rd = [addi rd 0 0, addi rd 0 1] ++ shrink_capcap cs2 cs1 rd
shrink_scss cs2 cs1 rd = [addi rd 0 0, addi rd 0 1] ++ shrink_capcap cs2 cs1 rd

rv32_xcheri_shrink :: [DecodeBranch [Instruction]]
rv32_xcheri_shrink = [ gcperm_raw       --> shrink_gcperm
                     , gctype_raw       --> shrink_gctype
                     , gcbase_raw       --> shrink_gcbase
                     , gclen_raw        --> shrink_gclen
                     , gctag_raw        --> shrink_gctag
                     , gchigh_raw       --> shrink_gchigh
                     , gcmode_raw       --> shrink_gcmode
                     , acperm_raw       --> shrink_capint
                     , scaddr_raw       --> shrink_capint
                     , schi_raw         --> shrink_capint
                     , cmv_raw          --> noshrink_capcap -- Ensure this is above cadd
                     , cadd_raw         --> shrink_capint
                     , scbndsr_raw      --> shrink_capint
                     , scbnds_raw       --> shrink_capint
                     , cbld_raw         --> shrink_capcap
                     , sentry_raw       --> shrink_cap
                     , caddi_raw        --> shrink_capimm
                     , scbndsi_raw      --> shrink_scbndsi
                     , sceq_raw         --> shrink_sceq
                     , scss_raw         --> shrink_scss
--                   , cram_raw         --> noshrink
                     , scmode_raw       --> shrink_capcap
--                   , sc_raw           --> noshrink
--                   , lc_raw           --> noshrink
                     ]

-- | List of cheri inspection instructions
rv32_xcheri_inspection :: Integer -> Integer -> [Instruction]
rv32_xcheri_inspection src dest = [ gcperm dest src
                                  , gctype dest src
                                  , gcbase dest src
                                  , gclen  dest src
                                  , gctag  dest src
                                  , gchigh dest src
                                  , gcmode dest src
                                  , cram   dest src]

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
  , sceq                dest src1 src2
  , scss                dest src1 src2 ]

-- | List of cheri miscellaneous instructions
rv32_xcheri_misc :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_misc src1 src2 imm dest =
  [ acperm      dest src1 src2
  , scmode      dest src1 src2
  , cbld        dest src1 src2
  , sentry      dest src1
  , cmv         dest src1
  ]

-- | List of cheri control instructions
rv32_xcheri_control :: Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_control src1 src2 dest = [ modeswcap
                                     , modeswint]

-- | List of cheri memory instructions
rv32_xcheri_mem :: ArchDesc -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_mem    arch srcAddr srcData imm dest =
  [ lc dest srcAddr         imm
  , sc      srcAddr srcData imm
  ]
  ++ [ lc    dest srcAddr      imm
  ,    gctag dest dest ]

-- | List of cheri memory instructions
rv32_a_xcheri :: Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_a_xcheri      srcAddr srcData dest aq rl =
  [ lr_b dest srcAddr aq rl
  , sc_b dest srcAddr srcData aq rl
  , lr_h dest srcAddr aq rl
  , sc_h dest srcAddr srcData aq rl
  , lr_c dest srcAddr aq rl
  , sc_c dest srcAddr srcData aq rl
  ]

-- | List of cheri instructions
rv32_xcheri :: ArchDesc -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri arch src1 src2 imm dest =
     rv32_xcheri_inspection src1 dest
  ++ rv32_xcheri_arithmetic src1 src2 imm dest
  ++ rv32_xcheri_misc src1 src2 imm dest
  ++ rv32_xcheri_control src1 src2 dest
  ++ rv32_xcheri_mem arch src1 src2 imm dest
