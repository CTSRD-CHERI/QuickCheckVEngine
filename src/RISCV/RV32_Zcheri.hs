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
  ypermr
, ytyper
, ybaser
, ylenr
, ytopr -- new
, ytagr
, srliy
, ymoder
, ypermc
, ymodew
, yaddrw
, packy
, yadd
, yaddi
, ybndsrw
, ybndsw
, ybndswi
, ybld
, ysunseal
, ysentry
, ymv
, ymodeswy
, ymodeswi
, yeq
, yss
, yamask
, ly
, sy
, lr_b
, sc_b
, lr_h
, sc_h
, lr_y
, sc_y
, amoswap_y
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
ypermr_raw               =                              "1111010 00001 cs1[4:0] 000 rd[4:0] 1111011"
ypermr rd cs1            = encode ypermr_raw                           cs1          rd
ytyper_raw               =                              "1111010 00101 cs1[4:0] 000 rd[4:0] 1111011"
ytyper rd cs1            = encode ytyper_raw                           cs1          rd
ybaser_raw               =                              "1111010 00000 cs1[4:0] 000 rd[4:0] 1111011"
ybaser rd cs1            = encode ybaser_raw                           cs1          rd
ylenr_raw                =                              "1111010 00011 cs1[4:0] 000 rd[4:0] 1111011"
ylenr rd cs1             = encode ylenr_raw                            cs1          rd
ytopr_raw                =                              "1111010 00010 cs1[4:0] 000 rd[4:0] 1111011"
ytopr rd cs1             = encode ytopr_raw                            cs1          rd
ytagr_raw                =                              "1111010 00100 cs1[4:0] 000 rd[4:0] 1111011"
ytagr rd cs1             = encode ytagr_raw                            cs1          rd
srliy_raw                =                              "00000 1000000 cs1[4:0] 101 rd[4:0] 1111011"
srliy rd cs1             = encode srliy_raw                            cs1          rd
ymoder_raw               =                              "1111010 00110 cs1[4:0] 000 rd[4:0] 1111011"
ymoder rd cs1            = encode ymoder_raw                           cs1          rd

-- Capability Modification
ypermc_raw                 =                            "0010011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
ypermc cd cs1 rs2          = encode ypermc_raw                   rs2      cs1          cd
ymodew_raw                 =                            "0101011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
ymodew cd cs1 rs2          = encode ymodew_raw                   rs2      cs1          cd
yaddrw_raw                 =                            "0001011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
yaddrw cd cs1 rs2          = encode yaddrw_raw                   rs2      cs1          cd
packy_raw                  =                            "0000001 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
packy cd cs1 rs2           = encode packy_raw                    rs2      cs1          cd
yadd_raw                   =                            "0000011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
yadd cd cs1 rs2            = encode yadd_raw                     rs2      cs1          cd
yaddi_raw                  =                            "imm[11:0] cs1[4:0] 100 cd[4:0] 1111011"
yaddi cd cs1 imm           = encode yaddi_raw            imm       cs1          cd
ybndsrw_raw                =                            "0100011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
ybndsrw cd cs1 rs2         = encode ybndsrw_raw                  rs2      cs1          cd
ybndsw_raw                 =                            "0011011 rs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
ybndsw cd cs1 rs2          = encode ybndsw_raw                   rs2      cs1          cd
ybndswi_raw                =                            "111 imm[8:0] cs1[4:0] 101 cd[4:0] 1111011"
ybndswi cd cs1 imm         = encode ybndswi_raw              imm      cs1          cd
ybld_raw                   =                            "0001111 cs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
ybld cd cs1 cs2            = encode ybld_raw                     cs2      cs1          cd
ysunseal_raw               =                            "0000111 cs2[4:0] cs1[4:0] 000 cd[4:0] 1111011"
ysunseal cd cs1 cs2        = encode ysunseal_raw                 cs2      cs1          cd
ysentry_raw                =                            "0010111 cs1[4:0] 00000 000 cd[4:0] 1111011"
ysentry cd cs1             = encode ysentry_raw                  cs1                cd


-- Capability Pointer Arithmetic
ymv_raw                    =                            "0000011 00000 cs1[4:0] 000 cd[4:0] 1111011"
ymv cd cs1                 = encode ymv_raw                            cs1          cd


-- Control Flow
ymodeswy_raw              =                            "0101011 00000 00000 000 00000 1111011"
ymodeswy                  = encode ymodeswy_raw
ymodeswi_raw              =                            "0101011 00001 00000 000 00000 1111011"
ymodeswi                  = encode ymodeswi_raw

-- Assertion
yeq_raw                   =                            "0000110 cs2[4:0] cs1[4:0] 000 rd[4:0] 1111011"
yeq rd cs1 cs2            = encode yeq_raw                     cs2      cs1          rd
yss_raw                   =                            "0001110 cs2[4:0] cs1[4:0] 000 rd[4:0] 1111011"
yss rd cs1 cs2            = encode yss_raw                     cs2      cs1          rd

-- Adjusting to Compressed Capability Precision
yamask_raw    =                                           "1111000 00000 rs1[4:0] 000 rd[4:0] 1111011"
yamask rd rs1 = encode yamask_raw                                          rs1          rd

-- Memory -- Needs further refinement
ly_raw                     =                             "imm[11:0] rs1[4:0] 001 cd[4:0] 1111011"
ly cd rs1 imm              = encode ly_raw                imm       rs1          cd
sy_raw                     =                             "imm[11:5] cs2[4:0] rs1[4:0] 010 imm[4:0] 1111011"
sy rs1 cs2 imm             = encode sy_raw                imm       cs2      rs1
lr_b_raw                   =                             "00010 aq[0] rl[0]    00000 rs1[4:0] 000 rd[4:0] 0101111"
lr_b rd rs1 aq rl          = encode lr_b_raw                    aq    rl             rs1          rd
sc_b_raw                   =                             "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 000 rd[4:0] 0101111"
sc_b rd rs1 rs2 aq rl      = encode sc_h_raw                    aq    rl    rs2      rs1          rd
lr_h_raw                   =                             "00010 aq[0] rl[0]    00000 rs1[4:0] 001 rd[4:0] 0101111"
lr_h rd rs1 aq rl          = encode lr_b_raw                    aq    rl             rs1          rd
sc_h_raw                   =                             "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 001 rd[4:0] 0101111"
sc_h rd rs1 rs2 aq rl      = encode sc_h_raw                    aq    rl    rs2      rs1          rd
lr_y_raw                   =                             "00010 aq[0] rl[0]    00000 rs1[4:0] 011 rd[4:0] 1111011"
lr_y rd rs1 aq rl          = encode lr_y_raw                    aq    rl             rs1          rd
sc_y_raw                   =                             "00011 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 1111011"
sc_y rd rs1 rs2 aq rl      = encode sc_y_raw                    aq    rl    rs2      rs1          rd
amoswap_y_raw              =                             "00001 aq[0] rl[0] rs2[4:0] rs1[4:0] 011 rd[4:0] 1111011"
amoswap_y rd rs1 rs2 aq rl = encode amoswap_y_raw               aq    rl    rs2      rs1          rd

-- | Pretty-print a 2 sources instruction
pretty_2src instr src2 src1 = concat [instr, " ", reg src1, ", ", reg src2]

-- | Scaled I-type instruction pretty printer
pretty_ybndswi instr imm cs1 cd =
  concat [instr, " ", reg cd, ", ", reg cs1, ", ", int imm]

-- | Dissassembly of CHERI instructions
rv32_xcheri_disass :: [DecodeBranch String]
rv32_xcheri_disass = [ ypermr_raw     --> prettyR_2op "ypermr"
                     , ytyper_raw     --> prettyR_2op "ytyper"
                     , ybaser_raw     --> prettyR_2op "ybaser"
                     , ylenr_raw      --> prettyR_2op "ylenr"
                     , ytopr_raw      --> prettyR_2op "ytopr"
                     , ytagr_raw      --> prettyR_2op "ytagr"
                     , srliy_raw      --> prettyR_2op "srliy"
                     , ymoder_raw     --> prettyR_2op "ymoder"
                     , ypermr_raw     --> prettyR "ypermr"
                     , yaddrw_raw     --> prettyR "yaddrw"
                     , packy_raw      --> prettyR "packy"
                     , ymv_raw        --> prettyR_2op "ymv" -- Ensure this is above yadd
                     , yadd_raw       --> prettyR "yadd"
                     , ybndsrw_raw    --> prettyR "ybndsrw"
                     , ybndsw_raw     --> prettyR "ybndsw"
                     , ybld_raw       --> prettyR "ybld"  --xxx
                     , ysunseal_raw   --> prettyR "ysunseal" --xxx
                     , ysentry_raw    --> prettyR_2op "ysentry" --xxx
                     , yaddi_raw      --> prettyI "yaddi"
                     , ybndswi_raw    --> pretty_ybndswi "ybndswi"
                     , ymodeswy_raw   --> "modesw.cap" --xxx
                     , ymodeswi_raw   --> "modesw.int" --xxx
                     , yeq_raw        --> prettyR "yeq"
                     , yss_raw        --> prettyR "yss"
                     , yamask_raw     --> prettyR_2op "yamask" --xxx
                     , ymodew_raw     --> prettyR "ymodew" --xxx
                     , sy_raw         --> prettyS "sy"
                     , ly_raw         --> prettyL "ly"
                     , lr_b_raw       --> prettyR_A_1op "lr.b"
                     , sc_b_raw       --> prettyR_A "sc.b"
                     , lr_h_raw       --> prettyR_A_1op "lr.h"
                     , sc_h_raw       --> prettyR_A "sc.h"
                     , lr_y_raw       --> prettyR_A_1op "lr.c"
                     , sc_y_raw       --> prettyR_A "sc.c" ]

extract_ymv :: Integer -> Integer -> ExtractedRegs
extract_ymv rs1 rd = (True, Nothing, Just rs1, Just rd, \x y z -> encode ymv_raw y z)

extract_ybndswi :: String -> Integer -> Integer -> Integer -> ExtractedRegs
extract_ybndswi instr imm rs1 rd = (False, Nothing, Just rs1, Just rd, \x y z -> encode instr imm y z)

rv32_xcheri_extract :: [DecodeBranch ExtractedRegs]
rv32_xcheri_extract = [ ypermc_raw      --> extract_1op ypermc_raw
                      , ytyper_raw      --> extract_1op ytyper_raw
                      , ybaser_raw      --> extract_1op ybaser_raw
                      , ylenr_raw       --> extract_1op ylenr_raw
                      , ytopr_raw       --> extract_1op ytopr_raw
                      , ytagr_raw       --> extract_1op ytagr_raw
                      , srliy_raw       --> extract_1op srliy_raw
                      , ymoder_raw      --> extract_1op ymoder_raw
                      , ypermc_raw      --> extract_2op ypermc_raw
                      , yaddrw_raw      --> extract_2op yaddrw_raw
                      , packy_raw       --> extract_2op packy_raw
                      , ymv_raw         --> extract_ymv -- Ensure this is above yadd
                      , yadd_raw        --> extract_2op yadd_raw
                      , ybndsrw_raw     --> extract_2op ybndsrw_raw
                      , ybndsw_raw      --> extract_2op ybndsw_raw
                      , ybld_raw        --> extract_2op ybld_raw
                      , ysunseal_raw    --> extract_2op ysunseal_raw
                      , ysentry_raw     --> extract_1op ysentry_raw
                      , yaddi_raw       --> extract_imm yaddi_raw
                      , ybndswi_raw     --> extract_ybndswi ybndswi_raw
                      , yamask_raw      --> extract_1op yamask_raw
                      , ymodew_raw      --> extract_2op ymodew_raw
                      , sy_raw          --> extract_nodst sy_raw
                      , ly_raw          --> extract_imm ly_raw
                      ]

shrink_ypermr :: Integer -> Integer -> [Instruction]
shrink_ypermr cs rd = [addi rd 0 0, addi rd 0 0x7ff]

shrink_ytyper :: Integer -> Integer -> [Instruction]
shrink_ytyper cs rd = [addi rd 0 0, addi rd 0 6, addi rd 0 0xfff]

shrink_ybaser :: Integer -> Integer -> [Instruction]
shrink_ybaser cs rd = [addi rd 0 0]

shrink_ylenr :: Integer -> Integer -> [Instruction]
shrink_ylenr cs rd = [addi rd 0 0, addi rd 0 0xfff, ybaser rd cs]

shrink_ytopr :: Integer -> Integer -> [Instruction]
shrink_ytopr cs rd = [addi rd 0 0, addi rd 0 0xfff, ybaser rd cs]

shrink_ytagr :: Integer -> Integer -> [Instruction]
shrink_ytagr cs rd = [addi rd 0 1, addi rd 0 0]

shrink_srliy :: Integer -> Integer -> [Instruction]
shrink_srliy cs rd = [addi rd cs 0, addi rd cs 0xfff]

shrink_ymoder :: Integer -> Integer -> [Instruction]
shrink_ymoder cs rd = [addi rd 0 1, addi rd 0 0]

shrink_cap :: Integer -> Integer -> [Instruction]
shrink_cap cs cd = [ecall,
                    --ymv cd cs,
                    ymoder cd cs,
                    ypermr cd cs,
                    ytyper cd cs,
                    ybaser cd cs,
                    ylenr cd cs,
                    ytagr cd cs
                   ]

shrink_capcap :: Integer -> Integer -> Integer -> [Instruction]
shrink_capcap cs2 cs1 cd = (shrink_cap cs2 cd) ++ (shrink_cap cs1 cd)

noshrink_cap :: Integer -> Integer -> [Instruction]
noshrink_cap cs1 cd = []

shrink_capint :: Integer -> Integer -> Integer -> [Instruction]
shrink_capint rs cs cd = shrink_cap cs cd

shrink_capimm :: Integer -> Integer -> Integer -> [Instruction]
shrink_capimm imm cs cd = shrink_cap cs cd ++ [addi cd 0 imm, addi cd cs imm]

shrink_ybndswi :: Integer -> Integer -> Integer -> [Instruction]
shrink_ybndswi imm cs cd = shrink_cap cs cd ++ [addi cd 0 imm, addi cd cs imm]

shrink_yeq cs2 cs1 rd = [addi rd 0 0, addi rd 0 1] ++ shrink_capcap cs2 cs1 rd
shrink_yss cs2 cs1 rd = [addi rd 0 0, addi rd 0 1] ++ shrink_capcap cs2 cs1 rd

rv32_xcheri_shrink :: [DecodeBranch [Instruction]]
rv32_xcheri_shrink = [ ypermr_raw       --> shrink_ypermr
                     , ytyper_raw       --> shrink_ytyper
                     , ybaser_raw       --> shrink_ybaser
                     , ylenr_raw        --> shrink_ylenr
                     , ytopr_raw        --> shrink_ytopr
                     , ytagr_raw        --> shrink_ytagr
                     , srliy_raw        --> shrink_srliy
                     , ymoder_raw       --> shrink_ymoder
                     , ypermc_raw       --> shrink_capint
                     , yaddrw_raw       --> shrink_capint
                     , packy_raw        --> shrink_capint
                     --, ymv_raw          --> noshrink_cap -- Ensure this is above yadd
                     , yadd_raw         --> shrink_capint
                     , ybndsrw_raw      --> shrink_capint
                     , ybndsw_raw       --> shrink_capint
                     , ybld_raw         --> shrink_capcap
                     , ysunseal_raw     --> shrink_capcap
                     , ysentry_raw      --> shrink_cap
                     , yaddi_raw        --> shrink_capimm
                     , ybndswi_raw      --> shrink_ybndswi
                     , yeq_raw          --> shrink_yeq
                     , yss_raw          --> shrink_yss
--                   , yamask_raw         --> noshrink
                     , ymodew_raw       --> shrink_capcap
--                   , sy_raw           --> noshrink
--                   , ly_raw           --> noshrink
                     ]

-- | List of cheri inspection instructions
rv32_xcheri_inspection :: Integer -> Integer -> [Instruction]
rv32_xcheri_inspection src dest = [ ypermr dest src
                                  , ytyper dest src
                                  , ybaser dest src
                                  , ylenr  dest src
                                  , ytopr  dest src
                                  , ytagr  dest src
                                  , ymoder dest src
                                  , yamask dest src]

-- | List of cheri arithmetic instructions
rv32_xcheri_arithmetic :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_arithmetic src1 src2 imm dest =
  [ yaddrw              dest src1 src2
  , packy               dest src1 src2
  --, yadd                dest src1 src2
  , ybndsrw             dest src1 src2
  , ybndsw              dest src1 src2
  , ybndswi             dest src1 imm
  , yaddi               dest src1 imm
  , yeq                 dest src1 src2
  , srliy               dest src1
  , yss                 dest src1 src2 ]

-- | List of cheri miscellaneous instructions
rv32_xcheri_misc :: Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_misc src1 src2 imm dest =
  [ ypermc      dest src1 src2
  , ymodew      dest src1 src2
  , ybld        dest src1 src2
  , ysunseal    dest src1 src2
  , ysentry     dest src1
  --, ymv         dest src1
  ]

-- | List of cheri control instructions
rv32_xcheri_control :: Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_control src1 src2 dest = [ ymodeswy
                                     , ymodeswi]

-- | List of cheri memory instructions
rv32_xcheri_mem :: ArchDesc -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri_mem    arch srcAddr srcData imm dest =
  [ ly dest srcAddr         imm
  , sy      srcAddr srcData imm
  ]
  ++ [ ly    dest srcAddr      imm
  ,    ytagr dest dest ]

-- | List of cheri memory instructions
rv32_a_xcheri :: Integer -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_a_xcheri      srcAddr srcData dest aq rl =
  [ lr_b dest srcAddr aq rl
  , sc_b dest srcAddr srcData aq rl
  , lr_h dest srcAddr aq rl
  , sc_h dest srcAddr srcData aq rl
  , lr_y dest srcAddr aq rl
  , sc_y dest srcAddr srcData aq rl
  ]

-- | List of cheri instructions
rv32_xcheri :: ArchDesc -> Integer -> Integer -> Integer -> Integer -> [Instruction]
rv32_xcheri arch src1 src2 imm dest =
     rv32_xcheri_inspection src1 dest
  ++ rv32_xcheri_arithmetic src1 src2 imm dest
  ++ rv32_xcheri_misc src1 src2 imm dest
  ++ rv32_xcheri_control src1 src2 dest
  ++ rv32_xcheri_mem arch src1 src2 imm dest
