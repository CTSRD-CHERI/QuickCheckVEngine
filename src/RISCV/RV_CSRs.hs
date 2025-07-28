--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019-2021 Alexandre Joannou
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
    Module      : RISCV.RV_CSRs
    Description : RISC-V control and status register helpers

    The 'RISCV.RV_CSRs' module provides some helpers to reference RISC-V CSRs
-}

module RISCV.RV_CSRs (
  CSRIdx
, CSRName
, HPMCounterIdx
, HPMCounterCSRIdx
, HPMEventSelCSRIdx
, hpmcounter_idx_to_counter_csr_idx
, hpmcounter_idx_to_mcounter_csr_idx
, hpmcounter_idx_to_mevent_sel_csr_idx
, hpmcounter_indices
, hpmcounter_csr_indices
, mhpmcounter_csr_indices
, mhpmevent_csr_indices
, csrs_map
, csrs_indexFromName
, unsafe_csrs_indexFromName
, csrs_nameFromIndex
) where

import Data.Maybe

-- | CSRIdx type
type CSRIdx = Integer
-- | CSRName type
type CSRName = String
-- | HPMCounterIdx type
type HPMCounterIdx = Integer
-- | HPMCounterCSRIdx type
type HPMCounterCSRIdx = Integer
-- | HPMEventSelCSRIdx type
type HPMEventSelCSRIdx = Integer

-- | the number of supported HPM counters
nHPMCounters = 29

-- | Turns an 'HPMCounterIdx' into an 'HPMCounterCSRIdx' for hpmcounter
hpmcounter_idx_to_counter_csr_idx :: HPMCounterIdx -> HPMCounterCSRIdx
hpmcounter_idx_to_counter_csr_idx idx
  |    (fromInteger idx >= head hpmcounter_indices)
    && (fromInteger idx < (fromInteger (head hpmcounter_indices) + nHPMCounters)) =
      hpmcounter_csr_indices !! fromInteger (idx - head hpmcounter_indices)
  | otherwise = error $ "hpmcounter_idx_to_counter_csr_idx idx is out of bounds: " ++ show idx

-- | Turns an 'HPMCounterIdx' into an 'HPMCounterCSRIdx' for mhpmcounter
hpmcounter_idx_to_mcounter_csr_idx :: HPMCounterIdx -> HPMCounterCSRIdx
hpmcounter_idx_to_mcounter_csr_idx idx
  |    (fromInteger idx >= head hpmcounter_indices)
    && (fromInteger idx < (fromInteger (head hpmcounter_indices) + nHPMCounters)) =
      mhpmcounter_csr_indices !! fromInteger (idx - head hpmcounter_indices)
  | otherwise = error $ "hpmcounter_idx_to_counter_csr_idx idx is out of bounds: " ++ show idx

-- | Turns an 'HPMCounterIdx' into an 'HPMEventSelCSRIdx'
hpmcounter_idx_to_mevent_sel_csr_idx :: HPMCounterIdx -> HPMEventSelCSRIdx
hpmcounter_idx_to_mevent_sel_csr_idx idx
  |    (fromInteger idx >= head hpmcounter_indices)
    && (fromInteger idx < (fromInteger (head hpmcounter_indices) + nHPMCounters)) =
      mhpmevent_csr_indices !! fromInteger (idx - head hpmcounter_indices)
  | otherwise = error $ "hpmcounter_idx_to_counter_csr_idx idx is out of bounds: " ++ show idx

-- | Return the list of available existing hpmcounter indices
hpmcounter_indices :: [HPMCounterIdx]
hpmcounter_indices = take nHPMCounters [ 3 .. ]

-- | Return the list of available existing hpmcounter CSR indices
hpmcounter_csr_indices :: [HPMCounterCSRIdx]
hpmcounter_csr_indices = take nHPMCounters [ 0xC03 .. ]

-- | Return the list of available existing mhpmcounter CSR indices
mhpmcounter_csr_indices :: [HPMCounterCSRIdx]
mhpmcounter_csr_indices = take nHPMCounters [ 0xB03 .. ]

-- | Return the list of available existing hpmevent selector CSR indices
mhpmevent_csr_indices :: [HPMEventSelCSRIdx]
mhpmevent_csr_indices = take nHPMCounters [ 0x323 .. ]

-- | Return 'Just' a CSR index for a known CSR name or 'Nothing'
csrs_indexFromName :: CSRName -> Maybe CSRIdx
csrs_indexFromName nm = lookup nm [ (b, a) | (a, b) <- csrs_map ]

-- | Return a CSR index for a known CSR name or an error
unsafe_csrs_indexFromName :: CSRName -> CSRIdx
unsafe_csrs_indexFromName nm = fromMaybe (error $ "unknown CSR name: " ++ nm)
                                         (csrs_indexFromName nm)

-- | Return 'Just' a CSR name for a known index or 'Nothing'
csrs_nameFromIndex :: CSRIdx -> Maybe CSRName
csrs_nameFromIndex idx = lookup idx csrs_map

-- | List of CSRs' (index, name) tuples
csrs_map :: [(CSRIdx, CSRName)]
csrs_map = -- User Thread ID
           [ (0x480, "utid")]
        ++ -- User Floating-Point CSRs
           [ (0x001, "fflags")
           , (0x002, "frm")
           , (0x003, "fcsr") ]
        ++ -- User Counters/Timers
           [ (0xC00, "cycle")
           , (0xC01, "time")
           , (0xC02, "instret") ]
        ++ [ (0xC00 + x, "hpmcounter" ++ show x) | x <- hpmcounter_indices ]
        ++ [ (0xC80, "cycleh")
           , (0xC81, "timeh")
           , (0xC82, "instreth") ]
        ++ [ (0xC80 + x, "hpmcounter" ++ show x ++ "h") | x <- hpmcounter_indices ]
        ++ -- Supervisor Trap Setup
           [ (0x100, "sstatus")
           , (0x102, "sedeleg")
           , (0x103, "sideleg")
           , (0x104, "sie")
           , (0x105, "stvec")
           , (0x106, "scounteren") ]
        ++ -- Supervisior Config
           [ (0x10a, "senvcfg") ]
        ++ -- Supervisor Trap Handling
           [ (0x140, "sscratch")
           , (0x141, "sepc")
           , (0x580, "stid")
           , (0x142, "scause")
           , (0x143, "stval")
           , (0x14B, "stval2")
           , (0x144, "sip") ]
        ++ -- Supervisor Protection and Translation
           [ (0x180, "satp") ]
        -- TODO Hypervisor CSRs
        ++ -- Machine Information Registers
           [ (0xF11, "mvendorid")
           , (0xF12, "marchid")
           , (0xF13, "mimpid")
           , (0xF14, "mhartid")
           , (0xF15, "mconfigptr") ]
        ++ -- Machine Trap Setup
           [ (0x300, "mstatus")
           , (0x301, "misa")
           , (0x302, "medeleg")
           , (0x303, "mideleg")
           , (0x304, "mie")
           , (0x305, "mtvec")
           , (0x306, "mcounteren")
           , (0x310, "mstatush") ]
        ++ -- Machine Config
           [ (0x30A, "menvcfg")
           , (0x31A, "menvcfgh")
           , (0x747, "mseccfg")
           , (0x757, "mseccfgh") ]
        ++ -- Machine Trap Handling
           [ (0x340, "mscratch")
           , (0x341, "mepc")
           , (0x780, "mtid")
           , (0x342, "mcause")
           , (0x343, "mtval")
           , (0x34B, "mtval2")
           , (0x344, "mip") ]
        ++ -- Machine Memory Protection
           [ (0x3A0 + x, "pmpcfg" ++ show x) | x <- [0..3] ]
        ++ [ (0x3B0 + x, "pmpaddr" ++ show x) | x <- [0..15] ]
        ++ -- Machine Counters/Timers
           [ (0xB00, "mcycle")
           , (0xB02, "minstret") ]
        ++ map (\x -> (x, "mhpmcounter" ++ show (x - (head mhpmcounter_csr_indices) + 3)))
               mhpmcounter_csr_indices
        ++ [ (0xB80, "mcycleh")
           , (0xB82, "minstreth") ]
        ++ [ (0xB80 + x, "mhpmcounter" ++ show x ++ "h") | x <- hpmcounter_indices ]
        ++ -- Machine Counter Setup
           [ (0x320, "mcountinhibit") ]
        ++ -- CHERI CSRs
           [ (0x8C0, "uccsr")
           , (0x9C0, "sccsr")
           , (0xBC0, "mccsr") ]
        ++ map (\x -> (x, "mhpmevent" ++ show (x - (head mhpmevent_csr_indices) + 3)))
               mhpmevent_csr_indices
        -- TODO Debug/Trace Registers (shared with Debug Mode)
        -- TODO Debug Mode Registers
        -- List last checked from:
        -- The RISC-V Instruction Set Manual
        -- Volume II: Privileged Architecture
        -- Document Version 1.12-draft
        -- September 13, 2019
