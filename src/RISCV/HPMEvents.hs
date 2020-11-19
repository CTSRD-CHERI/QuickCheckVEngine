--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2020 Alexandre Joannou
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
    Module      : RISCV.HPMEvents
    Description : RISC-V hardware performance monitor events

    The 'RISCV.HPMEvents' module provides the map of event name to event
    index used to configure the hardware performance monitor counters using the
    29 mhpmevent[3..31] CSRs.
-}

module RISCV.HPMEvents (
  hpmevent_map
, hpmevent_indexFromName
, hpmevent_nameFromIndex
) where

-- | Return 'Just' an hpmevent index for a known hpmevent name or 'Nothing'
hpmevent_indexFromName :: String -> Maybe Integer
hpmevent_indexFromName nm = lookup nm [ (b, a) | (a, b) <- hpmevent_map]

-- | Return 'Just' an hpmevent name for a known hpmevent index or 'Nothing'
hpmevent_nameFromIndex :: Integer -> Maybe String
hpmevent_nameFromIndex idx = lookup idx hpmevent_map

-- | List of hpmevents' (index, name) tuples
--   XXX Note: this list is derived from the document at
--   XXX https://github.com/CTSRD-CHERI/Flute/blob/CHERI/Doc/Performance_Monitor/Performance_Monitoring.md
--   XXX and should be updated/refined, and possibly contributed back when it
--   XXX comes to architectural events.
hpmevent_map :: [(Integer, String)]
hpmevent_map = -- Architecturally defined events
               [ (0, "noEvent") ]
            ++ -- Architectural events
               [ (1,  "pcRedirect")
               , (3,  "branchInst")
               , (4,  "jalInst")
               , (5,  "jalrInst")
               , (6,  "auipcInst")
               , (7,  "loadInst")
               , (8,  "storeInst")
               , (9,  "lrInst")
               , (10, "scInst")
               , (11, "amoInst")
               , (12, "shiftInst")
               , (13, "mulDivInst")
               , (14, "fpInst")
               , (18, "fenceInst")
               ]
            ++ -- Architectural CHERI events 
               [ (24, "cheriSetBoundsImprecise")
               , (25, "cheriUnrepCap")
               , (26, "cheriWideLoad")
               , (27, "cheriWideStore")
               , (28, "cheriCapLoad")
               , (29, "cheriCapStore")
               ]
            ++ -- Flute core specific events
               [ (15, "scSuccess")
               , (16, "loadWaitCycle")
               , (17, "storeWaitCycle")
               , (19, "FBusyNoConsume")
               , (20, "DBusyNoConsume")
               , (21, "1BusyNoConsume")
               , (22, "2BusyNoConsume")
               , (23, "3BusyNoConsume")
               ]
