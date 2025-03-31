--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2020-2021 Alexandre Joannou
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

module QuickCheckVEngine.Templates.Utils.HPM (
  setupHPMEventSel
, triggerHPMCounter
, inhibitHPMCounter
, enableHPMCounterM
, disableHPMCounterM
, enableHPMCounterS
, disableHPMCounterS
, readHPMCounter
, readHPMCounterM
, writeHPMCounterM
, surroundWithHPMAccess
, surroundWithUHPMAccess_core
, surroundWithHPMAccess_core
) where

import QuickCheckVEngine.Template
import QuickCheckVEngine.Templates.Utils.General
import RISCV ( HPMCounterIdx
             , HPMEventIdx
             , HPMEventSelCSRIdx
             , hpmcounter_idx_to_counter_csr_idx
             , hpmcounter_idx_to_mcounter_csr_idx
             , hpmcounter_idx_to_mevent_sel_csr_idx
             , hpmcounter_indices
             , hpmevent_indices
             , unsafe_csrs_indexFromName
             )
import Data.Bits
import Test.QuickCheck

-- | Sets up the provided HPM counter to count the provided HPM event
--   (using the provided temporary)
setupHPMEventSel :: Integer -> HPMEventSelCSRIdx -> HPMEventIdx -> Template
setupHPMEventSel tmpReg sel evt = li32 tmpReg evt <> csrw csrIdx tmpReg
  where csrIdx = hpmcounter_idx_to_mevent_sel_csr_idx sel

-- | Trigger the provided HPM counter
triggerHPMCounter :: Integer -> HPMCounterIdx -> Template
triggerHPMCounter tmpReg idx = csrBitSetOrClear False mcountinhibit idx tmpReg
  where mcountinhibit = unsafe_csrs_indexFromName "mcountinhibit"

-- | Inhibit the provided HPM counter
inhibitHPMCounter :: Integer -> HPMCounterIdx -> Template
inhibitHPMCounter tmpReg idx = csrBitSetOrClear True mcountinhibit idx tmpReg
  where mcountinhibit = unsafe_csrs_indexFromName "mcountinhibit"

-- | Enable the provided HPM counter's accessibility from less privileged modes
--   than M
enableHPMCounterM :: Integer -> HPMCounterIdx -> Template
enableHPMCounterM tmpReg idx = csrBitSetOrClear True mcounteren idx tmpReg
  where mcounteren = unsafe_csrs_indexFromName "mcounteren"

-- | Disable the provided HPM counter's accessibility from less privileged modes
--   than M
disableHPMCounterM :: Integer -> HPMCounterIdx -> Template
disableHPMCounterM tmpReg idx = csrBitSetOrClear False mcounteren idx tmpReg
  where mcounteren = unsafe_csrs_indexFromName "mcounteren"

-- | Enable the provided HPM counter's accessibility from less privileged modes
--   than S
enableHPMCounterS :: Integer -> HPMCounterIdx -> Template
enableHPMCounterS tmpReg idx = csrBitSetOrClear True mcounteren idx tmpReg
  where mcounteren = unsafe_csrs_indexFromName "scounteren"

-- | Disable the provided HPM counter's accessibility from less privileged modes
--   than S
disableHPMCounterS :: Integer -> HPMCounterIdx -> Template
disableHPMCounterS tmpReg idx = csrBitSetOrClear False mcounteren idx tmpReg
  where mcounteren = unsafe_csrs_indexFromName "scounteren"

-- | Read the provided HPM counter into the provided destination register
readHPMCounter :: Integer -> HPMCounterIdx -> Template
readHPMCounter rd idx = csrr rd csrIdx
  where csrIdx = hpmcounter_idx_to_counter_csr_idx idx

-- | Read the provided HPM counter into the provided destination register
readHPMCounterM :: Integer -> HPMCounterIdx -> Template
readHPMCounterM rd idx = csrr rd csrIdx
  where csrIdx = hpmcounter_idx_to_mcounter_csr_idx idx

-- | Write the provided general purpose register's value into the provided
--   HPM counter
writeHPMCounterM :: HPMCounterIdx -> Integer -> Template
writeHPMCounterM idx rs1 = csrw csrIdx rs1
  where csrIdx = hpmcounter_idx_to_mcounter_csr_idx idx

-- | Clear the provided HPM counter
resetHPMCounterM :: Integer -> HPMCounterIdx -> Template
resetHPMCounterM tmp idx = mconcat [ li32 tmpNonZero 0
                                   , csrw csrIdx tmpNonZero ]
  where csrIdx = hpmcounter_idx_to_mcounter_csr_idx idx
        tmpNonZero = max 1 tmp

-- | Clear the provided HPM counter
resetHPMCounter :: Integer -> HPMCounterIdx -> Template
resetHPMCounter tmp idx = mconcat [ li32 tmpNonZero 0
                                  , csrw csrIdx tmpNonZero ]
  where csrIdx = hpmcounter_idx_to_counter_csr_idx idx
        tmpNonZero = max 1 tmp


surroundWithUHPMAccess_core :: Bool -> HPMEventIdx -> Template -> Integer -> Integer -> Template
surroundWithUHPMAccess_core shrink evt x tmpReg hpmCntIdx = random $ do
  let prologue = resetHPMCounter tmpReg hpmCntIdx
  let epilogue = readHPMCounter  tmpReg hpmCntIdx
  return $ if shrink then prologue <> x <> epilogue
                     else shrinkScope $ noShrink prologue <> x <> epilogue

-- | 'surroundWithHPMAccess' wraps a 'Template' by setting up an HPM counter to
--   count an event and before running the 'Template' and reading the HPM
--   counter's value after
surroundWithHPMAccess :: Template -> Template
surroundWithHPMAccess x = random $ do
  evt <- oneof $ map return hpmevent_indices
  hpmCntIdx <- oneof $ map return hpmcounter_indices
  tmpReg <- dest
  return $ surroundWithHPMAccess_core False evt x tmpReg hpmCntIdx Nothing

-- | inner helper

surroundWithHPMAccess_core :: Bool -> HPMEventIdx -> Template -> Integer -> Integer -> Maybe (Integer, Integer)
                           -> Template
surroundWithHPMAccess_core shrink evt x tmpReg hpmCntIdx instRet = random $ do
  let prologue =    inhibitHPMCounter tmpReg hpmCntIdx
                 <> setupHPMEventSel tmpReg hpmCntIdx evt
                 <> resetHPMCounterM tmpReg hpmCntIdx
                 <> uniform [enableHPMCounterM tmpReg hpmCntIdx, mempty]
                 <> uniform [enableHPMCounterS tmpReg hpmCntIdx, mempty]
                 <> triggerHPMCounter tmpReg hpmCntIdx
  let epilogue = uniform [ readHPMCounter  tmpReg hpmCntIdx
                                 , readHPMCounterM tmpReg hpmCntIdx ]
  return $ if shrink then prologue <> surroundWithHPMAccess_raw x instRet <> epilogue
                     else shrinkScope $ noShrink prologue <> surroundWithHPMAccess_raw x instRet <> noShrink epilogue


surroundWithHPMAccess_raw :: Template -> Maybe (Integer, Integer) -> Template
surroundWithHPMAccess_raw x instRet = case (instRet) of
  Just (reg1, reg2) -> shrinkScope $ ((noShrink (csrr reg1 (unsafe_csrs_indexFromName "minstret"))) <> x <> (noShrink (csrr reg2 (unsafe_csrs_indexFromName "minstret"))))
  Nothing -> x
