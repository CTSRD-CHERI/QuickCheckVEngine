--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2025 Peter Rugg
-- All rights reserved.
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
    Module      : QuickCheckVEngine.Stats
    Description : Utils to accumulate stats for rough coverage metrics
-}

module QuickCheckVEngine.Stats (
-- * Types
  Stats
-- * Constructor
, emptyStats
-- * Trace processing
, statTrace
) where

import Data.Map(empty,member,Map,alter,toList)
import Data.Maybe
import Data.List.Split(splitOn)
import Data.List(sortOn)
import RISCV.InstInspect
import InstrCodec
import QuickCheckVEngine.TestTypes
import QuickCheckVEngine.RVFI_DII
import Text.Layout.Table

data Stats = MkStats (Map String (Int, Int))

emptyStats = MkStats empty

reportToVal (Just r) = if rvfiIsTrap r then (0,1) else (1,0)
reportToVal Nothing = (0,1)

reportToKey (Just r) = head $ splitOn " " (rv_pretty (MkInstruction (toInteger (rvfi_insn r))) Nothing)
reportToKey Nothing = "None"

statTrace :: Test (Maybe RVFI_Packet) -> Stats -> Stats
statTrace t (MkStats s) = MkStats $ foldl (\d r -> alter (comb (reportToVal r)) (reportToKey r) d) s t
                where comb v Nothing = Just v
                      comb (a,b) (Just (a',b')) = Just (a+a',b+b')

instance Show Stats where
  show (MkStats s) = tableString $ columnHeaderTableS columnSpec asciiS headers ((rowG . extract) <$> (sortRows $ toList s))
                     where extract (k, (a, b)) = [k                         , show a  , show b]
                           headers =     titlesH ["Insn"                    , "notrap", "trap"]
                           columnSpec =          [column expand left def def, numCol  , numCol]
                           sortRows = reverse . sortOn sortKey
                           sortKey (_, (a, b)) = a+b
