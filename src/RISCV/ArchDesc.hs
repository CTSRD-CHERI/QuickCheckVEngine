--
-- SPDX-License-Identifier: BSD-2-Clause
--
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
    Module      : RISCV.ArchDesc
    Description : RISC-V architectural configuration

    The 'RISCV.ArchDesc' module provides helpers to represent a RISC-V
    architectural configuration as a set of 'Bool' options
-}

module RISCV.ArchDesc (
  ArchDesc(..)
, archDesc_null
, archDesc_rv32i
, fromString
) where

import Text.Regex.Posix ((=~))
import Data.List
import Data.List.Split (splitOneOf)
import Data.Char (toLower)

-- | The 'ArchDesc' type is a record type with 'Bool' fields indicating the
--   presence of a RISC-V extension. It can be used to select a subset of
--   instructions to generate when used in functions to generate 'Template's.
data ArchDesc = ArchDesc { has_xlen_32 :: Bool
                         , has_xlen_64 :: Bool
                         , has_i       :: Bool
                         , has_m       :: Bool
                         , has_s       :: Bool
                         , has_a       :: Bool
                         , has_f       :: Bool
                         , has_d       :: Bool
                         , has_c       :: Bool
                         , has_n       :: Bool
                         , has_icsr    :: Bool
                         , has_ifencei :: Bool
                         , has_cheri   :: Bool }

-- | The 'Show' instance for 'ArchDesc' renders an approximation of a RISC-V
--   archstring
instance Show ArchDesc where
  show a = "rv" ++ intercalate "_" [ x | x <- [ ext has_xlen_32 "32"
                                              , ext has_xlen_64 "64" ]
                                       , not $ null x ]
                ++ ext has_i "i"
                ++ ext has_m "m"
                ++ ext has_s "s"
                ++ ext has_a "a"
                ++ ext has_f "f"
                ++ ext has_d "d"
                ++ ext has_c "c"
                ++ ext has_n "n"
                ++ intercalate "_" [ x | x <- [ ext has_icsr "Zicsr"
                                              , ext has_ifencei "Zifencei"
                                              , ext has_cheri "Xcheri" ]
                                       , not $ null x ]
            where ext pred str = if pred a then str else ""

-- | 'archDesc_null' is an 'ArchDesc' with all its fields set to 'False'
archDesc_null  = ArchDesc { has_xlen_32 = False
                          , has_xlen_64 = False
                          , has_i       = False
                          , has_m       = False
                          , has_s       = False
                          , has_a       = False
                          , has_f       = False
                          , has_d       = False
                          , has_c       = False
                          , has_n       = False
                          , has_icsr    = False
                          , has_ifencei = False
                          , has_cheri   = False
                          }

-- | 'archDesc_rv32i' is an 'ArchDesc' with its 'has_xlen_32' and 'has_i' fields
--   set to 'True' and all others set to 'False'
archDesc_rv32i = ArchDesc { has_xlen_32 = True
                          , has_xlen_64 = False
                          , has_i       = True
                          , has_m       = False
                          , has_s       = False
                          , has_a       = False
                          , has_f       = False
                          , has_d       = False
                          , has_c       = False
                          , has_n       = False
                          , has_icsr    = False
                          , has_ifencei = False
                          , has_cheri   = False
                          }

-- | 'fromString' expects a valid RISC-V archstring and turns it into an
--   'ArchDesc'
fromString :: String -> ArchDesc
fromString str = ArchDesc { has_xlen_32 = True
                          , has_xlen_64 = rv64
                          , has_i       = i
                          , has_m       = m
                          , has_s       = s
                          , has_a       = a
                          , has_f       = f
                          , has_d       = d
                          , has_c       = c
                          , has_n       = n
                          , has_icsr    = icsr
                          , has_ifencei = ifencei
                          , has_cheri   = cheri
                          }
  where rawSplit = splitOneOf "_zx" (map toLower str)
        archStrings = filter (\x -> not $ null x) rawSplit
        rv64 = (head archStrings) =~ "rv64"
        i = (head archStrings =~ "i") || (head archStrings =~ "g")
        m = (head archStrings =~ "m") || (head archStrings =~ "g")
        s = (head archStrings =~ "s") || (head archStrings =~ "g")
        a = (head archStrings =~ "a") || (head archStrings =~ "g")
        f = (head archStrings =~ "f") || (head archStrings =~ "g")
        d = (head archStrings =~ "d") || (head archStrings =~ "g")
        icsr = elem "icsr" archStrings || (head archStrings =~ "g")
        ifencei = elem "ifencei" archStrings || (head archStrings =~ "g")
        c = head archStrings =~ "c"
        n = head archStrings =~ "n"
        cheri = elem "cheri" archStrings
