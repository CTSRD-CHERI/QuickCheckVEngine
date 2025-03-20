--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2021 Peter Rugg
-- Copyright (c) 2021 Alexandre Joannou
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
    Module      : QuickCheckVEngine.TestTypes
    Description : Types used by Test and Template library (internal use only)
-}

module QuickCheckVEngine.TestTypes (
  Test(..)
, TestResult
, ShrinkStrategy
, Report(..)
, MetaInfo(..)
) where

import Data.Semigroup (Semigroup(..))

import Control.Applicative (liftA2)
import QuickCheckVEngine.RVFI_DII

type TestResult = (DII_Packet, Maybe RVFI_Packet, Maybe RVFI_Packet)

data Report = ReportAssert Bool String

instance Show Report where
  show (ReportAssert b s) =
    (if b then "Passed" else "Failed") ++ " assert: " ++ s

type ShrinkStrategy = Test TestResult -> [Test TestResult]

data MetaInfo = MetaShrinkStrategy ShrinkStrategy
              | MetaNoShrink
              | MetaShrinkScope
              | MetaAssertLastVal (RVFI_Packet -> Bool, String, Integer, String)
              | MetaAssertCompound (Test TestResult -> (Bool, String))
              | MetaReport Report

data Test t = TestEmpty
            | TestSingle t
            | TestSequence (Test t) (Test t)
            | TestMeta (MetaInfo, Bool) (Test t)

instance Eq t => Eq (Test t) where
  TestEmpty == TestEmpty = True
  TestSingle x == TestSingle y = x == y
  TestSequence ax bx == TestSequence ay by = ax == ay && bx == by
  TestMeta _ x == TestMeta _ y = x == y
  _ == _ = False
instance Ord t => Ord (Test t) where
  TestMeta _ x <= TestMeta _ y = x <= y
  TestEmpty <= _ = True
  _ <= TestEmpty = False
  TestSingle x <= TestSingle y = x <= y
  TestSingle _ <= _ = True
  _ <= TestSingle _ = False
  TestSequence ax bx <= TestSequence ay by = ax <= ay || (ax == ay && bx <= by)

instance Semigroup (Test t) where
  x <> y = TestSequence x y
instance Monoid (Test t) where
  mempty = TestEmpty
  mappend = (<>)
instance Functor Test where
  fmap f TestEmpty = TestEmpty
  fmap f (TestSingle x) = TestSingle $ f x
  fmap f (TestSequence x y) = TestSequence (fmap f x) (fmap f y)
  fmap f (TestMeta m x) = TestMeta m (fmap f x)
instance Foldable Test where
  foldr _ z TestEmpty = z
  foldr f z (TestSingle x) = f x z
  foldr f z (TestSequence x y) = foldr f (foldr f z y) x
  foldr f z (TestMeta _ x) = foldr f z x
instance Traversable Test where
  traverse _ TestEmpty = pure TestEmpty
  traverse f (TestSingle x) = TestSingle <$> f x
  traverse f (TestSequence x y) =
    liftA2 TestSequence (traverse f x) (traverse f y)
  traverse f (TestMeta m x) = TestMeta m <$> traverse f x
