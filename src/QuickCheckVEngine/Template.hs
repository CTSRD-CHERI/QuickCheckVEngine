{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
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
    Module      : QuickCheckVEngine.Template
    Description : Helpers to define RISC-V tests

    The 'QuickCheckVEngine.Template' module provides helpers to express RISC-V
    tests
-}

module QuickCheckVEngine.Template (
-- * Types
  Template
-- * Template API
, module Data.Semigroup -- XXX Remove once everyone has a newer compiler
, random
, dist
, uniform
, repeatN
, repeatTillEnd
-- ** Assertions
, assertLastVal
, assertCompound
-- ** Shrinking information
, noShrink
, shrinkScope
, shrinkStrategy
-- ** Templates for explicit instructions
, inst
, instSeq
, instDist
, instUniform
, instAssert
-- ** Generate Test from Template
, genTest
) where

import Test.QuickCheck
import Data.Semigroup (Semigroup(..))
import RISCV (Instruction(..))
--import Data.Kind
--import Control.Applicative (liftA2)
--import Text.Printf
--import QuickCheckVEngine.RVFI_DII
--import Text.Parsec hiding (parseTest)
--import Text.Parsec.Token
--import Text.Parsec.Language
--import Control.Monad
import QuickCheckVEngine.TestTypes

data Template = TemplateEmpty
              | TemplateSingle Instruction
              | TemplateSequence Template Template
              | TemplateMeta MetaInfo Template
              | TemplateRandom (Gen Template)

instance Semigroup Template where
  x <> y = TemplateSequence x y
instance Monoid Template where
  mempty = TemplateEmpty
  mappend = (<>)

-- Template API --

random :: Gen Template -> Template
random = TemplateRandom

dist :: [(Int, Template)] -> Template
dist xs = TemplateRandom $ frequency $ map (\(a, b) -> (a, return b)) xs

uniform :: [Template] -> Template
uniform = dist . map ((,) 1)

repeatN :: Int -> Template -> Template
repeatN n t = mconcat $ replicate n t

-- | Note that this requires the argument to always return a Test of length 1
repeatTillEnd :: Template -> Template
repeatTillEnd t =
  TemplateRandom $ repeatN <$> getSize <*> pure t

assertLastVal :: Template -> Integer -> Template
assertLastVal t v = TemplateMeta (MetaAssertLastVal v) t

assertCompound :: Template -> (Test TestResult -> (Bool, String)) -> Template
assertCompound t f = TemplateMeta (MetaAssertCompound f) t

noShrink :: Template -> Template
noShrink = TemplateMeta MetaNoShrink

shrinkScope :: Template -> Template
shrinkScope = TemplateMeta MetaShrinkScope

shrinkStrategy :: Template -> ShrinkStrategy -> Template
shrinkStrategy x f = TemplateMeta (MetaShrinkStrategy f) x

inst :: Instruction -> Template
inst = TemplateSingle

instSeq :: [Instruction] -> Template
instSeq = mconcat . map TemplateSingle

instDist :: [(Int, Instruction)] -> Template
instDist = dist . map (\(a, b) -> (a, TemplateSingle b))

instUniform :: [Instruction] -> Template
instUniform = instDist . map ((,) 1)

instAssert :: Instruction -> Integer -> Template
instAssert i v = assertLastVal (TemplateSingle i) v

genTest :: Template -> Gen (Test Instruction)
genTest TemplateEmpty = return TestEmpty
genTest (TemplateSingle x) = return $ TestSingle x
genTest (TemplateSequence x y) = TestSequence <$> genTest x <*> genTest y
genTest (TemplateMeta m@(MetaShrinkStrategy _) x) =
  TestMeta (m, False) <$> genTest x
genTest (TemplateMeta m@(MetaAssertCompound _) x) =
  TestMeta (m, False) <$> genTest x
genTest (TemplateMeta m x) = TestMeta (m, True) <$> genTest x
genTest (TemplateRandom g) = genTest =<< g
