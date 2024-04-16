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
, readParams
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
-- ** Test parameters
, TestParams(..)
) where

import Test.QuickCheck
import Data.Semigroup (Semigroup(..))
import RISCV (Instruction(..), CSRIdx)
import RISCV.ArchDesc
--import Data.Kind
--import Control.Applicative (liftA2)
--import Text.Printf
--import QuickCheckVEngine.RVFI_DII
--import Text.Parsec hiding (parseTest)
--import Text.Parsec.Token
--import Text.Parsec.Language
--import Control.Monad
import QuickCheckVEngine.TestTypes
import QuickCheckVEngine.RVFI_DII

-- | Micellaneous data indicating global parameters to
--   influence test generation, which should be passed
--   recursively to sub-templates.
data TestParams = TestParams { archDesc  :: ArchDesc
                             , csrFilter :: CSRIdx -> Bool }

data Template = TemplateEmpty
              | TemplateSingle Instruction
              | TemplateSequence Template Template
              | TemplateMeta MetaInfo Template
              | TemplateRandom (Gen Template)
              | TemplateReadParams (TestParams -> Template)

instance Semigroup Template where
  x <> y = TemplateSequence x y
instance Monoid Template where
  mempty = TemplateEmpty
  mappend = (<>)

-- Template API --

random :: Gen Template -> Template
random = TemplateRandom

readParams :: (TestParams -> Template) -> Template
readParams = TemplateReadParams

dist :: [(Int, Template)] -> Template
dist xs = TemplateRandom $ frequency $ map (\(a, b) -> (a, return b)) xs

uniform :: [Template] -> Template
uniform = dist . map ((,) 1)

repeatN :: Int -> Template -> Template
repeatN n t = mconcat $ replicate n t

repeatTillEnd :: Template -> Template
repeatTillEnd t = TemplateRandom $ do
  s <- getSize
  if s == 0 then
    return mempty
  else
    return $ (TemplateRandom $ resize s (pure t)) <> repeatTillEnd t

assertLastVal :: Template -> Integer -> Template
assertLastVal t v = TemplateMeta (MetaAssertLastVal (\r -> toInteger (rvfi_rd_wdata_or_zero r) == v, "rd_wdata", v, "")) t

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

-- Generate a Test from a Template, aiming to achieve the size
-- specified by quickcheck, splitting the size equally among
-- recursive Random template constructors
genTest :: TestParams -> Template -> Gen (Test Instruction)
genTest param x = (\(a,_,_) -> a) <$> (go x (countTemplate x)) where
  -- Determine the number of (static instructions, recursive random templates)
  -- there are in a template
  countTemplate TemplateEmpty = (0, 0)
  countTemplate (TemplateSingle x) = (1, 0)
  countTemplate (TemplateSequence x y) =
    (ix + iy, rx + ry)
    where (ix, rx) = countTemplate x
          (iy, ry) = countTemplate y
  countTemplate (TemplateMeta _ x) = countTemplate x
  countTemplate (TemplateRandom g) = (0, 1)
  countTemplate (TemplateReadParams f) = countTemplate (f param)
  -- Generate a test from a template, given the global number of static
  -- instructions and recursive calls. Also return the number of recursive
  -- calls resolved and the new instructions that resulted
  go :: Template -> (Int, Int) -> Gen (Test Instruction, Int, Int)
  go TemplateEmpty _ = return (TestEmpty, 0, 0)
  go (TemplateSingle x) _ = return (TestSingle x, 0, 0)
  go (TemplateSequence x y) (i, r) = do
    (x', ix, rx) <- go x (i, r)
    -- We now know how many instructions were generated in the left
    -- subtree. Generate the right subtree with the info to compensate
    -- for over/under usage in the left subtree
    (y', iy, ry) <- go y (i + ix, r - rx)
    return (TestSequence x' y', ix + iy, rx + ry)
  go (TemplateMeta m@(MetaShrinkStrategy _) x) c =
    (\(t, i, r) -> (TestMeta (m, False) t, i, r)) <$> go x c
  go (TemplateMeta m@(MetaAssertCompound _) x) c =
    (\(t, i, r) -> (TestMeta (m, False) t, i, r)) <$> go x c
  go (TemplateMeta m x) c =
    (\(t, i, r) -> (TestMeta (m, True) t, i, r)) <$> go x c
  go (TemplateRandom g) (i, r) = do
    -- Get the global target size
    s <- getSize
    -- Work out this template's quota, rounding up to avoid giving everyone
    -- a quota of zero
    let targetSize = max 0 ((s - i + r - 1) `div` r)
    -- Generate the recursive template
    g' <- resize targetSize g
    -- Work out the characteristics of the template
    let (i', r') = countTemplate g'
    -- Turn the elaborated template into a test
    (g'', i'', r'') <- resize targetSize $ go g' (i', r')
    -- The number of new dynamic instructions to return is the number of
    -- static + dynamic instructions in this sub-template. We have only
    -- evaluated one recursive call as far as the parent template is
    -- concerned.
    return (g'', i'' + i', 1)
  go (TemplateReadParams f) c = go (f param) c
