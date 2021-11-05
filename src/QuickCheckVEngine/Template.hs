{-# LANGUAGE RankNTypes #-}
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
  Template
, emptyTemplate
, sequenceTemplate
, randomTemplate
, distTemplate
, uniformTemplate
, replicateTemplate
, repeatTemplateTillEnd
, assertLastVal
, assertSingle
, inst
, instSeq
, instDist
, instUniform
, noShrink
, shrinkUnit
, (<>)
, Test
, mapWithAssertLastVal
, genTest
) where

import Test.QuickCheck
import Data.Semigroup
import RISCV (Instruction)
import Data.Kind
import Control.Applicative
import Text.Printf

data Report = ReportAssert Bool String

instance Show Report where
  show (ReportAssert b s) = (if b then "Passed" else "Failed") ++ " assert: " ++ s

data MetaInfo r = MetaShrink -- XXX TODO add api for MetaShrink
                | MetaNoShrink
                | MetaCustomShrink (r -> [Test Instruction]) -- XXX TODO should be a test of r as input to allow bypass shrinking
                | MetaAssertLastVal Integer
                | MetaAssertCompound (Test r -> (Bool, String))
                | MetaReport Report

data Template r = TemplateEmpty
                | TemplateSingle Instruction
                | TemplateSequence Template Template
                | TemplateMeta (MetaInfo r) Template
                | TemplateRandom (Gen Template)

instance Semigroup Template where
  x <> y = TemplateSequence x y
instance Monoid Template where
  mempty = TemplateEmpty
  mappend = (<>)

-- Template API --

emptyTemplate :: Template
emptyTemplate = mempty

sequenceTemplate :: [Template] -> Template
sequenceTemplate = mconcat

randomTemplate :: Gen Template -> Template
randomTemplate = TemplateRandom

distTemplate :: [(Int, Template)] -> Template
distTemplate xs = TemplateRandom $ frequency $ map (\(a, b) -> (a, return b)) xs

uniformTemplate :: [Template] -> Template
uniformTemplate = distTemplate . map ((,) 1)

replicateTemplate :: Int -> Template -> Template
replicateTemplate n t = mconcat $ replicate n t

-- | Note that this requires the argument to always return a list of length 1
repeatTemplateTillEnd :: Template -> Template
repeatTemplateTillEnd t = TemplateRandom $ replicateTemplate <$> getSize <*> pure t

assertLastVal :: Template -> Integer -> Template
assertLastVal t v = TemplateMeta (MetaAssertLastVal v) t

assertSingle :: Instruction -> Integer -> Template
assertSingle i v = assertLastVal (TemplateSingle i) v

inst :: Instruction -> Template
inst = TemplateSingle

instSeq :: [Instruction] -> Template
instSeq = mconcat . map TemplateSingle

instDist :: [(Int, Instruction)] -> Template
instDist = distTemplate . map (\(a, b) -> (a, TemplateSingle b))

instUniform :: [Instruction] -> Template
instUniform = instDist . map ((,) 1)

noShrink :: Template -> Template
noShrink = TemplateMeta MetaNoShrink

shrinkUnit :: Template -> Template
shrinkUnit = TemplateMeta MetaShrink

--------------------------------------------------------------------------------

data Test r t = TestEmpty
              | TestSingle t
              | TestSequence (Test r t) (Test r t)
              | TestMeta (MetaInfo r) (Test r t)

instance Semigroup (Test r t) where
  x <> y = TestSequence x y
instance Monoid (Test r t) where
  mempty = TestEmpty
  mappend = (<>)
instance Functor (Test r) where
  fmap f TestEmpty = TestEmpty
  fmap f (TestSingle x) = TestSingle $ f x
  fmap f (TestSequence x y) = TestSequence (fmap f x) (fmap f y)
  fmap f (TestMeta m x) = TestMeta m (fmap f x)
instance Foldable (Test r) where
  foldr _ z TestEmpty = z
  foldr f z (TestSingle x) = f x z
  foldr f z (TestSequence x y) = foldr f (foldr f z y) x
  foldr f z (TestMeta _ x) = foldr f z x
instance Traversable (Test r) where
  traverse _ TestEmpty = pure TestEmpty
  traverse f (TestSingle x) = TestSingle <$> f x
  traverse f (TestSequence x y) = liftA2 TestSequence (traverse f x) (traverse f y)
  traverse f (TestMeta m x) = TestMeta m <$> traverse f x
instance Show t, Integral t => Show (Test r t) where
  show TestEmpty = ""
  show (TestSingle x) = printf ".4byte 0x%08x # %s" (toInteger x) (show x)
  show (TestSequence x y) = show x ++ "\n" ++ show y
  show (TestMeta MetaShrink x) = "#>START_SHRINK\n" ++ show x ++ "\n#>END_SHRINK"
  show (TestMeta MetaNoShrink x) = "#>START_NOSHRINK\n" ++ show x ++ "\n#>END_NOSHRINK"
  show (TestMeta (MetaCustomShrink _) x) = show x -- Cannot serialise function
  show (TestMeta (MetaAssertLastVal v) x) = printf "%s\n#>ASSERT rd_wdata == 0x%x" (show x) v
  show (TestMeta (MetaAssertCompound _) x) = show x -- Cannot serialise function
  show (TestMeta (MetaReport r) x) = "# REPORT     '" ++ show r ++ "' {\n" ++ show x ++ "\n# } END REPORT '" ++ show r ++ "'"

-- XXX TODO won't work if there are empties at the end of the tree
mapWithAssertLastVal :: (Maybe Integer -> a -> b) -> Test r a -> Test r b
mapWithAssertLastVal = go Nothing
  where go _ _ TestEmpty = TestEmpty
        go v f (TestSingle x) = TestSingle (f v x)
        go v f (TestSequence x y) = TestSequence (go Nothing f x) (go v f y)
        go _ f (TestMeta m@(MetaAssertLastVal v) x) = TestMeta m (go (Just v) f x)
        go v f (TestMeta m x) = TestMeta m (go v f x)

runAsserts :: Test r r -> (Test r r, [(Bool, String)])
runAsserts TestEmpty = TestEmpty
runAsserts (TestSingle x) = TestSingle Nothing
runAsserts (TestSequence x y) = TestSequence (assertCompound x) (assertCompound y)
-- TODO Fold in mapWithAssertLastVal here somehow?
runAsserts (TestMeta (MetaAssertCompound f) x) = TestMeta m' x'
  where m' = MetaReport $ uncurry ReportAssert (f x)
        x' = runAssertCompound x
runAsserts (TestMeta m x) = TestMeta m (runAsserts x)

gatherReports :: Test r t -> [(Report, Test r t)]
gatherReports (TestSequence x y) = gatherReports x ++ gatherReports y
gatherReports (TestMeta (MetaReport r) x) = (r, x) : gatherReports x
gatherReports (TestMeta _ x) = gatherReports x
gatherReports _ = []

singleTest :: t -> (forall r. Test r t)
singleTest = TestSingle

genTest :: Template r -> Gen (Test r Instruction)
genTest TemplateEmpty = return TestEmpty
genTest (TemplateSingle x) = return $ TestSingle x
genTest (TemplateSequence x y) = TestSequence <$> genTest x <*> genTest y
genTest (TemplateMeta m x) = TestMeta m <$> genTest x
genTest (TemplateRandom g) = genTest =<< g

instance Arbitrary (Test r r) where
  arbitrary = return TestEmpty
  shrink = shrinkHelper (const [])
    where shrinkHelper :: (r -> [Test Instruction]) -> Test r r -> [Test r r]
          shrinkHelper _ TestEmpty = []
          shrinkHelper f (TestSingle x) = TestEmpty : f x
          shrinkHelper f (TestSequence x y) =
            let xs = shrinkHelper f x
                ys = shrinkHelper f y
            in    [TestSequence x' y  | x' <- xs]
               ++ [TestSequence x  y' | y' <- ys]
          shrinkHelper f (TestMeta MetaShrink t) = TestEmpty : shrinkHelper f t
          shrinkHelper _ (TestMeta MetaNoShrink _) = []
          shrinkHelper _ (TestMeta (MetaCustomShrink f) t) = shrinkHelper f t
          shrinkHelper f (TestMeta m t) = TestMeta m <$> shrinkHelper f t
