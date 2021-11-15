{-# LANGUAGE RankNTypes #-}
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
, ShrinkStrategy
, noShrink
, shrinkScope
, shrinkStrategy
, (<>)
, Test
, TestResult
, sequenceShrink
, singleShrink
, defaultShrink
, mapWithAssertLastVal
, runAsserts
, Report(..)
, gatherReports
, singleTest
, seqSingleTest
, noShrinkTest
, shrinkTestStrategy
, genTest
) where

import Test.QuickCheck
import Data.Semigroup (Semigroup(..))
import RISCV (Instruction(..))
import Data.Kind
import Control.Applicative (liftA2)
import Text.Printf
import QuickCheckVEngine.RVFI_DII
import Text.Parsec hiding (parseTest)
import Text.Parsec.Token
import Text.Parsec.Language

type TestResult = (DII_Packet, Maybe RVFI_Packet, Maybe RVFI_Packet)

data Report = ReportAssert Bool String

instance Show Report where
  show (ReportAssert b s) = (if b then "Passed" else "Failed") ++ " assert: " ++ s

type ShrinkStrategy = Test TestResult -> [Test TestResult]

data MetaInfo = MetaShrinkStrategy ShrinkStrategy
              | MetaNoShrink
              | MetaShrinkScope
              | MetaAssertLastVal Integer
              | MetaAssertCompound (Test TestResult -> (Bool, String))
              | MetaReport Report

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

shrinkScope :: Template -> Template
shrinkScope = TemplateMeta MetaShrinkScope

shrinkStrategy :: Template -> ShrinkStrategy -> Template
shrinkStrategy x f = TemplateMeta (MetaShrinkStrategy f) x

-- * Test datatype definition core
--------------------------------------------------------------------------------

data Test t = TestEmpty
            | TestSingle t
            | TestSequence (Test t) (Test t)
            | TestMeta MetaInfo (Test t)

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
  traverse f (TestSequence x y) = liftA2 TestSequence (traverse f x) (traverse f y)
  traverse f (TestMeta m x) = TestMeta m <$> traverse f x

genTest :: Template -> Gen (Test Instruction)
genTest TemplateEmpty = return TestEmpty
genTest (TemplateSingle x) = return $ TestSingle x
genTest (TemplateSequence x y) = TestSequence <$> genTest x <*> genTest y
genTest (TemplateMeta m x) = TestMeta m <$> genTest x
genTest (TemplateRandom g) = genTest =<< g

-- * Test shrinking
--------------------------------------------------------------------------------

instance Arbitrary (Test TestResult) where
  arbitrary = return TestEmpty
  shrink TestEmpty = []
  shrink (TestSingle x) = []
  shrink (TestSequence x y) = let xs = shrink x
                                  ys = shrink y
                              in    [TestSequence x' y  | x' <- xs]
                                 ++ [TestSequence x  y' | y' <- ys]
  shrink (TestMeta MetaNoShrink _) = []
  shrink (TestMeta m@(MetaShrinkStrategy f) x) = TestMeta m <$> (f x ++ shrink x)
  shrink (TestMeta m x) = TestMeta m <$> shrink x

data ShrinkMethods = MkShrinkMethods { methodSingle :: TestResult -> [Test TestResult]
                                     , methodSequence :: Test TestResult -> ShrinkStrategy
                                     , methodShrinkScope :: ShrinkStrategy }
defaultShrinkMethods :: ShrinkMethods
defaultShrinkMethods = MkShrinkMethods { methodSingle = const []
                                       , methodSequence = const $ const []
                                       , methodShrinkScope = const [] }

recurseShrink :: ShrinkMethods -> ShrinkStrategy
recurseShrink                     _ TestEmpty = []
recurseShrink   MkShrinkMethods{..} (TestSingle x) = methodSingle x
recurseShrink s@MkShrinkMethods{..} (TestSequence x y) = let xs = recurseShrink s x
                                                             ys = recurseShrink s y
                                                         in methodSequence x y
                                                            ++ [TestSequence x' y  | x' <- xs]
                                                            ++ [TestSequence x  y' | y' <- ys]
recurseShrink                     _ (TestMeta MetaNoShrink _) = []
recurseShrink s@MkShrinkMethods{..} (TestMeta MetaShrinkScope x) = methodShrinkScope x ++ (TestMeta MetaShrinkScope <$> recurseShrink s x)
recurseShrink s@MkShrinkMethods{..} (TestMeta m x) = TestMeta m <$> recurseShrink s x

-- * Test API
--------------------------------------------------------------------------------

singleShrink :: (TestResult -> [Test TestResult]) -> ShrinkStrategy
singleShrink f = recurseShrink defaultShrinkMethods { methodSingle = f }

sequenceShrink :: (Test TestResult -> ShrinkStrategy) -> ShrinkStrategy
sequenceShrink g = recurseShrink defaultShrinkMethods { methodSequence = g }

defaultShrink :: ShrinkStrategy
defaultShrink = recurseShrink defaultShrinkMethods { methodSingle = const [TestEmpty]
                                                   , methodShrinkScope = const [TestEmpty] }

mapWithAssertLastVal :: ([Integer] -> a -> b) -> Test a -> Test b
mapWithAssertLastVal f x = snd $ go [] f x
  where go  _ _ TestEmpty = (False, TestEmpty)
        go vs f (TestSingle x) = (True, TestSingle (f vs x))
        go vs f (TestSequence x y) = let (by, y') = go vs f y
                                         vs' = if by then [] else vs
                                         (bx, x') = go vs' f x
                                     in (by || bx, TestSequence x' y')
        go vs f (TestMeta m@(MetaAssertLastVal v) x) = let (b, x') = go (v:vs) f x in (b, TestMeta m x')
        go vs f (TestMeta m x) = let (b, x') = go vs f x in (b, TestMeta m x')

runAsserts :: Test TestResult -> Test TestResult
runAsserts TestEmpty = TestEmpty
runAsserts (TestSingle x) = TestSingle x
runAsserts (TestSequence x y) = TestSequence (runAsserts x) (runAsserts y)
-- TODO Fold in mapWithAssertLastVal here somehow?
runAsserts (TestMeta (MetaAssertCompound f) x) = TestMeta m' x'
  where m' = MetaReport $ uncurry ReportAssert (f x)
        x' = runAsserts x
runAsserts (TestMeta m x) = TestMeta m (runAsserts x)

gatherReports :: Test t -> [(Report, Test t)]
gatherReports (TestSequence x y) = gatherReports x ++ gatherReports y
gatherReports (TestMeta (MetaReport r) x) = (r, x) : gatherReports x
gatherReports (TestMeta _ x) = gatherReports x
gatherReports _ = []

singleTest :: t -> Test t
singleTest = TestSingle

seqSingleTest :: [t] -> Test t
seqSingleTest = mconcat . map TestSingle

noShrinkTest :: Test t -> Test t
noShrinkTest = TestMeta MetaNoShrink

shrinkTestStrategy :: Test TestResult -> ShrinkStrategy -> Test TestResult
shrinkTestStrategy x f = TestMeta (MetaShrinkStrategy f) x

-- * IO of tests
--------------------------------------------------------------------------------

shrinkScopeTok = "SHRINK_SCOPE"
noShrinkTok = "NO_SHRINK"
assertLastValTok = "ASSERT_LAST_VAL"
startTok = "START_"
endTok = "END_"
versionTok = "QCVENGINE_TEST_V2.0"
magicTok = "#>"

instance Show t => Show (Test t) where
  show x = printf "%s%s%s" magicTok versionTok (go x)
    where go TestEmpty = ""
          go (TestSingle x) = printf "\n%s" (show x)
          go (TestSequence x y) = printf "%s%s" (go x) (go y)
          go (TestMeta MetaShrinkScope x) = printf "\n%s%s%s%s\n%s%s%s" magicTok startTok shrinkScopeTok
                                                                        (go x)
                                                                        magicTok endTok shrinkScopeTok
          go (TestMeta MetaNoShrink x) = printf "\n%s%s%s%s\n%s%s%s" magicTok startTok noShrinkTok
                                                                     (go x)
                                                                     magicTok endTok noShrinkTok
          go (TestMeta (MetaShrinkStrategy _) x) = go x -- Cannot serialise function
          go (TestMeta (MetaAssertLastVal v) x) = printf "\n%s%s%s%s\n%s%s%s rd_wdata == 0x%x \"\"" magicTok startTok assertLastValTok
                                                                                                    (go x)
                                                                                                    magicTok endTok assertLastValTok v
          go (TestMeta (MetaAssertCompound _) x) = go x -- Cannot serialise function
          go (TestMeta (MetaReport r) x) = printf "\n# REPORT     '%s' {%s\n# } END REPORT '%s'" (show r) (go x) (show r)

type Parser = Parsec String ()

-- Parse a legacy Test
ltp = makeTokenParser $ emptyDef { commentLine   = "#"
                                 , reservedNames = [ ".shrink", ".noshrink"
                                                   , ".4byte", ".2byte"
                                                   , ".assert"] }
legacyParseTest :: Parser (Test Instruction)
legacyParseTest = do
  whiteSpace ltp
  test <- mconcat <$> many legacyParseTestStrand
  eof
  return test
legacyParseTestStrand :: Parser (Test Instruction)
legacyParseTestStrand = do
  mshrink <- optionMaybe $     (reserved ltp ".noshrink" >> return False)
                           <|> (reserved ltp   ".shrink" >> return  True)
  insts <- mconcat <$> many1 legacyParseInst
  return $ case mshrink of Just False -> TestMeta MetaNoShrink insts
                           _ -> insts
legacyParseInst :: Parser (Test Instruction)
legacyParseInst = do
  reserved ltp ".4byte" <|> reserved ltp ".2byte"
  bits <- natural ltp
  let inst = TestSingle $ MkInstruction bits
  option inst (try $ legacyParseSingleAssert inst)
legacyParseSingleAssert :: Test Instruction -> Parser (Test Instruction)
legacyParseSingleAssert x = do
  reserved ltp ".assert"
  reserved ltp "rd_wdata"
  symbol ltp "=="
  val <- natural ltp
  str <- stringLiteral ltp
  return $ TestMeta (MetaAssertLastVal val) x

-- Parse a Test
tp = makeTokenParser $ emptyDef { reservedNames = [ ".4byte", ".2byte"
                                                  , "#", versionTok ]
                                                  ++ [magicTok ++ pfx ++ sfx | pfx <- [startTok, endTok], sfx <- [shrinkScopeTok, noShrinkTok, assertLastValTok]] }

parseTest :: Parser (Test Instruction)
parseTest = do
  parseComments
  reserved tp versionTok
  parseComments
  parseTestBody

parseTestBody :: Parser (Test Instruction)
parseTestBody = option TestEmpty $ do
  first <- parseInst <|> parseShrinkScope <|> parseNoShrink <|> parseAssert
  rest <- parseTestBody
  return $ first <> rest

parseInst :: Parser (Test Instruction)
parseInst = do
  reserved tp ".4byte" <|> reserved tp ".2byte"
  bits <- natural tp
  parseComments
  return $ TestSingle $ MkInstruction bits

parseAssert :: Parser (Test Instruction)
parseAssert = do
  reserved tp $ magicTok ++ startTok ++ assertLastValTok
  parseComments
  inner <- parseTestBody
  reserved tp $ magicTok ++ endTok ++ assertLastValTok
  string "rd_wdata"
  symbol tp "=="
  val <- natural tp
  stringLiteral tp
  parseComments
  return $ TestMeta (MetaAssertLastVal val) inner

parseScope :: String -> Parser (Test Instruction)
parseScope tok = do
  reserved tp $ magicTok ++ startTok ++ tok
  parseComments
  inner <- parseTestBody
  reserved tp $ magicTok ++ endTok ++ tok
  parseComments
  return inner

parseShrinkScope :: Parser (Test Instruction)
parseShrinkScope = do
  inner <- parseScope shrinkScopeTok
  return $ TestMeta MetaShrinkScope inner

parseNoShrink :: Parser (Test Instruction)
parseNoShrink = do
  inner <- parseScope noShrinkTok
  return $ TestMeta MetaNoShrink inner

parseComments :: Parser ()
parseComments = do many (reserved tp "#" >> manyTill anyToken newline)
                   return ()

instance Read (Test Instruction) where
  readsPrec _ str = case parse (partial (try parseTest <|> legacyParseTest)) "Read" str of
                      Left  e -> error $ show e ++ ", in:\n" ++ str ++ "\n"
                      Right x -> [x]
    where partial p = (,) <$> p <*> getInput

