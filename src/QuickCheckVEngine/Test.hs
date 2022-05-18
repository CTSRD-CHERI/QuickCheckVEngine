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
    Module      : QuickCheckVEngine.Test
    Description : Helpers to work with RISC-V tests
-}

module QuickCheckVEngine.Test (
-- * Types
  Test
, TestResult
, ShrinkStrategy
, Report(..)
-- * Test API
, module Data.Semigroup -- XXX Remove once everyone has a newer compiler
, single
, singleSeq
, noShrink
, shrinkStrategy
-- ** Test tree transformations
, removeEmpties
, addShrinkScopes
, balance
-- ** Shrink strategy constructors
, defaultShrink
, singleShrink
, sequenceShrink
-- ** Test tree processing
, mapWithAssertLastVal
, runAssertCompounds
-- ** Tree summarisation
, gatherReports
, filterTest
) where

import Test.QuickCheck
import Data.Semigroup (Semigroup(..))
import RISCV (Instruction(..))
import Text.Printf
import Text.Parsec hiding (parseTest)
import Text.Parsec.Token
import Text.Parsec.Language
import Control.Monad (void)
import QuickCheckVEngine.TestTypes

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
  shrink (TestMeta (MetaNoShrink, _) _) = []
  shrink (TestMeta m@(MetaShrinkStrategy f, _) x) =
    TestMeta m <$> (f x ++ shrink x)
  shrink (TestMeta m x) = TestMeta m <$> shrink x

data ShrinkMethods =
  MkShrinkMethods { methodSingle :: TestResult -> [Test TestResult]
                  , methodSequence :: Test TestResult -> ShrinkStrategy
                  , methodShrinkScope :: ShrinkStrategy }
defaultShrinkMethods :: ShrinkMethods
defaultShrinkMethods = MkShrinkMethods { methodSingle = const []
                                       , methodSequence = const $ const []
                                       , methodShrinkScope = const [] }

recurseShrink :: ShrinkMethods -> ShrinkStrategy
recurseShrink                     _ TestEmpty = []
recurseShrink   MkShrinkMethods{..} (TestSingle x) = methodSingle x
recurseShrink s@MkShrinkMethods{..} (TestSequence x y) =
  let xs = recurseShrink s x
      ys = recurseShrink s y
  in methodSequence x y
     ++ inter [TestSequence x' y  | x' <- xs]
              [TestSequence x  y' | y' <- ys]
  where inter (x:xs) (y:ys) = x:y:(inter xs ys)
        inter []     ys     = ys
        inter xs     []     = xs
recurseShrink                     _ (TestMeta (MetaNoShrink, _) _) = []
recurseShrink s@MkShrinkMethods{..} (TestMeta m@(MetaShrinkScope, _) x) =
  methodShrinkScope x ++ (TestMeta m <$> recurseShrink s x)
recurseShrink s@MkShrinkMethods{..} (TestMeta m x) =
  TestMeta m <$> recurseShrink s x

-- * Test API
--------------------------------------------------------------------------------

single :: t -> Test t
single = TestSingle

singleSeq :: [t] -> Test t
singleSeq = mconcat . map TestSingle

noShrink :: Test t -> Test t
noShrink = TestMeta (MetaNoShrink, True)

shrinkStrategy :: Test TestResult -> ShrinkStrategy -> Test TestResult
shrinkStrategy x f = TestMeta (MetaShrinkStrategy f, False) x

removeEmpties :: Test t -> Test t
removeEmpties = snd . go
  where go TestEmpty = (False, TestEmpty)
        go x@(TestSingle _) = (True, x)
        go (TestSequence x y) = let (bx, x') = go x
                                    (by, y') = go y
                                in case (bx, by) of
                                  (True, True) -> (True, TestSequence x' y')
                                  (True, False) -> (True, x')
                                  (False, True) -> (True, y')
                                  (False, False) -> (False, TestEmpty)
        go (TestMeta m x) = let (b, x') = go x in (b, TestMeta m x')

addShrinkScopes :: Test t -> Test t
addShrinkScopes = snd . go
  where go TestEmpty = (False, TestEmpty)
        go x@(TestSingle _) = (False, x)
        go (TestSequence x y) =
          let (bx, x') = go x
              (by, y') = go y
              newT = TestSequence x' y'
              b = bx || by
          in (b, (if b then id else TestMeta (MetaShrinkScope, False)) newT)
        go x@(TestMeta (MetaNoShrink, _) _) = (True, x)
        go (TestMeta m@(MetaShrinkScope, _) x) =
          let (_, x') = go x in (False, TestMeta m x')
        go (TestMeta m x) = let (b, x') = go x in (b, TestMeta m x')

flattenTopLevel :: Test t -> [Test t]
flattenTopLevel TestEmpty = [TestEmpty]
flattenTopLevel x@(TestSingle _) = [x]
flattenTopLevel (TestSequence x y) = flattenTopLevel x ++ flattenTopLevel y
flattenTopLevel x@(TestMeta _ _) = [x]

bisect :: [Test t] -> Test t
bisect [] = TestEmpty
bisect [x] = x
bisect xs = let newLen = length xs `div` 2
            in TestSequence (bisect $ take newLen xs) (bisect $ drop newLen xs)

balance :: Test t -> Test t
balance TestEmpty = TestEmpty
balance x@(TestSingle _) = x
balance s@(TestSequence _ _) = bisect (balance <$> flattenTopLevel s)
balance (TestMeta m x) = TestMeta m (balance x)

defaultShrink :: ShrinkStrategy
defaultShrink =
  recurseShrink defaultShrinkMethods { methodSingle = const [TestEmpty]
                                     , methodShrinkScope = const [TestEmpty] }

singleShrink :: (TestResult -> [Test TestResult]) -> ShrinkStrategy
singleShrink f = recurseShrink defaultShrinkMethods { methodSingle = f }

sequenceShrink :: (Test TestResult -> ShrinkStrategy) -> ShrinkStrategy
sequenceShrink g = recurseShrink defaultShrinkMethods { methodSequence = g }

mapWithAssertLastVal :: ([Integer] -> a -> b) -> Test a -> Test b
mapWithAssertLastVal f x = snd $ go [] f x
  where go  _ _ TestEmpty = (False, TestEmpty)
        go vs f (TestSingle x) = (True, TestSingle (f vs x))
        go vs f (TestSequence x y) = let (by, y') = go vs f y
                                         vs' = if by then [] else vs
                                         (bx, x') = go vs' f x
                                     in (by || bx, TestSequence x' y')
        go vs f (TestMeta m@(MetaAssertLastVal v, _) x) =
          let (b, x') = go (v:vs) f x in (b, TestMeta m x')
        go vs f (TestMeta m x) = let (b, x') = go vs f x in (b, TestMeta m x')

runAssertCompounds :: Test TestResult -> Test TestResult
runAssertCompounds TestEmpty = TestEmpty
runAssertCompounds (TestSingle x) = TestSingle x
runAssertCompounds (TestSequence x y) =
  TestSequence (runAssertCompounds x) (runAssertCompounds y)
runAssertCompounds (TestMeta (MetaAssertCompound f, _) x) = TestMeta m' x'
  where m' = (MetaReport $ uncurry ReportAssert (f x), True)
        x' = runAssertCompounds x
runAssertCompounds (TestMeta m x) = TestMeta m (runAssertCompounds x)

gatherReports :: Test t -> [(Report, Test t)]
gatherReports (TestSequence x y) = gatherReports x ++ gatherReports y
gatherReports (TestMeta (MetaReport r, _) x) = (r, x) : gatherReports x
gatherReports (TestMeta _ x) = gatherReports x
gatherReports _ = []

filterTest :: (t -> Bool) -> Test t -> Test t
filterTest _ TestEmpty = TestEmpty
filterTest p t@(TestSingle x) = if p x then t else TestEmpty
filterTest p (TestSequence x y) = TestSequence (filterTest p x) (filterTest p y)
filterTest p (TestMeta m x) = TestMeta m (filterTest p x)

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
          go (TestMeta (_, False) x) = go x
          go (TestMeta (MetaShrinkScope, True) x) =
            printf "\n%s%s%s%s\n%s%s%s" magicTok startTok shrinkScopeTok
                                        (go x)
                                        magicTok endTok shrinkScopeTok
          go (TestMeta (MetaNoShrink, True) x) =
            printf "\n%s%s%s%s\n%s%s%s" magicTok startTok noShrinkTok
                                        (go x)
                                        magicTok endTok noShrinkTok
          go (TestMeta (MetaShrinkStrategy _, True) x) =
            error "Cannot serialise shrink strategy"
          go (TestMeta (MetaAssertLastVal v, True) x) =
            printf "\n%s%s%s%s\n%s%s%s rd_wdata == 0x%x \"\""
                   magicTok startTok assertLastValTok
                   (go x)
                   magicTok endTok assertLastValTok v
          go (TestMeta (MetaAssertCompound _, True) x) =
            error "Cannot serialise compound assertion"
          go (TestMeta (MetaReport r, True) x) =
            printf "\n# REPORT     '%s' {%s\n# } END REPORT '%s'"
                   (show r) (go x) (show r)

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
  return $ case mshrink of Just False -> TestMeta (MetaNoShrink, True) insts
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
  return $ TestMeta (MetaAssertLastVal val, True) x

-- Parse a Test
tp = makeTokenParser $
  emptyDef { reservedNames =
               [ ".4byte", ".2byte", magicTok ++ versionTok ] ++
               [ magicTok ++ pfx ++ sfx 
                 | pfx <- [startTok, endTok]
                 , sfx <- [shrinkScopeTok, noShrinkTok, assertLastValTok] ]
           , identStart = oneOf "#."
           , identLetter = alphaNum <|> char '>' <|> char '_' <|> char '.' }

parseTest :: Parser (Test Instruction)
parseTest = do
  whiteSpace tp
  parseComments
  reserved tp $ magicTok ++ versionTok
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
  symbol tp "rd_wdata"
  symbol tp "=="
  val <- natural tp
  stringLiteral tp
  parseComments
  return $ TestMeta (MetaAssertLastVal val, True) inner

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
  return $ TestMeta (MetaShrinkScope, True) inner

parseNoShrink :: Parser (Test Instruction)
parseNoShrink = do
  inner <- parseScope noShrinkTok
  return $ TestMeta (MetaNoShrink, True) inner

parseComments :: Parser ()
parseComments = many p >> return ()
  where p = try $ do char '#'
                     notFollowedBy $ char '>'
                     manyTill anyChar (void newline <|> eof)

instance Read (Test Instruction) where
  readsPrec _ str =
    case parse (partial (try parseTest <|> legacyParseTest)) "Read" str of
      Left  e -> error $ show e ++ ", in:\n" ++ str ++ "\n"
      Right (x, s) -> [(removeEmpties x, s)]
    where partial p = (,) <$> p <*> getInput
