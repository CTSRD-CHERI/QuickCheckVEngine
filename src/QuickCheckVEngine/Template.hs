{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

--
-- SPDX-License-Identifier: BSD-2-Clause
--
-- Copyright (c) 2019 Peter Rugg
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
    Module      : QuickCheckVEngine.Template
    Description : Helpers to define RISC-V tests

    The 'QuickCheckVEngine.Template' module provides helpers to express RISC-V
    tests
-}

module QuickCheckVEngine.Template (
  Template(..)
, instSeq
, instDist
, uniformTemplate
, replicateTemplate
, repeatTemplate
, repeatTemplateTillEnd
, TestCase(..) -- TODO no longer export internals
, TestStrand(..) -- TODO no longer export internals
, toTestCase
, fromTestCase
, testCaseInstCount
, genTemplate
, genTemplateSized
, genTemplateUnsized
, (<>)
) where

import Test.QuickCheck
import QuickCheckVEngine.RVFI_DII.RVFI
import Data.List
import Data.Maybe
import Data.Semigroup -- Should no longer be required with modern ghc
import RISCV
import Text.Printf
import Text.Parsec hiding (Empty)
import Text.Parsec.Token
import Text.Parsec.Language

-- | 'Template' type to describe sequences of instructions (represented as
--   'Integer's) to be used as tests
data Template = Empty
              | Single Integer
              | Distribution [(Int, Template)]
              | Sequence [Template]
              | Random (Gen Template)
              | Assert (Template, [RVFI_Packet] -> Bool, String)
              | NoShrink Template
instance Show Template where
  show Empty = "Empty"
  show (Single x) = "Single (" ++ show x ++ ")"
  show (Distribution x) = "Distribution " ++ show x
  show (Sequence x) = "Sequence " ++ show x
  show (Random x) = "Random (?)"
  show (Assert (x,y,z)) = "Assert (" ++ show x ++ ") " ++ show z
  show (NoShrink x) = "NoShrink (" ++ show x ++ ")"
instance Semigroup Template where
  x <> y = Sequence [x, y]
instance Monoid Template where
  mempty = Empty

-- | Turn a list of 'Integer' instructions into a 'Sequence [Template]'
instSeq :: [Integer] -> Template
instSeq insts = Sequence (map Single insts)

-- | Turn a list of '(Int, Integer)' weigthed instructions into a
--   'Distribution [(Int, Template)]'
instDist :: [(Int, Integer)] -> Template
instDist winsts = Distribution $ map (\(w, x) -> (w, Single x)) winsts

-- | Turn a list of possibilities into a uniform distribution
class UniformTemplate t where
  uniformTemplate :: [t] -> Template
instance UniformTemplate Template where
  uniformTemplate options = Distribution $ map (\x -> (1, x)) options
instance UniformTemplate Integer where
  uniformTemplate options = Distribution $ map (\x -> (1, Single x)) options

-- | Replicate a 'Template' a given number of times
replicateTemplate :: Int -> Template -> Template
replicateTemplate n template = Sequence $ replicate n template

-- | Repeat a 'Template' an infinite number of times
repeatTemplate :: Template -> Template
repeatTemplate template = Sequence $ repeat template

-- | Note that this requires the argument to always return a list of length 1
repeatTemplateTillEnd :: Template -> Template
repeatTemplateTillEnd template = Random $ do
  size <- getSize
  return $ replicateTemplate size template

-- | 'TestCase' type for generated 'Template'
newtype TestCase = TC ([TestStrand], [RVFI_Packet] -> [String])
instance Show TestCase where
  show (TC (tss, _)) = intercalate "\n" (map show tss)
instance Read TestCase where
  readsPrec _ str = case parse (partial parseTestCase) "Read" str of
                      Left  e -> error $ show e ++ ", in:\n" ++ str ++ "\n"
                      Right x -> [x]
instance Semigroup TestCase where
  TC (xs, xAssert) <> TC (ys, yAssert) = TC $ (xs ++ ys, concatHelper (flatten xs) (flatten ys) (const []) yAssert)
    where concatHelper []     ys xAcc yAcc = \zs -> xAssert (reverse $ xAcc zs) ++ yAcc zs
          concatHelper (x:xs) ys xAcc yAcc = concatHelper xs ys (\zs -> (head zs) : (xAcc . tail) zs) (yAcc . tail)
instance Monoid TestCase where
  mempty = TC ([], const [])

flatten [] = []
flatten ((TS b s):ss) = (map (\x -> (b, x)) s) ++ (flatten ss)

prepend_all x xs = map ((:) x) xs

substitute [] _ _ = []
substitute ((b,x):xs) new old = (if b && m_rs2 == Just old then [(b,reencode new (fromMaybe 0 m_rs1) (fromMaybe 0 m_rd)):xs] else [])
                             ++ (if b && m_rs1 == Just old then [(b,reencode (fromMaybe 0 m_rs2) new (fromMaybe 0 m_rd)):xs] else [])
                             ++ (if (m_rd == Just old) || (m_rd == Just new) then [] else prepend_all (b,x) (substitute xs new old))
                                where (_, m_rs2, m_rs1, m_rd, reencode) = extract_regs x

shrink_bypass :: [(Bool, Integer)] -> [[(Bool, Integer)]]
shrink_bypass [] = []
shrink_bypass ((b,x):xs) = prepend_all (b,x) ((if is_bypass && not (m_rd == m_rs1) then substitute xs (fromMaybe 0 m_rs1) (fromMaybe 0 m_rd) else []) ++ (shrink_bypass xs))
                           where (is_bypass, _, m_rs1, m_rd, _) = extract_regs x

coalesce [] = []
coalesce [s] = [s]
coalesce ((TS b1 x1):(TS b2 x2):ss) = if b1==b2 then coalesce ((TS b1 (x1++x2)):ss) else (TS b1 x1):(coalesce ((TS b2 x2):ss))

instance Arbitrary TestCase where
  arbitrary = (\c -> TC (c, const [])) <$> arbitrary
  shrink (TC (ss,_)) = map ((\c -> TC (c, const [])) . coalesce) $
                           [    filter tsNotNull ys
                             ++ if tsNotNull z' then [z'] else []
                             ++ filter tsNotNull zs
                           | (ys, z:zs) <- splits ss
                           , z' <- shrink z ]
                           ++ map (map (\(b,x) -> TS b [x])) (shrink_bypass (flatten ss))
    where splits [] = []
          splits (x:xs) = ([], x:xs) : [(x:ys, zs) | (ys, zs) <- splits xs]
          tsNotNull ts = (not . null) $ testStrandInsts ts
-- | 'TestStrand' type representing a shrinkable part of a 'TestCase'
data TestStrand = TS { testStrandShrink :: Bool
                     , testStrandInsts  :: [Integer] }
instance Show TestStrand where
  show (TS { testStrandShrink = shrink
           , testStrandInsts  = insts }) = showShrink ++ "\n" ++ showInsts
    where showShrink = if shrink then ".shrink" else ".noshrink"
          showInsts  = intercalate "\n" (map showInst insts)
          showInst inst = printf ".4byte 0x%08x # %s" inst (pretty inst)
instance Arbitrary TestStrand where
  arbitrary = do TC (strands, _) <- genTemplate Empty
                 return $ head strands
  shrink (TS False x) = []
  shrink (TS True  x) = map (TS True) (shrinkList rv_shrink x)

-- | Create a simple 'TestCase' ...
class ToTestCase x where
  toTestCase :: x -> TestCase
-- | ... from a list of 'TestStrand's
instance ToTestCase [TestStrand] where
  toTestCase ss = TC (ss, const [])
-- | ... from a list of instructions represented as a list of 'Integer'
instance ToTestCase [Integer] where
  toTestCase insts = TC ([TS True insts], const [])

-- Parse a TestCase
tp = makeTokenParser $ emptyDef { commentLine   = "#"
                                , reservedNames = [ ".shrink", ".noshrink"
                                                  , ".4byte", ".2byte"
                                                  , ".assert"] }
partial p = (,) <$> p <*> getInput
parseTestCase = do
  whiteSpace tp
  tss <- optionMaybe $ many1 parseTestStrand
  eof
  return $ foldl (<>) mempty (fromMaybe [mempty] tss)
parseTestStrand = do
  mshrink <- optionMaybe $     (reserved tp ".noshrink" >> return False)
                           <|> (reserved tp   ".shrink" >> return  True)
  insts   <- many1 parseInst
  return $ TC ([TS (fromMaybe True mshrink) (map fst insts)],
               \reports -> concat $ zipWith (\assert report -> assert report) (map snd insts) reports)
parseInst = do
  (reserved tp ".4byte" <|> reserved tp ".2byte")
  bits <- natural tp
  asserts <- optionMaybe $ many1 parseAssert
  return (bits, \report -> case asserts of Nothing -> []
                                           Just as -> concat $ map ($ report) as)
parseAssert = do
  reserved tp ".assert"
  field <- identifier tp
  symbol tp "=="
  val <- natural tp
  str <- stringLiteral tp
  return $ \report -> case rvfiGetFromString field of Nothing -> ["Invalid assert (" ++ str ++ "): field " ++ field ++ " not recognised"]
                                                      Just getter -> if not (getter report == val) then [str] else []

-- | Create a list of instructions represented as a list of 'Integer' from the
--   given 'TestCase'
fromTestCase :: TestCase -> ([Integer], [RVFI_Packet] -> [String])
fromTestCase (TC (ss, a)) = (concatMap testStrandInsts ss, a)

-- | Count the number of instructions in a 'TestCase'
testCaseInstCount :: TestCase -> Int
testCaseInstCount = length . fromTestCase

-- | Turn a 'Template' into a single QuickCheck 'Gen [Integer]' generator
--   of list of instructions
genTemplate :: Template -> Gen TestCase
genTemplate template = getSize >>= genTemplateSized template

-- | Same as 'genTemplate' but specify the desired size
genTemplateSized :: Template -> Int -> Gen TestCase
genTemplateSized template size = do
  TC (xs, a) <- genHelper template
  let (_, mbss) = mapAccumL (\acc (TS shrink insts) ->
                     let remaining = max 0 (size - acc)
                         nbInsts = length insts
                         newAcc = acc + nbInsts
                     in (newAcc, if (remaining == 0)
                                   then Nothing
                                   else Just $
                                     if (nbInsts <= remaining)
                                       then TS shrink insts
                                       else TS shrink (take remaining insts))
                  ) 0 xs
  return $ TC (catMaybes mbss, a) -- XXX Unclear what should happen to asserts
                                  -- where corresponding instructions were
                                  -- truncated

-- | Turn a 'Template' into a single QuickCheck 'Gen [Integer]' generator
--   of list of instructions, in an explicitly unsized manner
genTemplateUnsized :: Template -> Gen TestCase
genTemplateUnsized = genHelper

-- | Inner helper to implement the 'genTemplate' functions
genHelper :: Template -> Gen TestCase
genHelper Empty = return mempty
genHelper (Single x) = return $ TC ([TS True [x]], const [])
genHelper (Distribution xs) = do let xs' = map (\(a, b) -> (a, return b)) xs
                                 frequency xs' >>= genHelper
genHelper (Sequence []) = return mempty
genHelper (Sequence (x:xs)) = do
  TC (start, aS) <- genHelper x
  TC (end,   aE) <- genHelper $ Sequence xs
  let TC (_, aO) = TC (start, aS) <> TC (end, aE)
  case (start, end) of
    ([], []) -> return mempty
    ([], _)  -> return $ TC (end, aO)
    (_, [])  -> return $ TC (start, aO)
    (_, _)   -> do
      let (TS shrink0 insts0) = last start
      let (TS shrink1 insts1) = head end
      if shrink0 == shrink1
        then return $ TC (  init start
                         ++ [TS shrink0 (insts0 ++ insts1)]
                         ++ tail end, aO)
        else return $ TC (start ++ end, aO)
genHelper (Random x) = x >>= genHelper
genHelper (Assert (x,y,z)) = do
  TC (a, b) <- genHelper x
  return $ TC (a, \c -> (if not(y c) then [z] else []) ++ b c)
genHelper (NoShrink x) = do
  TC (a, b) <- genHelper x
  return $ TC ([TS False (fst $ fromTestCase $ TC (a, b))], b)
