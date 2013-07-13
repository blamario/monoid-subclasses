{- 
    Copyright 2013 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

{-# LANGUAGE Rank2Types, ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Main where

import Prelude hiding (foldl, foldr, gcd, length, null, reverse, span, splitAt, takeWhile)

import Test.QuickCheck (Arbitrary, CoArbitrary, Property, Gen,
                        quickCheck, arbitrary, coarbitrary, property, label, forAll, variant, whenFail, (.&&.))
import Test.QuickCheck.Instances ()

import Data.Int (Int8, Int32)
import Data.Foldable (toList)
import Data.List (intersperse, unfoldr)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Either (lefts, rights)
import Data.Tuple (swap)
import Data.String (IsString, fromString)
import Data.Char (isLetter)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Text
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Vector (Vector, fromList)

import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8(ByteStringUTF8))

import Data.Monoid (Monoid, mempty, (<>), mconcat, All(All), Any(Any), Dual(Dual),
                    First(First), Last(Last), Sum(Sum), Product(Product))
import Data.Monoid.Null (MonoidNull, null)
import Data.Monoid.Factorial (FactorialMonoid, factors, splitPrimePrefix, splitPrimeSuffix, primePrefix, primeSuffix,
                              foldl, foldl', foldr, length, reverse, span, split, splitAt)
import Data.Monoid.Cancellative (CommutativeMonoid, ReductiveMonoid, LeftReductiveMonoid, RightReductiveMonoid,
                                 CancellativeMonoid, LeftCancellativeMonoid, RightCancellativeMonoid,
                                 GCDMonoid, LeftGCDMonoid, RightGCDMonoid,
                                 (</>), gcd,
                                 isPrefixOf, stripPrefix, commonPrefix, stripCommonPrefix,
                                 isSuffixOf, stripSuffix, commonSuffix, stripCommonSuffix)
import Data.Monoid.Textual (TextualMonoid)
import qualified Data.Monoid.Textual as Textual

data Test = CommutativeTest (forall a. (Arbitrary a, Show a, Eq a, CommutativeMonoid a) => a -> Property)
          | NullTest (forall a. (Arbitrary a, Show a, Eq a, MonoidNull a) => a -> Property)
          | FactorialTest (forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property)
          | TextualTest (forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property)
          | LeftReductiveTest (forall a. (Arbitrary a, Show a, Eq a, LeftReductiveMonoid a) => a -> Property)
          | RightReductiveTest (forall a. (Arbitrary a, Show a, Eq a, RightReductiveMonoid a) => a -> Property)
          | ReductiveTest (forall a. (Arbitrary a, Show a, Eq a, ReductiveMonoid a) => a -> Property)
          | LeftCancellativeTest (forall a. (Arbitrary a, Show a, Eq a, LeftCancellativeMonoid a) => a -> Property)
          | RightCancellativeTest (forall a. (Arbitrary a, Show a, Eq a, RightCancellativeMonoid a) => a -> Property)
          | CancellativeTest (forall a. (Arbitrary a, Show a, Eq a, CancellativeMonoid a) => a -> Property)
          | LeftGCDTest (forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a) => a -> Property)
          | RightGCDTest (forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a) => a -> Property)
          | GCDTest (forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a) => a -> Property)
          | CancellativeGCDTest (forall a. (Arbitrary a, Show a, Eq a, CancellativeMonoid a, GCDMonoid a) 
                                 => a -> Property)

main = mapM_ (quickCheck . uncurry checkInstances) tests

checkInstances :: String -> Test -> Property
checkInstances name (CommutativeTest checkType) = label name (checkType (mempty :: Sum Integer)
                                                              .&&. checkType (mempty :: Product Integer)
                                                              .&&. checkType (mempty :: Dual (Sum Integer))
                                                              .&&. checkType (mempty :: (Sum Integer, Sum Int))
                                                              .&&. checkType (mempty :: IntSet)
                                                              .&&. checkType (mempty :: Set Integer))
checkInstances name (NullTest checkType) = label name (checkType ()
                                                       .&&. checkType (mempty :: Ordering)
                                                       .&&. checkType (mempty :: All)
                                                       .&&. checkType (mempty :: Any)
                                                       .&&. checkType (mempty :: String)
                                                       .&&. checkType (mempty :: ByteString)
                                                       .&&. checkType (mempty :: Lazy.ByteString)
                                                       .&&. checkType (mempty :: Text)
                                                       .&&. checkType (mempty :: Lazy.Text)
                                                       .&&. checkType (mempty :: Dual String)
                                                       .&&. checkType (mempty :: Sum Float)
                                                       .&&. checkType (mempty :: Product Int)
                                                       .&&. checkType (mempty :: First Int)
                                                       .&&. checkType (mempty :: Last Int)
                                                       .&&. checkType (mempty :: Maybe String)
                                                       .&&. checkType (mempty :: (Text, String))
                                                       .&&. checkType (mempty :: IntMap Int)
                                                       .&&. checkType (mempty :: IntSet)
                                                       .&&. checkType (mempty :: Map String Int)
                                                       .&&. checkType (mempty :: Seq Int)
                                                       .&&. checkType (mempty :: Set String)
                                                       .&&. checkType (mempty :: Vector Int))
checkInstances name (FactorialTest checkType) = label name (checkType (mempty :: TestString)
                                                            .&&. checkType (mempty :: String)
                                                            .&&. checkType (mempty :: ByteString)
                                                            .&&. checkType (mempty :: Lazy.ByteString)
                                                            .&&. checkType (mempty :: ByteStringUTF8)
                                                            .&&. checkType (mempty :: Text)
                                                            .&&. checkType (mempty :: Lazy.Text)
                                                            .&&. checkType (mempty :: Dual String)
                                                            .&&. checkType (mempty :: Sum Int8)
                                                            .&&. checkType (mempty :: Product Int32)
                                                            .&&. checkType (mempty :: Maybe String)
                                                            .&&. checkType (mempty :: (Text, String))
                                                            .&&. checkType (mempty :: IntMap Int)
                                                            .&&. checkType (mempty :: IntSet)
                                                            .&&. checkType (mempty :: Map String Int)
                                                            .&&. checkType (mempty :: Seq Int)
                                                            .&&. checkType (mempty :: Set String)
                                                            .&&. checkType (mempty :: Vector Int))
checkInstances name (TextualTest checkType) = label name (checkType (mempty :: TestString)
                                                          .&&. checkType (mempty :: String)
                                                          .&&. checkType (mempty :: ByteStringUTF8)
                                                          .&&. checkType (mempty :: Text)
                                                          .&&. checkType (mempty :: Lazy.Text)
                                                          .&&. checkType (mempty :: Seq Char)
                                                          .&&. checkType (mempty :: Vector Char))
checkInstances name (LeftReductiveTest checkType) = label name (checkType (mempty :: String)
                                                                .&&. checkType (mempty :: ByteString)
                                                                .&&. checkType (mempty :: Lazy.ByteString)
                                                                .&&. checkType (mempty :: Text)
                                                                .&&. checkType (mempty :: Lazy.Text)
                                                                .&&. checkType (mempty :: Dual Text)
                                                                .&&. checkType (mempty :: Sum Integer)
                                                                .&&. checkType (mempty :: Product Integer)
                                                                .&&. checkType (mempty :: (Text, String))
                                                                .&&. checkType (mempty :: IntSet)
                                                                .&&. checkType (mempty :: Seq String)
                                                                .&&. checkType (mempty :: Set Integer)
                                                                .&&. checkType (mempty :: Vector Int))
checkInstances name (RightReductiveTest checkType) = label name (checkType (mempty :: ByteString)
                                                                 .&&. checkType (mempty :: Lazy.ByteString)
                                                                 .&&. checkType (mempty :: Text)
                                                                 .&&. checkType (mempty :: Lazy.Text)
                                                                 .&&. checkType (mempty :: Dual String)
                                                                 .&&. checkType (mempty :: Sum Integer)
                                                                 .&&. checkType (mempty :: Product Integer)
                                                                 .&&. checkType (mempty :: (Text, ByteString))
                                                                 .&&. checkType (mempty :: IntSet)
                                                                 .&&. checkType (mempty :: Seq Int)
                                                                 .&&. checkType (mempty :: Set String)
                                                                 .&&. checkType (mempty :: Vector Int))
checkInstances name (ReductiveTest checkType) = label name (checkType (mempty :: Sum Integer)
                                                            .&&. checkType (mempty :: Product Integer)
                                                            .&&. checkType (mempty :: Dual (Sum Integer))
                                                            .&&. checkType (mempty :: (Sum Integer, Sum Int))
                                                            .&&. checkType (mempty :: IntSet)
                                                            .&&. checkType (mempty :: Set Integer))
checkInstances name (LeftCancellativeTest checkType) = label name (checkType (mempty :: String)
                                                                   .&&. checkType (mempty :: ByteString)
                                                                   .&&. checkType (mempty :: Lazy.ByteString)
                                                                   .&&. checkType (mempty :: Text)
                                                                   .&&. checkType (mempty :: Lazy.Text)
                                                                   .&&. checkType (mempty :: Dual Text)
                                                                   .&&. checkType (mempty :: Sum Integer)
                                                                   .&&. checkType (mempty :: (Text, String))
                                                                   .&&. checkType (mempty :: Seq Int)
                                                                   .&&. checkType (mempty :: Vector Int))
checkInstances name (RightCancellativeTest checkType) = label name (checkType (mempty :: ByteString)
                                                                    .&&. checkType (mempty :: Lazy.ByteString)
                                                                    .&&. checkType (mempty :: Text)
                                                                    .&&. checkType (mempty :: Lazy.Text)
                                                                    .&&. checkType (mempty :: Dual String)
                                                                    .&&. checkType (mempty :: Sum Integer)
                                                                    .&&. checkType (mempty :: (Text, ByteString))
                                                                    .&&. checkType (mempty :: Seq Int)
                                                                    .&&. checkType (mempty :: Vector Int))
checkInstances name (CancellativeTest checkType) = label name (checkType (mempty :: Sum Integer)
                                                               .&&. checkType (mempty :: Dual (Sum Integer))
                                                               .&&. checkType (mempty :: (Sum Integer, Sum Int)))
checkInstances name (LeftGCDTest checkType) = label name (checkType (mempty :: String)
                                                          .&&. checkType (mempty :: ByteString)
                                                          .&&. checkType (mempty :: Lazy.ByteString)
                                                          .&&. checkType (mempty :: Text)
                                                          .&&. checkType (mempty :: Lazy.Text)
                                                          .&&. checkType (mempty :: Dual ByteString)
                                                          .&&. checkType (mempty :: Sum Integer)
                                                          .&&. checkType (mempty :: Product Integer)
                                                          .&&. checkType (mempty :: (Text, String))
                                                          .&&. checkType (mempty :: IntMap Int)
                                                          .&&. checkType (mempty :: IntSet)
                                                          .&&. checkType (mempty :: Map String Int)
                                                          .&&. checkType (mempty :: Seq Int)
                                                          .&&. checkType (mempty :: Set String)
                                                          .&&. checkType (mempty :: Vector Int))
checkInstances name (RightGCDTest checkType) = label name (checkType (mempty :: ByteString)
                                                           .&&. checkType (mempty :: Lazy.ByteString)
                                                           .&&. checkType (mempty :: Dual String)
                                                           .&&. checkType (mempty :: Sum Integer)
                                                           .&&. checkType (mempty :: Product Integer)
                                                           .&&. checkType (mempty :: (Seq Int, ByteString))
                                                           .&&. checkType (mempty :: IntSet)
                                                           .&&. checkType (mempty :: Seq Int)
                                                           .&&. checkType (mempty :: Set String)
                                                           .&&. checkType (mempty :: Vector Int))
checkInstances name (GCDTest checkType) = label name (checkType (mempty :: Sum Integer)
                                                      .&&. checkType (mempty :: Product Integer)
                                                      .&&. checkType (mempty :: Dual (Product Integer))
                                                      .&&. checkType (mempty :: (Sum Integer, Sum Int))
                                                      .&&. checkType (mempty :: IntSet)
                                                      .&&. checkType (mempty :: Set String))
checkInstances name (CancellativeGCDTest checkType) = label name (checkType (mempty :: Sum Integer)
                                                                  .&&. checkType (mempty :: Dual (Sum Integer))
                                                                  .&&. checkType (mempty :: (Sum Integer, Sum Int)))

tests :: [(String, Test)]
tests = [("CommutativeMonoid", CommutativeTest checkCommutative),
         ("MonoidNull", NullTest checkNull),
         ("mconcat . factors == id", FactorialTest checkConcatFactors),
         ("all factors . factors", FactorialTest checkFactorsOfFactors),
         ("splitPrimePrefix", FactorialTest checkSplitPrimePrefix),
         ("splitPrimeSuffix", FactorialTest checkSplitPrimeSuffix),
         ("primePrefix", FactorialTest checkPrimePrefix),
         ("primeSuffix", FactorialTest checkPrimeSuffix),
         ("foldl", FactorialTest checkLeftFold),
         ("foldl'", FactorialTest checkLeftFold'),
         ("foldr", FactorialTest checkRightFold),
         ("length", FactorialTest checkLength),
         ("span", FactorialTest checkSpan),
         ("split", FactorialTest checkSplit),
         ("splitAt", FactorialTest checkSplitAt),
         ("reverse", FactorialTest checkReverse),
         ("fromText", TextualTest checkFromText),
         ("singleton", TextualTest checkSingleton),
         ("Textual.splitCharacterPrefix", TextualTest checkSplitCharacterPrefix),
         ("Textual.characterPrefix", TextualTest checkCharacterPrefix),
         ("Textual factors", TextualTest checkTextualFactors),
         ("Textual.unfoldr", TextualTest checkUnfoldrToFactors),
         ("factors . fromString", TextualTest checkFactorsFromString),
         ("Textual.map", TextualTest checkTextualMap),
         ("Textual.concatMap", TextualTest checkConcatMap),
         ("Textual.any", TextualTest checkAny),
         ("Textual.all", TextualTest checkAll),
         ("Textual.foldl", TextualTest checkTextualFoldl),
         ("Textual.foldr", TextualTest checkTextualFoldr),
         ("Textual.foldl'", TextualTest checkTextualFoldl'),
         ("Textual.scanl", TextualTest checkTextualScanl),
         ("Textual.scanr", TextualTest checkTextualScanr),
         ("Textual.scanl1", TextualTest checkTextualScanl1),
         ("Textual.scanr1", TextualTest checkTextualScanr1),
         ("Textual.mapAccumL", TextualTest checkTextualMapAccumL),
         ("Textual.mapAccumR", TextualTest checkTextualMapAccumR),
         ("Textual.mapAccumR", TextualTest checkTextualMapAccumR),
         ("Textual.takeWhile", TextualTest checkTextualTakeWhile),
         ("Textual.dropWhile", TextualTest checkTextualDropWhile),
         ("Textual.span", TextualTest checkTextualSpan),
         ("Textual.break", TextualTest checkTextualBreak),
         ("Textual.split", TextualTest checkTextualSplit),
         ("Textual.find", TextualTest checkTextualFind),
         ("stripPrefix", LeftReductiveTest checkStripPrefix),
         ("isPrefixOf", LeftReductiveTest checkIsPrefixOf),
         ("stripSuffix", RightReductiveTest checkStripSuffix),
         ("isSuffixOf", RightReductiveTest checkIsSuffixOf),
         ("</>", ReductiveTest checkUnAppend),
         ("cancellative stripPrefix", LeftCancellativeTest checkStripPrefix'),
         ("cancellative stripSuffix", RightCancellativeTest checkStripSuffix'),
         ("cancellative </>", CancellativeTest checkUnAppend'),
         ("stripCommonPrefix 1", LeftGCDTest checkStripCommonPrefix1),
         ("stripCommonPrefix 2", LeftGCDTest checkStripCommonPrefix2),
         ("stripCommonSuffix 1", RightGCDTest checkStripCommonSuffix1),
         ("stripCommonSuffix 2", RightGCDTest checkStripCommonSuffix2),
         ("gcd", GCDTest checkGCD),
         ("cancellative gcd", CancellativeGCDTest checkCancellativeGCD)
        ]

checkCommutative :: forall a. (Arbitrary a, Show a, Eq a, CommutativeMonoid a) => a -> Property
checkCommutative e = forAll (arbitrary :: Gen (a, a)) (\(a, b)-> a <> b == b <> a)

checkNull :: forall a. (Arbitrary a, Show a, Eq a, MonoidNull a) => a -> Property
checkNull e = null e .&&. forAll (arbitrary :: Gen a) (\a-> null a == (a == mempty))

checkConcatFactors :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkConcatFactors e = null (factors e) .&&. forAll (arbitrary :: Gen a) check
   where check a = mconcat (factors a) == a

checkFactorsOfFactors :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkFactorsOfFactors _ = forAll (arbitrary :: Gen a) (all singleton . factors)
   where singleton prime = factors prime == [prime]

checkSplitPrimePrefix :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkSplitPrimePrefix _ = forAll (arbitrary :: Gen a) (\a-> factors a == unfoldr splitPrimePrefix a)

checkSplitPrimeSuffix :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkSplitPrimeSuffix _ = forAll (arbitrary :: Gen a) check
   where check a = factors a == reverse (unfoldr (fmap swap . splitPrimeSuffix) a)

checkPrimePrefix :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkPrimePrefix _ = forAll (arbitrary :: Gen a) (\a-> primePrefix a == maybe mempty fst (splitPrimePrefix a))

checkPrimeSuffix :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkPrimeSuffix _ = forAll (arbitrary :: Gen a) (\a-> primeSuffix a == maybe mempty snd (splitPrimeSuffix a))

checkLeftFold :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkLeftFold _ = forAll (arbitrary :: Gen a) (\a-> foldl (flip (:)) [] a == List.foldl (flip (:)) [] (factors a))

checkLeftFold' :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkLeftFold' _ = forAll (arbitrary :: Gen a) (\a-> foldl' (flip (:)) [] a == List.foldl' (flip (:)) [] (factors a))

checkRightFold :: forall a. (Arbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkRightFold _ = forAll (arbitrary :: Gen a) (\a-> foldr (:) [] a == List.foldr (:) [] (factors a))

checkLength :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkLength _ = forAll (arbitrary :: Gen a) (\a-> length a == List.length (factors a))

checkSpan :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkSpan _ = property $ \p-> forAll (arbitrary :: Gen a) (check p)
   where check p a = span p a == (mconcat l, mconcat r)
            where (l, r) = List.span p (factors a)

checkSplit :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkSplit _ = forAll (arbitrary :: Gen a) check
   where check a = property (\pred-> all (all (not . pred) . factors) (split pred a))
                   .&&. property (\prime-> mconcat (intersperse prime $ split (== prime) a) == a)

checkSplitAt :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkSplitAt _ = property $ \i-> forAll (arbitrary :: Gen a) (check i)
   where check i a = splitAt i a == (mconcat l, mconcat r)
            where (l, r) = List.splitAt i (factors a)

checkReverse :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) => a -> Property
checkReverse _ = property $ forAll (arbitrary :: Gen a) (\a-> reverse a == mconcat (List.reverse $ factors a))

checkFromText :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkFromText _ = forAll (arbitrary :: Gen Text) (\t-> Textual.fromText t == (fromString (Text.unpack t) :: a))

checkSingleton :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkSingleton _ = forAll (arbitrary :: Gen Char) (\c-> Textual.singleton c == (fromString [c] :: a))

checkSplitCharacterPrefix :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkSplitCharacterPrefix _ = forAll (arbitrary :: Gen (Char, a)) check
   where check p@(c, t) = Textual.splitCharacterPrefix (Textual.singleton c <> t) == Just p
                          && Textual.splitCharacterPrefix (primePrefix t)
                             == fmap (\(c, t)-> (c, mempty)) (Textual.splitCharacterPrefix t)

checkCharacterPrefix :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkCharacterPrefix _ = forAll (arbitrary :: Gen a) check
   where check t = Textual.characterPrefix t == fmap fst (Textual.splitCharacterPrefix t)

checkTextualFactors :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualFactors _ = forAll (arbitrary :: Gen a) check
   where check a = all (maybe True (null . snd) . Textual.splitCharacterPrefix) (factors a)

checkUnfoldrToFactors :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkUnfoldrToFactors _ = forAll (arbitrary :: Gen a) check
   where check a = factors a == unfoldr splitPrimePrefix a

checkFactorsFromString :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkFactorsFromString _ = forAll (arbitrary :: Gen String) check
   where check s = unfoldr Textual.splitCharacterPrefix (fromString s :: a) == s

checkTextualMap :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualMap _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.map succ a == Textual.concatMap (Textual.singleton . succ) a
                    && Textual.map id a == a
         check2 s = Textual.map succ (fromString s :: a) == fromString (List.map succ s)

checkConcatMap :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkConcatMap _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.concatMap (fromString . f) a == mconcat (map apply $ factors a)
                    && Textual.concatMap Textual.singleton a == a
         check2 s = Textual.concatMap (fromString . f) (fromString s :: a) == fromString (List.concatMap f s)
         f = replicate 3
         apply prime = maybe prime (fromString . f) (Textual.characterPrefix prime)

checkAll :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkAll _ = forAll (arbitrary :: Gen a) check
   where check a = Textual.all isLetter a == Textual.foldr (const id) ((&&) . isLetter) True a

checkAny :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkAny _ = forAll (arbitrary :: Gen a) check
   where check a = Textual.any isLetter a == Textual.foldr (const id) ((||) . isLetter) False a

checkTextualFoldl :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualFoldl _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl (\l a-> Left a : l) (\l c-> Right c : l) [] a == List.reverse (textualFactors a)
                    && Textual.foldl (<>) (\a-> (a <>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldl undefined (flip (:)) [] s == List.foldl (flip (:)) [] s

checkTextualFoldr :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualFoldr _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldr (\a l-> Left a : l) (\c l-> Right c : l) [] a == textualFactors a
                    && Textual.foldr (<>) ((<>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldr undefined (:) [] s == s

checkTextualFoldl' :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualFoldl' _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl' (\l a-> Left a : l) (\l c-> Right c : l) [] a == List.reverse (textualFactors a)
                    && Textual.foldl' (<>) (\a-> (a <>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldl' undefined (flip (:)) [] s == List.foldl' (flip (:)) [] s

checkTextualScanl :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualScanl _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = (rights . textualFactors . Textual.scanl f 'Z') a == (List.scanl f 'Z' . rights . textualFactors) a
                    && (lefts . textualFactors . Textual.scanl f 'Y') a == (lefts . textualFactors) a
                    && Textual.scanl f 'W' a == Textual.scanl1 f (Textual.singleton 'W' <> a)
         check2 s = Textual.scanl f 'X' (fromString s :: a) == fromString (List.scanl f 'X' s)
         f c1 c2 = succ (max c1 c2)

checkTextualScanr :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualScanr _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = (rights . textualFactors . Textual.scanr f 'Z') a == (List.scanr f 'Z' . rights . textualFactors) a
                    && (lefts . textualFactors . Textual.scanr f 'Y') a == (lefts . textualFactors) a
                    && Textual.scanr f 'W' a == Textual.scanr1 f (a <> Textual.singleton 'W')
         check2 s = Textual.scanr f 'X' (fromString s :: a) == fromString (List.scanr f 'X' s)
         f c1 c2 = succ (max c1 c2)

checkTextualScanl1 :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualScanl1 _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.scanl1 (const id) a == a
         check2 s = Textual.scanl1 f (fromString s :: a) == fromString (List.scanl1 f s)
         f c1 c2 = succ (max c1 c2)

checkTextualScanr1 :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualScanr1 _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.scanr1 const a == a
         check2 s = Textual.scanr1 f (fromString s :: a) == fromString (List.scanr1 f s)
         f c1 c2 = succ (max c1 c2)

checkTextualMapAccumL :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualMapAccumL _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = uncurry (Textual.mapAccumL (,)) ((), a) == ((), a)
         check2 s = Textual.mapAccumL f c (fromString s :: a) == fmap fromString (List.mapAccumL f c s)
         c = 0 :: Int
         f n c = if isLetter c then (succ n, succ c) else (2*n, c)

checkTextualMapAccumR :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualMapAccumR _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = uncurry (Textual.mapAccumR (,)) ((), a) == ((), a)
         check2 s = Textual.mapAccumR f c (fromString s :: a) == fmap fromString (List.mapAccumR f c s)
         c = 0 :: Int
         f n c = if isLetter c then (succ n, succ c) else (2*n, c)

checkTextualTakeWhile :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualTakeWhile _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = textualFactors (Textual.takeWhile (const True) isLetter a)
                    == List.takeWhile (either (const True) isLetter) (textualFactors a)
                    && Textual.takeWhile (const True) (const True) a == a
         check2 s = Textual.takeWhile undefined isLetter (fromString s :: a) == fromString (List.takeWhile isLetter s)

checkTextualDropWhile :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualDropWhile _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = textualFactors (Textual.dropWhile (const True) isLetter a)
                    == List.dropWhile (either (const True) isLetter) (textualFactors a)
                    && Textual.dropWhile (const False) (const False) a == a
         check2 s = Textual.dropWhile undefined isLetter (fromString s :: a)
                    == fromString (List.dropWhile isLetter s)

checkTextualSpan :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualSpan _ = forAll (arbitrary :: Gen a) check
   where check a = Textual.span pt pc a == (Textual.takeWhile pt pc a, Textual.dropWhile pt pc a)
            where pt = (== primePrefix a)
         pc = isLetter

checkTextualBreak :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualBreak _ = forAll (arbitrary :: Gen a) check
   where check a = Textual.break pt pc a == Textual.span (not . pt) (not . pc) a
            where pt = (/= primePrefix a)
         pc = isLetter

checkTextualSplit :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualSplit _ = forAll (arbitrary :: Gen a) check
   where check a = List.all (List.all isLetter . rights . textualFactors) (Textual.split (not . isLetter) a)
                   && (mconcat . intersperse (fromString " ") . Textual.split (== ' ')) a == a

checkTextualFind :: forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => a -> Property
checkTextualFind _ = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.find isLetter a == (List.find isLetter . rights . textualFactors) a
         check2 s = Textual.find isLetter (fromString s :: a) == List.find isLetter s

checkStripPrefix :: forall a. (Arbitrary a, Show a, Eq a, LeftReductiveMonoid a) => a -> Property
checkStripPrefix _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = maybe b (a <>) (stripPrefix a b) == b

checkIsPrefixOf :: forall a. (Arbitrary a, Show a, Eq a, LeftReductiveMonoid a) => a -> Property
checkIsPrefixOf _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = isPrefixOf a b == isJust (stripPrefix a b)
                        && a `isPrefixOf` (a <> b)

checkStripSuffix :: forall a. (Arbitrary a, Show a, Eq a, RightReductiveMonoid a) => a -> Property
checkStripSuffix _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = maybe b (<> a) (stripSuffix a b) == b

checkIsSuffixOf :: forall a. (Arbitrary a, Show a, Eq a, RightReductiveMonoid a) => a -> Property
checkIsSuffixOf _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = isSuffixOf a b == isJust (stripSuffix a b)
                        && b `isSuffixOf` (a <> b)

checkUnAppend :: forall a. (Arbitrary a, Show a, Eq a, ReductiveMonoid a) => a -> Property
checkUnAppend _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = maybe a (b <>) (a </> b) == a
                        && maybe a (<> b) (a </> b) == a

checkStripPrefix' :: forall a. (Arbitrary a, Show a, Eq a, LeftCancellativeMonoid a) => a -> Property
checkStripPrefix' _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripPrefix a (a <> b) == Just b

checkStripSuffix' :: forall a. (Arbitrary a, Show a, Eq a, RightCancellativeMonoid a) => a -> Property
checkStripSuffix' _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripSuffix b (a <> b) == Just a

checkUnAppend' :: forall a. (Arbitrary a, Show a, Eq a, CancellativeMonoid a) => a -> Property
checkUnAppend' _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = a <> b </> a == Just b
                        && a <> b </> b == Just a

checkStripCommonPrefix1 :: forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a) => a -> Property
checkStripCommonPrefix1 _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripCommonPrefix a b == (p, a', b')
            where p = commonPrefix a b
                  Just a' = stripPrefix p a
                  Just b' = stripPrefix p b

checkStripCommonPrefix2 :: forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a) => a -> Property
checkStripCommonPrefix2 _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = p == commonPrefix a b && p <> a' == a && p <> b' == b
            where (p, a', b') = stripCommonPrefix a b

checkStripCommonSuffix1 :: forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a) => a -> Property
checkStripCommonSuffix1 _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripCommonSuffix a b == (a', b', s)
            where s = commonSuffix a b
                  Just a' = stripSuffix s a
                  Just b' = stripSuffix s b

checkStripCommonSuffix2 :: forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a) => a -> Property
checkStripCommonSuffix2 _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = s == commonSuffix a b && a' <> s == a && b' <> s == b
            where (a', b', s) = stripCommonSuffix a b

checkGCD :: forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a) => a -> Property
checkGCD _ = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = d == commonPrefix a b
                        && d == commonSuffix a b
                        && isJust (a </> d)
                        && isJust (b </> d)
            where d = gcd a b

checkCancellativeGCD :: forall a. (Arbitrary a, Show a, Eq a, CancellativeMonoid a, GCDMonoid a) => a -> Property
checkCancellativeGCD _ = forAll (arbitrary :: Gen (a, a, a)) check
   where check (a, b, c) = commonPrefix (a <> b) (a <> c) == a <> (commonPrefix b c)
                           && commonSuffix (a <> c) (b <> c) == (commonSuffix a b) <> c
                           && gcd (a <> b) (a <> c) == a <> gcd b c
                           && gcd (a <> c) (b <> c) == gcd a b <> c

textualFactors :: TextualMonoid t => t -> [Either t Char]
textualFactors = map characterize . factors
   where characterize prime = maybe (Left prime) Right (Textual.characterPrefix prime)

newtype TestString = TestString String deriving (Eq, Show, Arbitrary, CoArbitrary, 
                                                 Monoid, LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid,
                                                 MonoidNull, IsString)

instance FactorialMonoid TestString where
   splitPrimePrefix (TestString []) = Nothing
   splitPrimePrefix (TestString (x:xs)) = Just (TestString [x], TestString xs)

instance TextualMonoid TestString where
   splitCharacterPrefix (TestString []) = Nothing
   splitCharacterPrefix (TestString (x:xs)) = Just (x, TestString xs)

instance Show a => Show (a -> Bool) where
   show _ = "predicate"

instance Arbitrary All where
   arbitrary = fmap All arbitrary

instance Arbitrary Any where
   arbitrary = fmap Any arbitrary

instance Arbitrary a => Arbitrary (Dual a) where
   arbitrary = fmap Dual arbitrary

instance Arbitrary a => Arbitrary (First a) where
   arbitrary = fmap First arbitrary

instance Arbitrary a => Arbitrary (Last a) where
   arbitrary = fmap Last arbitrary

instance Arbitrary a => Arbitrary (Product a) where
   arbitrary = fmap Product arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
   arbitrary = fmap Sum arbitrary

instance Arbitrary a => Arbitrary (Vector a) where
   arbitrary = fmap fromList arbitrary

instance Arbitrary ByteStringUTF8 where
   arbitrary = fmap ByteStringUTF8 arbitrary

instance CoArbitrary All where
   coarbitrary (All p) = coarbitrary p

instance CoArbitrary Any where
   coarbitrary (Any p) = coarbitrary p

instance CoArbitrary a => CoArbitrary (Dual a) where
   coarbitrary (Dual a) = coarbitrary a

instance CoArbitrary a => CoArbitrary (First a) where
   coarbitrary (First a) = coarbitrary a

instance CoArbitrary a => CoArbitrary (Last a) where
   coarbitrary (Last a) = coarbitrary a

instance CoArbitrary a => CoArbitrary (Product a) where
   coarbitrary (Product a) = coarbitrary a

instance CoArbitrary a => CoArbitrary (Sum a) where
   coarbitrary (Sum a) = coarbitrary a

instance CoArbitrary a => CoArbitrary (Vector a) where
   coarbitrary = coarbitrary . toList

instance CoArbitrary ByteStringUTF8 where
   coarbitrary (ByteStringUTF8 bs) = coarbitrary bs
