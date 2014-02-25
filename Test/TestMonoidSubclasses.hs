{- 
    Copyright 2013 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

{-# LANGUAGE CPP, Rank2Types, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Prelude hiding (foldl, foldr, gcd, length, null, reverse, span, splitAt, takeWhile)

import Test.QuickCheck (Arbitrary, CoArbitrary, Property, Gen,
                        quickCheck, arbitrary, coarbitrary, property, label, forAll, variant, whenFail, (.&&.))
import Test.QuickCheck.Instances ()

import Control.Applicative (Applicative(..), liftA2)
import Data.Functor ((<$>))
import Data.Foldable (toList)
import Data.Int (Int8, Int32)
import qualified Data.Foldable as Foldable
import Data.Traversable (Traversable)
import Data.List (intersperse, unfoldr)
import qualified Data.List as List
import Data.Maybe (isJust)
import Data.Either (lefts, rights)
import Data.Tuple (swap)
import Data.String (IsString, fromString)
import Data.Char (isLetter)
import Data.Int (Int16)
import Data.Word (Word, Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)
import qualified Data.Text as Text
import qualified Data.Sequence as Sequence
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Vector (Vector, fromList)

import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8(ByteStringUTF8))
import Data.Monoid.Instances.Concat (Concat)
import qualified Data.Monoid.Instances.Concat as Concat
import Data.Monoid.Instances.Measured (Measured)
import qualified Data.Monoid.Instances.Measured as Measured
import Data.Monoid.Instances.Stateful (Stateful)
import qualified Data.Monoid.Instances.Stateful as Stateful
import Data.Monoid.Instances.Positioned (OffsetPositioned, LinePositioned)
import qualified Data.Monoid.Instances.Positioned as Positioned

import Data.Monoid (Monoid, mempty, (<>), mconcat, All(All), Any(Any), Dual(Dual),
                    First(First), Last(Last), Sum(Sum), Product(Product))
import Data.Monoid.Null (MonoidNull, PositiveMonoid, null)
import Data.Monoid.Factorial (FactorialMonoid, StableFactorialMonoid, 
                              factors, splitPrimePrefix, splitPrimeSuffix, primePrefix, primeSuffix,
                              foldl, foldl', foldr, length, reverse, span, split, splitAt)
import Data.Monoid.Cancellative (CommutativeMonoid, ReductiveMonoid, LeftReductiveMonoid, RightReductiveMonoid,
                                 CancellativeMonoid, LeftCancellativeMonoid, RightCancellativeMonoid,
                                 GCDMonoid, LeftGCDMonoid, RightGCDMonoid,
                                 (</>), gcd,
                                 isPrefixOf, stripPrefix, commonPrefix, stripCommonPrefix,
                                 isSuffixOf, stripSuffix, commonSuffix, stripCommonSuffix)
import Data.Monoid.Textual (TextualMonoid)
import qualified Data.Monoid.Textual as Textual

data Test = CommutativeTest (CommutativeMonoidInstance -> Property)
          | NullTest (NullMonoidInstance -> Property)
          | PositiveTest (PositiveMonoidInstance -> Property)
          | FactorialTest (FactorialMonoidInstance -> Property)
          | StableFactorialTest (StableFactorialMonoidInstance -> Property)
          | TextualTest (TextualMonoidInstance -> Property)
          | LeftReductiveTest (LeftReductiveMonoidInstance -> Property)
          | RightReductiveTest (RightReductiveMonoidInstance -> Property)
          | ReductiveTest (ReductiveMonoidInstance -> Property)
          | LeftCancellativeTest (LeftCancellativeMonoidInstance -> Property)
          | RightCancellativeTest (RightCancellativeMonoidInstance -> Property)
          | CancellativeTest (CancellativeMonoidInstance -> Property)
          | LeftGCDTest (LeftGCDMonoidInstance -> Property)
          | RightGCDTest (RightGCDMonoidInstance -> Property)
          | GCDTest (GCDMonoidInstance -> Property)
          | CancellativeGCDTest (CancellativeGCDMonoidInstance -> Property)

data CommutativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, CommutativeMonoid a) => 
                                 CommutativeMonoidInstance a
data NullMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, MonoidNull a) => 
                          NullMonoidInstance a
data PositiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, PositiveMonoid a) =>
                              PositiveMonoidInstance a
data FactorialMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) =>
                               FactorialMonoidInstance a
data StableFactorialMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, StableFactorialMonoid a) =>
                                     StableFactorialMonoidInstance a
data TextualMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) => 
                             TextualMonoidInstance a
data StableTextualMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, StableFactorialMonoid a,
                                              TextualMonoid a) =>
                                   StableTextualMonoidInstance a
data LeftReductiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, LeftReductiveMonoid a) => 
                                   LeftReductiveMonoidInstance a
data RightReductiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, RightReductiveMonoid a) => 
                                    RightReductiveMonoidInstance a
data ReductiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, ReductiveMonoid a) => 
                               ReductiveMonoidInstance a
data LeftCancellativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, LeftCancellativeMonoid a) => 
                                      LeftCancellativeMonoidInstance a
data RightCancellativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, RightCancellativeMonoid a) => 
                                       RightCancellativeMonoidInstance a
data CancellativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, CancellativeMonoid a) => 
                                  CancellativeMonoidInstance a
data LeftGCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a) => 
                             LeftGCDMonoidInstance a
data RightGCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a) => 
                              RightGCDMonoidInstance a
data GCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a) => 
                         GCDMonoidInstance a
data CancellativeGCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, CancellativeMonoid a, GCDMonoid a) => 
                                     CancellativeGCDMonoidInstance a

commutativeInstances :: [CommutativeMonoidInstance]
commutativeInstances = map upcast reductiveInstances
                       ++ [CommutativeMonoidInstance (mempty :: Product Double)]
   where upcast (ReductiveMonoidInstance i) = CommutativeMonoidInstance i

nullInstances :: [NullMonoidInstance]
nullInstances = map upcast factorialInstances
                ++ [NullMonoidInstance (mempty :: Ordering),
                    NullMonoidInstance (mempty :: All),
                    NullMonoidInstance (mempty :: Any),
                    NullMonoidInstance (mempty :: Sum Float),
                    NullMonoidInstance (mempty :: Product Int),
                    NullMonoidInstance (mempty :: First Int),
                    NullMonoidInstance (mempty :: Last Int),
                    NullMonoidInstance (mempty :: Concat Any),
                    NullMonoidInstance (mempty :: Concat (Dual String)),
                    NullMonoidInstance (mempty :: Concat (Map String Int))]
   where upcast (FactorialMonoidInstance i) = NullMonoidInstance i

positiveInstances = map upcast stableFactorialInstances
                     ++ [PositiveMonoidInstance (mempty :: ()),
                         PositiveMonoidInstance (mempty :: Ordering),
                         PositiveMonoidInstance (mempty :: All),
                         PositiveMonoidInstance (mempty :: Any),
                         PositiveMonoidInstance (mempty :: (Maybe (Sum Int))),
                         PositiveMonoidInstance (mempty :: (First Char)),
                         PositiveMonoidInstance (mempty :: (Last Int)),
                         PositiveMonoidInstance (mempty :: String),
                         PositiveMonoidInstance (mempty :: (Map Int16 Int)),
                         PositiveMonoidInstance (mempty :: (IntMap Char)),
                         PositiveMonoidInstance (mempty :: IntSet),
                         PositiveMonoidInstance (mempty :: (Set Float)),
                         PositiveMonoidInstance (mempty :: (Dual ()))]
   where upcast (StableFactorialMonoidInstance i) = PositiveMonoidInstance i

factorialInstances :: [FactorialMonoidInstance]
factorialInstances = map upcast stableFactorialInstances
                     ++ [FactorialMonoidInstance (mempty :: Sum Int8),
                         FactorialMonoidInstance (mempty :: Product Int32),
                         FactorialMonoidInstance (mempty :: Maybe String),
                         FactorialMonoidInstance (mempty :: (Text, String)),
                         FactorialMonoidInstance (mempty :: IntMap Int),
                         FactorialMonoidInstance (mempty :: IntSet),
                         FactorialMonoidInstance (mempty :: Map String Int),
                         FactorialMonoidInstance (mempty :: Set String),
                         FactorialMonoidInstance (mempty :: Concat ByteString),
                         FactorialMonoidInstance (mempty :: Concat (Dual ByteString)),
                         FactorialMonoidInstance (mempty :: Concat (Maybe String)),
                         FactorialMonoidInstance (mempty :: Concat (Text, String)),
                         FactorialMonoidInstance (mempty :: Concat (IntMap Int))]
   where upcast (StableFactorialMonoidInstance i) = FactorialMonoidInstance i

stableFactorialInstances :: [StableFactorialMonoidInstance]
stableFactorialInstances = stable1 ++ map measure stable1 ++ map position stable1 
   where stable1 = map upcast stableTextualInstances
                   ++ [StableFactorialMonoidInstance (mempty :: ByteString),
                       StableFactorialMonoidInstance (mempty :: Lazy.ByteString),
                       StableFactorialMonoidInstance (mempty :: Dual String),
                       StableFactorialMonoidInstance (mempty :: Seq Int),
                       StableFactorialMonoidInstance (mempty :: Vector Int)]
         upcast (StableTextualMonoidInstance i) = StableFactorialMonoidInstance i
         measure (StableFactorialMonoidInstance i) = StableFactorialMonoidInstance (Measured.measure i)
         position (StableFactorialMonoidInstance (i :: a)) = 
            StableFactorialMonoidInstance (pure i :: OffsetPositioned a)

textualInstances :: [TextualMonoidInstance]
textualInstances = map upcast stableTextualInstances
                   ++ [TextualMonoidInstance (mempty :: ByteStringUTF8),
                       TextualMonoidInstance (mempty :: Text),
                       TextualMonoidInstance (mempty :: Lazy.Text),
                       TextualMonoidInstance (mempty :: Seq Char),
                       TextualMonoidInstance (mempty :: Vector Char),
                       TextualMonoidInstance (mempty :: Stateful (IntMap Int) Text)]
   where upcast (StableTextualMonoidInstance i) = TextualMonoidInstance i

stableTextualInstances :: [StableTextualMonoidInstance]
stableTextualInstances = stable1 ++ map measure stable1 ++ concatMap position stable1
   where stable1 = [StableTextualMonoidInstance (mempty :: TestString),
                    StableTextualMonoidInstance (mempty :: String),
                    StableTextualMonoidInstance (mempty :: Text),
                    StableTextualMonoidInstance (mempty :: Lazy.Text),
                    StableTextualMonoidInstance (mempty :: Seq Char),
                    StableTextualMonoidInstance (mempty :: Vector Char)]
         measure (StableTextualMonoidInstance i) = StableTextualMonoidInstance (Measured.measure i)
         position (StableTextualMonoidInstance (i :: a)) = 
            [StableTextualMonoidInstance (pure i :: OffsetPositioned a),
             StableTextualMonoidInstance (pure i :: LinePositioned a)]

leftReductiveInstances = map upcast leftCancellativeInstances
                         ++ [LeftReductiveMonoidInstance (mempty :: Sum Integer),
                             LeftReductiveMonoidInstance (mempty :: IntSet),
                             LeftReductiveMonoidInstance (mempty :: Set Integer),
                             LeftReductiveMonoidInstance (mempty :: Concat String),
                             LeftReductiveMonoidInstance (mempty :: Concat ByteString),
                             LeftReductiveMonoidInstance (mempty :: Concat Lazy.ByteString),
                             LeftReductiveMonoidInstance (mempty :: Concat Text),
                             LeftReductiveMonoidInstance (mempty :: Concat Lazy.Text),
                             LeftReductiveMonoidInstance (mempty :: Concat (Dual Text))]
   where upcast (LeftCancellativeMonoidInstance i) = LeftReductiveMonoidInstance i

rightReductiveInstances = map upcast rightCancellativeInstances
                          ++ [RightReductiveMonoidInstance (mempty :: Product Integer),
                              RightReductiveMonoidInstance (mempty :: IntSet),
                              RightReductiveMonoidInstance (mempty :: Set String),
                              RightReductiveMonoidInstance (mempty :: Concat ByteString),
                              RightReductiveMonoidInstance (mempty :: Concat Lazy.ByteString),
                              RightReductiveMonoidInstance (mempty :: Concat Text),
                              RightReductiveMonoidInstance (mempty :: Concat Lazy.Text),
                              RightReductiveMonoidInstance (mempty :: Concat (Dual Text))]
   where upcast (RightCancellativeMonoidInstance i) = RightReductiveMonoidInstance i

reductiveInstances = map upcast cancellativeInstances
                     ++ [ReductiveMonoidInstance (mempty :: Product Integer),
                         ReductiveMonoidInstance (mempty :: IntSet),
                         ReductiveMonoidInstance (mempty :: Set Integer)]
   where upcast (CancellativeMonoidInstance i) = ReductiveMonoidInstance i

leftCancellativeInstances = map upcast cancellativeInstances
                            ++ [LeftCancellativeMonoidInstance (mempty :: String),
                                LeftCancellativeMonoidInstance (mempty :: ByteString),
                                LeftCancellativeMonoidInstance (mempty :: Lazy.ByteString),
                                LeftCancellativeMonoidInstance (mempty :: Text),
                                LeftCancellativeMonoidInstance (mempty :: Lazy.Text),
                                LeftCancellativeMonoidInstance (mempty :: Dual Text),
                                LeftCancellativeMonoidInstance (mempty :: (Text, String)),
                                LeftCancellativeMonoidInstance (mempty :: Seq Int),
                                LeftCancellativeMonoidInstance (mempty :: Vector Int)]
   where upcast (CancellativeMonoidInstance i) = LeftCancellativeMonoidInstance i

rightCancellativeInstances = map upcast cancellativeInstances
                            ++ [RightCancellativeMonoidInstance (mempty :: ByteString),
                                RightCancellativeMonoidInstance (mempty :: Lazy.ByteString),
                                RightCancellativeMonoidInstance (mempty :: Text),
                                RightCancellativeMonoidInstance (mempty :: Lazy.Text),
                                RightCancellativeMonoidInstance (mempty :: Dual String),
                                RightCancellativeMonoidInstance (mempty :: (Text, ByteString)),
                                RightCancellativeMonoidInstance (mempty :: Seq Int),
                                RightCancellativeMonoidInstance (mempty :: Vector Int)]
   where upcast (CancellativeMonoidInstance i) = RightCancellativeMonoidInstance i

cancellativeInstances = map upcast cancellativeGCDInstances
                        ++ []
   where upcast (CancellativeGCDMonoidInstance i) = CancellativeMonoidInstance i

leftGCDInstances = map upcast gcdInstances
                   ++ [LeftGCDMonoidInstance (mempty :: String),
                       LeftGCDMonoidInstance (mempty :: ByteString),
                       LeftGCDMonoidInstance (mempty :: Lazy.ByteString),
                       LeftGCDMonoidInstance (mempty :: Text),
                       LeftGCDMonoidInstance (mempty :: Lazy.Text),
                       LeftGCDMonoidInstance (mempty :: Dual ByteString),
                       LeftGCDMonoidInstance (mempty :: (Text, String)),
                       LeftGCDMonoidInstance (mempty :: IntMap Int),
                       LeftGCDMonoidInstance (mempty :: Map String Int),
                       LeftGCDMonoidInstance (mempty :: Seq Int),
                       LeftGCDMonoidInstance (mempty :: Vector Int),
                       LeftGCDMonoidInstance (mempty :: Concat String),
                       LeftGCDMonoidInstance (mempty :: Concat ByteString),
                       LeftGCDMonoidInstance (mempty :: Concat Lazy.ByteString),
                       LeftGCDMonoidInstance (mempty :: Concat Text),
                       LeftGCDMonoidInstance (mempty :: Concat Lazy.Text),
                       LeftGCDMonoidInstance (mempty :: Concat (Dual ByteString))]
   where upcast (GCDMonoidInstance i) = LeftGCDMonoidInstance i

rightGCDInstances = map upcast gcdInstances
                   ++ [RightGCDMonoidInstance (mempty :: ByteString),
                       RightGCDMonoidInstance (mempty :: Lazy.ByteString),
                       RightGCDMonoidInstance (mempty :: Dual String),
                       RightGCDMonoidInstance (mempty :: (Seq Int, ByteString)),
                       RightGCDMonoidInstance (mempty :: Seq Int),
                       RightGCDMonoidInstance (mempty :: Vector Int),
                       RightGCDMonoidInstance (mempty :: Concat ByteString),
                       RightGCDMonoidInstance (mempty :: Concat Lazy.ByteString),
                       RightGCDMonoidInstance (mempty :: Concat (Dual Text))]
   where upcast (GCDMonoidInstance i) = RightGCDMonoidInstance i

gcdInstances = map upcast cancellativeGCDInstances
               ++ [GCDMonoidInstance (mempty :: Product Integer),
                   GCDMonoidInstance (mempty :: Dual (Product Integer)),
                   GCDMonoidInstance (mempty :: IntSet),
                   GCDMonoidInstance (mempty :: Set String)]
   where upcast (CancellativeGCDMonoidInstance i) = GCDMonoidInstance i

cancellativeGCDInstances = [CancellativeGCDMonoidInstance (),
                            CancellativeGCDMonoidInstance (mempty :: Sum Integer),
                            CancellativeGCDMonoidInstance (mempty :: Dual (Sum Integer)),
                            CancellativeGCDMonoidInstance (mempty :: (Sum Integer, Sum Int))]

main = mapM_ (quickCheck . uncurry checkInstances) tests

checkInstances :: String -> Test -> Property
checkInstances name (CommutativeTest checkType) = label name $ foldr1 (.&&.) (map checkType commutativeInstances)
checkInstances name (NullTest checkType) = label name $ foldr1 (.&&.) (map checkType nullInstances)
checkInstances name (PositiveTest checkType) = label name $ foldr1 (.&&.) (map checkType positiveInstances)
checkInstances name (FactorialTest checkType) = label name $ foldr1 (.&&.) (map checkType factorialInstances)
checkInstances name (StableFactorialTest checkType) =
   label name $ foldr1 (.&&.) (map checkType stableFactorialInstances)
checkInstances name (TextualTest checkType) = label name $ foldr1 (.&&.) (map checkType textualInstances)
checkInstances name (LeftReductiveTest checkType) = label name $ foldr1 (.&&.) (map checkType leftReductiveInstances)
checkInstances name (RightReductiveTest checkType) = label name $ foldr1 (.&&.) (map checkType rightReductiveInstances)
checkInstances name (ReductiveTest checkType) = label name $ foldr1 (.&&.) (map checkType reductiveInstances)
checkInstances name (LeftCancellativeTest checkType) =
   label name $ foldr1 (.&&.) (map checkType leftCancellativeInstances) 
checkInstances name (RightCancellativeTest checkType) =
   label name $ foldr1 (.&&.) (map checkType rightCancellativeInstances) 
checkInstances name (CancellativeTest checkType) = label name $ foldr1 (.&&.) (map checkType cancellativeInstances) 
checkInstances name (LeftGCDTest checkType) = label name $ foldr1 (.&&.) (map checkType leftGCDInstances) 
checkInstances name (RightGCDTest checkType) = label name $ foldr1 (.&&.) (map checkType rightGCDInstances) 
checkInstances name (GCDTest checkType) = label name $ foldr1 (.&&.) (map checkType gcdInstances)  
checkInstances name (CancellativeGCDTest checkType) = 
   label name $ foldr1 (.&&.) (map checkType cancellativeGCDInstances) 

tests :: [(String, Test)]
tests = [("CommutativeMonoid", CommutativeTest checkCommutative),
         ("MonoidNull", NullTest checkNull),
         ("PositiveMonoid", PositiveTest checkPositive),
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
         ("stable", StableFactorialTest checkStability),
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

checkCommutative (CommutativeMonoidInstance (e :: a)) = forAll (arbitrary :: Gen (a, a)) (\(a, b)-> a <> b == b <> a)

checkNull (NullMonoidInstance (e :: a)) = null e .&&. forAll (arbitrary :: Gen a) (\a-> null a == (a == mempty))

checkPositive (PositiveMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen (a, a)) (\(a, b)-> null a && null b || not (null (a <> b)))

checkConcatFactors (FactorialMonoidInstance (e :: a)) = null (factors e) .&&. forAll (arbitrary :: Gen a) check
   where check a = mconcat (factors a) == a

checkFactorsOfFactors (FactorialMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) (all singleton . factors)
   where singleton prime = factors prime == [prime]

checkSplitPrimePrefix (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> factors a == unfoldr splitPrimePrefix a)

checkSplitPrimeSuffix (FactorialMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = factors a == reverse (unfoldr (fmap swap . splitPrimeSuffix) a)

checkPrimePrefix (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> primePrefix a == maybe mempty fst (splitPrimePrefix a))

checkPrimeSuffix (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> primeSuffix a == maybe mempty snd (splitPrimeSuffix a))

checkLeftFold (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> foldl (flip (:)) [] a == List.foldl (flip (:)) [] (factors a))

checkLeftFold' (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> foldl' (flip (:)) [] a == List.foldl' (flip (:)) [] (factors a))

checkRightFold (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> foldr (:) [] a == List.foldr (:) [] (factors a))

checkLength (FactorialMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) (\a-> length a == List.length (factors a))

checkSpan (FactorialMonoidInstance (_ :: a)) = property $ \p-> forAll (arbitrary :: Gen a) (check p)
   where check p a = span p a == (mconcat l, mconcat r)
            where (l, r) = List.span p (factors a)

checkSplit (FactorialMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = property (\pred-> all (all (not . pred) . factors) (split pred a))
                   .&&. property (\prime-> mconcat (intersperse prime $ split (== prime) a) == a)

checkSplitAt (FactorialMonoidInstance (_ :: a)) = property $ \i-> forAll (arbitrary :: Gen a) (check i)
   where check i a = splitAt i a == (mconcat l, mconcat r)
            where (l, r) = List.splitAt i (factors a)

checkReverse (FactorialMonoidInstance (_ :: a)) = 
   property $ forAll (arbitrary :: Gen a) (\a-> reverse a == mconcat (List.reverse $ factors a))

checkStability (StableFactorialMonoidInstance (_ :: a)) =
   property $ forAll (arbitrary :: Gen (a, a)) (\(a, b)-> factors (a <> b) == factors a <> factors b)

checkFromText (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen Text) (\t-> Textual.fromText t == (fromString (Text.unpack t) :: a))

checkSingleton (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen Char) (\c-> Textual.singleton c == (fromString [c] :: a))

checkSplitCharacterPrefix (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen String) check1 .&&. forAll (arbitrary :: Gen a) check2
   where check1 s = unfoldr Textual.splitCharacterPrefix (fromString s :: a) == s
         check2 t = Textual.splitCharacterPrefix (primePrefix t)
                    == fmap (\(c, t)-> (c, mempty)) (Textual.splitCharacterPrefix t)

checkCharacterPrefix (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check t = Textual.characterPrefix t == fmap fst (Textual.splitCharacterPrefix t)

checkTextualFactors (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = all (maybe True (null . snd) . Textual.splitCharacterPrefix) (factors a)

checkUnfoldrToFactors (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = factors a == unfoldr splitPrimePrefix a

checkFactorsFromString (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen String) check
   where check s = unfoldr Textual.splitCharacterPrefix (fromString s :: a) == s

checkTextualMap (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.map succ a == Textual.concatMap (Textual.singleton . succ) a
                    && Textual.map id a == a
         check2 s = Textual.map succ (fromString s :: a) == fromString (List.map succ s)

checkConcatMap (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.concatMap (fromString . f) a == mconcat (map apply $ factors a)
                    && Textual.concatMap Textual.singleton a == a
         check2 s = Textual.concatMap (fromString . f) (fromString s :: a) == fromString (List.concatMap f s)
         f = replicate 3
         apply prime = maybe prime (fromString . f) (Textual.characterPrefix prime)

checkAll (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = Textual.all isLetter a == Textual.foldr (const id) ((&&) . isLetter) True a

checkAny (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = Textual.any isLetter a == Textual.foldr (const id) ((||) . isLetter) False a

checkTextualFoldl (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl (\l a-> Left a : l) (\l c-> Right c : l) [] a == List.reverse (textualFactors a)
                    && Textual.foldl (<>) (\a-> (a <>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldl undefined (flip (:)) [] s == List.foldl (flip (:)) [] s

checkTextualFoldr (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldr (\a l-> Left a : l) (\c l-> Right c : l) [] a == textualFactors a
                    && Textual.foldr (<>) ((<>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldr undefined (:) [] s == s

checkTextualFoldl' (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl' (\l a-> Left a : l) (\l c-> Right c : l) [] a == List.reverse (textualFactors a)
                    && Textual.foldl' (<>) (\a-> (a <>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldl' undefined (flip (:)) [] s == List.foldl' (flip (:)) [] s

checkTextualScanl (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = (rights . textualFactors . Textual.scanl f 'Z') a == (List.scanl f 'Z' . rights . textualFactors) a
                    && (lefts . textualFactors . Textual.scanl f 'Y') a == (lefts . textualFactors) a
                    && Textual.scanl f 'W' a == Textual.scanl1 f (Textual.singleton 'W' <> a)
         check2 s = Textual.scanl f 'X' (fromString s :: a) == fromString (List.scanl f 'X' s)
         f c1 c2 = min c1 c2

checkTextualScanr (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = (rights . textualFactors . Textual.scanr f 'Z') a == (List.scanr f 'Z' . rights . textualFactors) a
                    && (lefts . textualFactors . Textual.scanr f 'Y') a == (lefts . textualFactors) a
                    && Textual.scanr f 'W' a == Textual.scanr1 f (a <> Textual.singleton 'W')
         check2 s = Textual.scanr f 'X' (fromString s :: a) == fromString (List.scanr f 'X' s)
         f c1 c2 = min c1 c2

checkTextualScanl1 (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.scanl1 (const id) a == a
         check2 s = Textual.scanl1 f (fromString s :: a) == fromString (List.scanl1 f s)
         f c1 c2 = min c1 c2

checkTextualScanr1 (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.scanr1 const a == a
         check2 s = Textual.scanr1 f (fromString s :: a) == fromString (List.scanr1 f s)
         f c1 c2 = min c1 c2

checkTextualMapAccumL (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = uncurry (Textual.mapAccumL (,)) ((), a) == ((), a)
         check2 s = Textual.mapAccumL f c (fromString s :: a) == fmap fromString (List.mapAccumL f c s)
         c = 0 :: Int
         f n c = if isLetter c then (succ n, succ c) else (2*n, c)

checkTextualMapAccumR (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = uncurry (Textual.mapAccumR (,)) ((), a) == ((), a)
         check2 s = Textual.mapAccumR f c (fromString s :: a) == fmap fromString (List.mapAccumR f c s)
         c = 0 :: Int
         f n c = if isLetter c then (succ n, succ c) else (2*n, c)

checkTextualTakeWhile (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = textualFactors (Textual.takeWhile (const True) isLetter a)
                    == List.takeWhile (either (const True) isLetter) (textualFactors a)
                    && Textual.takeWhile (const True) (const True) a == a
         check2 s = Textual.takeWhile undefined isLetter (fromString s :: a) == fromString (List.takeWhile isLetter s)

checkTextualDropWhile (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = textualFactors (Textual.dropWhile (const True) isLetter a)
                    == List.dropWhile (either (const True) isLetter) (textualFactors a)
                    && Textual.dropWhile (const False) (const False) a == a
         check2 s = Textual.dropWhile undefined isLetter (fromString s :: a)
                    == fromString (List.dropWhile isLetter s)

checkTextualSpan (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = Textual.span pt pc a == (Textual.takeWhile pt pc a, Textual.dropWhile pt pc a)
            where pt = (== primePrefix a)
         pc = isLetter

checkTextualBreak (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = Textual.break pt pc a == Textual.span (not . pt) (not . pc) a
            where pt = (/= primePrefix a)
         pc = isLetter

checkTextualSplit (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = List.all (List.all isLetter . rights . textualFactors) (Textual.split (not . isLetter) a)
                   && (mconcat . intersperse (fromString " ") . Textual.split (== ' ')) a == a

checkTextualFind (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.find isLetter a == (List.find isLetter . rights . textualFactors) a
         check2 s = Textual.find isLetter (fromString s :: a) == List.find isLetter s

checkStripPrefix (LeftReductiveMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = maybe b (a <>) (stripPrefix a b) == b

checkIsPrefixOf (LeftReductiveMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = isPrefixOf a b == isJust (stripPrefix a b)
                        && a `isPrefixOf` (a <> b)

checkStripSuffix (RightReductiveMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = maybe b (<> a) (stripSuffix a b) == b

checkIsSuffixOf (RightReductiveMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = isSuffixOf a b == isJust (stripSuffix a b)
                        && b `isSuffixOf` (a <> b)

checkUnAppend (ReductiveMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = maybe a (b <>) (a </> b) == a
                        && maybe a (<> b) (a </> b) == a

checkStripPrefix' (LeftCancellativeMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripPrefix a (a <> b) == Just b

checkStripSuffix' (RightCancellativeMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripSuffix b (a <> b) == Just a

checkUnAppend' (CancellativeMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = a <> b </> a == Just b
                        && a <> b </> b == Just a

checkStripCommonPrefix1 (LeftGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripCommonPrefix a b == (p, a', b')
            where p = commonPrefix a b
                  Just a' = stripPrefix p a
                  Just b' = stripPrefix p b

checkStripCommonPrefix2 (LeftGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = p == commonPrefix a b && p <> a' == a && p <> b' == b
            where (p, a', b') = stripCommonPrefix a b

checkStripCommonSuffix1 (RightGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripCommonSuffix a b == (a', b', s)
            where s = commonSuffix a b
                  Just a' = stripSuffix s a
                  Just b' = stripSuffix s b

checkStripCommonSuffix2 (RightGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = s == commonSuffix a b && a' <> s == a && b' <> s == b
            where (a', b', s) = stripCommonSuffix a b

checkGCD (GCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = d == commonPrefix a b
                        && d == commonSuffix a b
                        && isJust (a </> d)
                        && isJust (b </> d)
            where d = gcd a b

checkCancellativeGCD (CancellativeGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (a, b, c) = commonPrefix (a <> b) (a <> c) == a <> (commonPrefix b c)
                           && commonSuffix (a <> c) (b <> c) == (commonSuffix a b) <> c
                           && gcd (a <> b) (a <> c) == a <> gcd b c
                           && gcd (a <> c) (b <> c) == gcd a b <> c

textualFactors :: TextualMonoid t => t -> [Either t Char]
textualFactors = map characterize . factors
   where characterize prime = maybe (Left prime) Right (Textual.characterPrefix prime)

newtype TestString = TestString String deriving (Eq, Show, Arbitrary, CoArbitrary, 
                                                 Monoid, LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid,
                                                 MonoidNull, PositiveMonoid, StableFactorialMonoid, IsString)

instance FactorialMonoid TestString where
   splitPrimePrefix (TestString []) = Nothing
   splitPrimePrefix (TestString (x:xs)) = Just (TestString [x], TestString xs)

instance TextualMonoid TestString where
   splitCharacterPrefix (TestString []) = Nothing
   splitCharacterPrefix (TestString (x:xs)) = Just (x, TestString xs)

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

instance (Arbitrary a, MonoidNull a, PositiveMonoid a) => Arbitrary (Concat a) where
   arbitrary = fmap Concat.concatenate arbitrary

instance (Arbitrary a, FactorialMonoid a) => Arbitrary (Measured a) where
   arbitrary = fmap Measured.measure arbitrary

instance (Arbitrary a, FactorialMonoid a) => Arbitrary (OffsetPositioned a) where
   arbitrary = fmap pure arbitrary

instance (Arbitrary a, TextualMonoid a) => Arbitrary (LinePositioned a) where
   arbitrary = fmap pure arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Stateful a b) where
   arbitrary = Stateful.Stateful <$> liftA2 (,) arbitrary arbitrary

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

instance CoArbitrary a => CoArbitrary (Concat a) where
   coarbitrary = coarbitrary . Concat.extract

instance CoArbitrary a => CoArbitrary (Measured a) where
   coarbitrary = coarbitrary . Measured.extract

instance CoArbitrary a => CoArbitrary (OffsetPositioned a) where
   coarbitrary = coarbitrary . Positioned.extract

instance CoArbitrary a => CoArbitrary (LinePositioned a) where
   coarbitrary = coarbitrary . Positioned.extract

instance CoArbitrary b => CoArbitrary (Stateful a b) where
   coarbitrary = coarbitrary . Stateful.extract

instance Show a => Show (a -> Bool) where
   show _ = "predicate"

instance (PositiveMonoid a, MonoidNull b) => PositiveMonoid (a, b)

#if MIN_VERSION_containers(0,5,2)
#else
instance Applicative Seq where
   pure = Sequence.singleton
   fs <*> xs = Foldable.foldl' add mempty fs
      where add ys f = ys <> fmap f xs

#endif