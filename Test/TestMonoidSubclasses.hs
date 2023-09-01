{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

{-# LANGUAGE CPP, Rank2Types, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{- HLINT ignore "Use camelCase" -}

module Main where

import Prelude (Bool(..), Ordering, Int, Integer, Double, Float, Char, String,
                Maybe(..), Either(..), Eq, Show, (.), ($), (*), (==), (/=),
                (&&), (||), (++), (>>=), fmap, maybe, either, map, all, not,
                undefined, const, flip, succ, uncurry, min, id, replicate,
                minBound, maxBound, otherwise, fst, snd, concatMap, mappend, div)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary, CoArbitrary, Property, Gen,
                              arbitrary, coarbitrary, property, label, forAll, mapSize, testProperty, variant, whenFail, (.&&.), (===))
import Test.QuickCheck.Instances ()

import Control.Applicative (Applicative(..), liftA2)
import Data.Functor ((<$>))
import Data.Foldable (foldMap, toList)
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
import Numeric.Natural (Natural)

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
import Text.Show.Functions

import Data.Monoid.Instances.ByteString.UTF8 (ByteStringUTF8(ByteStringUTF8))
import Data.Monoid.Instances.CharVector ()
import Data.Monoid.Instances.Concat (Concat)
import qualified Data.Monoid.Instances.Concat as Concat
import Data.Monoid.Instances.Measured (Measured)
import qualified Data.Monoid.Instances.Measured as Measured
import qualified Data.Monoid.Instances.PrefixMemory as PrefixMemory
import Data.Monoid.Instances.Stateful (Stateful)
import qualified Data.Monoid.Instances.Stateful as Stateful
import Data.Monoid.Instances.Positioned (OffsetPositioned, LinePositioned)
import qualified Data.Monoid.Instances.Positioned as Positioned

import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, mempty, mconcat, All(All), Any(Any), Dual(Dual),
                    First(First), Last(Last), Sum(Sum), Product(Product))
import Data.Semigroup.Factorial (Factorial, StableFactorial, 
                                 factors, primePrefix, primeSuffix, foldl, foldl', foldr, length, reverse)
import Data.Semigroup.Cancellative (Commutative, Reductive,
                                    LeftReductive, RightReductive,
                                    Cancellative, LeftCancellative, RightCancellative,
                                    (</>), isPrefixOf, stripPrefix, isSuffixOf, stripSuffix)
import Data.Monoid.Null (MonoidNull, PositiveMonoid, null)
import Data.Monoid.Factorial (FactorialMonoid,
                              splitPrimePrefix, splitPrimeSuffix, inits, tails, span, spanMaybe, split, splitAt)
import Data.Monoid.GCD
    ( GCDMonoid
    , LeftGCDMonoid
    , RightGCDMonoid
    , DistributiveGCDMonoid
    , LeftDistributiveGCDMonoid
    , RightDistributiveGCDMonoid
    , commonPrefix
    , commonSuffix
    , gcd
    , stripCommonPrefix
    , stripCommonSuffix
    )
import Data.Monoid.LCM
    ( LCMMonoid
    , DistributiveLCMMonoid
    , lcm
    )
import Data.Monoid.Monus (OverlappingGCDMonoid, Monus,
                          (<\>), overlap, stripOverlap, stripPrefixOverlap, stripSuffixOverlap)
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
          | OverlappingGCDTest (OverlappingGCDMonoidInstance -> Property)
          | MonusTest (MonusInstance -> Property)
          | LeftCancellativeTest (LeftCancellativeMonoidInstance -> Property)
          | RightCancellativeTest (RightCancellativeMonoidInstance -> Property)
          | CancellativeTest (CancellativeMonoidInstance -> Property)
          | LeftGCDTest (LeftGCDMonoidInstance -> Property)
          | RightGCDTest (RightGCDMonoidInstance -> Property)
          | GCDTest (GCDMonoidInstance -> Property)
          | DistributiveGCDTest (DistributiveGCDMonoidInstance -> Property)
          | LeftDistributiveGCDTest (LeftDistributiveGCDMonoidInstance -> Property)
          | RightDistributiveGCDTest (RightDistributiveGCDMonoidInstance -> Property)
          | LCMTest (LCMMonoidInstance -> Property)
          | DistributiveLCMTest (DistributiveLCMMonoidInstance -> Property)

data CommutativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Commutative a, Monoid a) =>
                                 CommutativeMonoidInstance a
data NullMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, MonoidNull a) =>
                          NullMonoidInstance a
data PositiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, PositiveMonoid a) =>
                              PositiveMonoidInstance a
data FactorialMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, FactorialMonoid a) =>
                               FactorialMonoidInstance a
data StableFactorialMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a,
                                                StableFactorial a, FactorialMonoid a, PositiveMonoid a) =>
                                     StableFactorialMonoidInstance a
data TextualMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a, TextualMonoid a) =>
                             TextualMonoidInstance a
data StableTextualMonoidInstance = forall a. (Arbitrary a, CoArbitrary a, Show a, Eq a,
                                              StableFactorial a, FactorialMonoid a, PositiveMonoid a,
                                              TextualMonoid a) =>
                                   StableTextualMonoidInstance a
data LeftReductiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Monoid a, LeftReductive a) =>
                                   LeftReductiveMonoidInstance a
data RightReductiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Monoid a, RightReductive a) =>
                                    RightReductiveMonoidInstance a
data ReductiveMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Monoid a, Reductive a) =>
                               ReductiveMonoidInstance a
data OverlappingGCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, OverlappingGCDMonoid a, FactorialMonoid a) =>
                                    OverlappingGCDMonoidInstance a
data MonusInstance = forall a. (Arbitrary a, Show a, Eq a, Monus a, FactorialMonoid a) =>
                               MonusInstance a
data LeftCancellativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Monoid a, LeftCancellative a) =>
                                      LeftCancellativeMonoidInstance a
data RightCancellativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Monoid a, RightCancellative a) =>
                                       RightCancellativeMonoidInstance a
data CancellativeMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, Monoid a, Cancellative a) =>
                                  CancellativeMonoidInstance a
data LeftGCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, LeftGCDMonoid a) =>
                             LeftGCDMonoidInstance a
data RightGCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, RightGCDMonoid a) =>
                              RightGCDMonoidInstance a
data GCDMonoidInstance = forall a. (Arbitrary a, Show a, Eq a, GCDMonoid a) =>
                         GCDMonoidInstance a

data DistributiveGCDMonoidInstance =
    forall a. (Arbitrary a, Show a, Eq a, DistributiveGCDMonoid a)
        => DistributiveGCDMonoidInstance a

data LeftDistributiveGCDMonoidInstance =
    forall a. (Arbitrary a, Show a, Eq a, LeftDistributiveGCDMonoid a)
        => LeftDistributiveGCDMonoidInstance a

data RightDistributiveGCDMonoidInstance =
    forall a. (Arbitrary a, Show a, Eq a, RightDistributiveGCDMonoid a)
        => RightDistributiveGCDMonoidInstance a

data LCMMonoidInstance =
    forall a. (Arbitrary a, Show a, Eq a, LCMMonoid a)
        => LCMMonoidInstance a

data DistributiveLCMMonoidInstance =
    forall a. (Arbitrary a, Show a, Eq a, DistributiveLCMMonoid a)
        => DistributiveLCMMonoidInstance a

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
                         PositiveMonoidInstance (mempty :: (Maybe (Sum Integer))),
                         PositiveMonoidInstance (mempty :: (Product Natural)),
                         PositiveMonoidInstance (mempty :: (Sum Natural)),
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
                     ++ [FactorialMonoidInstance (mempty :: Sum Integer),
                         FactorialMonoidInstance (mempty :: Product Int32),
                         FactorialMonoidInstance (mempty :: Maybe String),
                         FactorialMonoidInstance (mempty :: (Text, String)),
                         FactorialMonoidInstance (mempty :: (Product Int32, ByteString, Sum Integer)),
                         FactorialMonoidInstance (mempty :: (IntSet, Text, Sum Integer, String)),
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
stableFactorialInstances = stable1 ++ map measure stable1 ++ map prefixed stable1 ++ map position stable1 
   where stable1 = map upcast stableTextualInstances
                   ++ [StableFactorialMonoidInstance (mempty :: ByteString),
                       StableFactorialMonoidInstance (mempty :: Lazy.ByteString),
                       StableFactorialMonoidInstance (mempty :: Dual String),
                       StableFactorialMonoidInstance (mempty :: Seq Int),
                       StableFactorialMonoidInstance (mempty :: Vector Int)]
         upcast (StableTextualMonoidInstance i) = StableFactorialMonoidInstance i
         measure (StableFactorialMonoidInstance i) = StableFactorialMonoidInstance (Measured.measure i)
         prefixed (StableFactorialMonoidInstance i) = StableFactorialMonoidInstance (PrefixMemory.shadowed i)
         position (StableFactorialMonoidInstance (i :: a)) = 
            StableFactorialMonoidInstance (pure i :: OffsetPositioned a)

textualInstances :: [TextualMonoidInstance]
textualInstances = map upcast stableTextualInstances
                   ++ [TextualMonoidInstance (mempty :: ByteStringUTF8),
                       TextualMonoidInstance (mempty :: Text),
                       TextualMonoidInstance (mempty :: Lazy.Text),
                       TextualMonoidInstance (mempty :: Seq Char),
                       TextualMonoidInstance (mempty :: Vector Char),
                       TextualMonoidInstance (mempty :: Stateful (IntMap Int) Text),
                       TextualMonoidInstance (mempty :: TestOffsetPositionedString),
                       TextualMonoidInstance (mempty :: TestLinePositionedString)]
   where upcast (StableTextualMonoidInstance i) = TextualMonoidInstance i

stableTextualInstances :: [StableTextualMonoidInstance]
stableTextualInstances = stable1 ++ map measure stable1 ++ map prefixed stable1 ++ concatMap position stable1
   where stable1 = [StableTextualMonoidInstance (mempty :: TestString),
                    StableTextualMonoidInstance (mempty :: String),
                    StableTextualMonoidInstance (mempty :: Text),
                    StableTextualMonoidInstance (mempty :: Lazy.Text),
                    StableTextualMonoidInstance (mempty :: Seq Char),
                    StableTextualMonoidInstance (mempty :: Vector Char)]
         measure (StableTextualMonoidInstance i) = StableTextualMonoidInstance (Measured.measure i)
         prefixed (StableTextualMonoidInstance i) = StableTextualMonoidInstance (PrefixMemory.shadowed i)
         position (StableTextualMonoidInstance (i :: a)) = 
            [StableTextualMonoidInstance (pure i :: OffsetPositioned a),
             StableTextualMonoidInstance (pure i :: LinePositioned a)]

leftReductiveInstances = map upcast leftCancellativeInstances
                         ++ [LeftReductiveMonoidInstance (mempty :: Sum Integer),
                             LeftReductiveMonoidInstance (mempty :: IntSet),
                             LeftReductiveMonoidInstance (mempty :: Set Integer),
                             LeftReductiveMonoidInstance (mempty :: IntMap Char),
                             LeftReductiveMonoidInstance (mempty :: Map Char Int),
                             LeftReductiveMonoidInstance (mempty :: Concat String),
                             LeftReductiveMonoidInstance (mempty :: Concat ByteString),
                             LeftReductiveMonoidInstance (mempty :: Concat Lazy.ByteString),
                             LeftReductiveMonoidInstance (mempty :: Concat Text),
                             LeftReductiveMonoidInstance (mempty :: Concat Lazy.Text),
                             LeftReductiveMonoidInstance (mempty :: Concat (Dual Text)),
                             LeftReductiveMonoidInstance (mempty :: LinePositioned Text),
                             LeftReductiveMonoidInstance (mempty :: OffsetPositioned Text),
                             LeftReductiveMonoidInstance (mempty :: Measured Text),
                             LeftReductiveMonoidInstance (mempty :: PrefixMemory.Shadowed Text),
                             LeftReductiveMonoidInstance (mempty :: Stateful (Sum Integer) Text)]
   where upcast (LeftCancellativeMonoidInstance i) = LeftReductiveMonoidInstance i

rightReductiveInstances = map upcast rightCancellativeInstances
                          ++ [RightReductiveMonoidInstance (mempty :: Product Integer),
                              RightReductiveMonoidInstance (mempty :: IntSet),
                              RightReductiveMonoidInstance (mempty :: Map Char Int),
                              RightReductiveMonoidInstance (mempty :: IntMap Char),
                              RightReductiveMonoidInstance (mempty :: Set String),
                              RightReductiveMonoidInstance (mempty :: Concat ByteString),
                              RightReductiveMonoidInstance (mempty :: Concat Lazy.ByteString),
                              RightReductiveMonoidInstance (mempty :: Concat Text),
                              RightReductiveMonoidInstance (mempty :: Concat Lazy.Text),
                              RightReductiveMonoidInstance (mempty :: Concat (Dual Text)),
                              RightReductiveMonoidInstance (mempty :: LinePositioned Text),
                              RightReductiveMonoidInstance (mempty :: OffsetPositioned Text),
                              RightReductiveMonoidInstance (mempty :: Measured Text),
                              RightReductiveMonoidInstance (mempty :: PrefixMemory.Shadowed Text),
                              RightReductiveMonoidInstance (mempty :: Stateful (Sum Integer) Text)]
   where upcast (RightCancellativeMonoidInstance i) = RightReductiveMonoidInstance i

reductiveInstances = map upcast cancellativeInstances
                     ++ [ReductiveMonoidInstance (mempty :: Product Integer),
                         ReductiveMonoidInstance (mempty :: IntSet),
                         ReductiveMonoidInstance (mempty :: Maybe IntSet),
                         ReductiveMonoidInstance (mempty :: Set Integer)]
   where upcast (CancellativeMonoidInstance i) = ReductiveMonoidInstance i

overlappingGCDMonoidInstances = map upcast monusInstances
                               ++ [OverlappingGCDMonoidInstance (mempty :: String),
                                   OverlappingGCDMonoidInstance (mempty :: Seq Int),
                                   OverlappingGCDMonoidInstance (mempty :: ByteString),
                                   OverlappingGCDMonoidInstance (mempty :: Lazy.ByteString),
                                   OverlappingGCDMonoidInstance (mempty :: Text),
                                   OverlappingGCDMonoidInstance (mempty :: Lazy.Text),
                                   OverlappingGCDMonoidInstance (mempty :: Vector Char),
                                   OverlappingGCDMonoidInstance (mempty :: IntMap Char),
                                   OverlappingGCDMonoidInstance (mempty :: Map Char Int)]
   where upcast (MonusInstance i) = OverlappingGCDMonoidInstance i

monusInstances = [MonusInstance (mempty :: Product Natural),
                  MonusInstance (mempty :: Sum Natural),
                  MonusInstance (mempty :: Dual (Product Natural)),
                  MonusInstance (mempty :: Maybe ()),
                  MonusInstance (mempty :: Maybe (Product Natural)),
                  MonusInstance (mempty :: Maybe (Sum Natural)),
                  MonusInstance (mempty :: IntSet),
                  MonusInstance (mempty :: Set String)]

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

cancellativeInstances = [CancellativeMonoidInstance ()]

leftGCDInstances = map upcast gcdInstances
                   ++ [LeftGCDMonoidInstance (mempty :: String),
                       LeftGCDMonoidInstance (mempty :: ByteString),
                       LeftGCDMonoidInstance (mempty :: Lazy.ByteString),
                       LeftGCDMonoidInstance (mempty :: Text),
                       LeftGCDMonoidInstance (mempty :: Lazy.Text),
                       LeftGCDMonoidInstance (mempty :: Dual ByteString),
                       LeftGCDMonoidInstance (mempty :: (Text, String)),
                       LeftGCDMonoidInstance (mempty :: (ByteString, Text, String)),
                       LeftGCDMonoidInstance (mempty :: ([Word8], ByteString, String, Text)),
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
                       RightGCDMonoidInstance (mempty :: Text),
                       RightGCDMonoidInstance (mempty :: Lazy.Text),
                       RightGCDMonoidInstance (mempty :: String),
                       RightGCDMonoidInstance (mempty :: Dual String),
                       RightGCDMonoidInstance (mempty :: (Seq Int, ByteString)),
                       RightGCDMonoidInstance (mempty :: Seq Int),
                       RightGCDMonoidInstance (mempty :: Vector Int),
                       RightGCDMonoidInstance (mempty :: Concat ByteString),
                       RightGCDMonoidInstance (mempty :: Concat Lazy.ByteString),
                       RightGCDMonoidInstance (mempty :: Concat (Dual Text))]
   where upcast (GCDMonoidInstance i) = RightGCDMonoidInstance i

gcdInstances =
    [ GCDMonoidInstance (mempty :: ())
    , GCDMonoidInstance (mempty :: Product Natural)
    , GCDMonoidInstance (mempty :: Dual (Product Natural))
    , GCDMonoidInstance (mempty :: IntSet)
    , GCDMonoidInstance (mempty :: Set String)
    ]

distributiveGCDMonoidInstances :: [DistributiveGCDMonoidInstance]
distributiveGCDMonoidInstances =
    [ DistributiveGCDMonoidInstance (mempty :: ())
    , DistributiveGCDMonoidInstance (mempty :: Product Natural)
    , DistributiveGCDMonoidInstance (mempty :: Sum Natural)
    , DistributiveGCDMonoidInstance (mempty :: IntSet)
    , DistributiveGCDMonoidInstance (mempty :: Set ())
    , DistributiveGCDMonoidInstance (mempty :: Set Bool)
    , DistributiveGCDMonoidInstance (mempty :: Set Word)
    , DistributiveGCDMonoidInstance (mempty :: Dual (Set ()))
    , DistributiveGCDMonoidInstance (mempty :: Dual (Set Bool))
    , DistributiveGCDMonoidInstance (mempty :: Dual (Set Word))
    ]

leftDistributiveGCDMonoidInstances :: [LeftDistributiveGCDMonoidInstance]
leftDistributiveGCDMonoidInstances =
    [ -- Instances for non-commutative monoids:
      LeftDistributiveGCDMonoidInstance (mempty :: [()])
    , LeftDistributiveGCDMonoidInstance (mempty :: [Bool])
    , LeftDistributiveGCDMonoidInstance (mempty :: [Word])
    , LeftDistributiveGCDMonoidInstance (mempty :: Seq ())
    , LeftDistributiveGCDMonoidInstance (mempty :: Seq Bool)
    , LeftDistributiveGCDMonoidInstance (mempty :: Seq Word)
    , LeftDistributiveGCDMonoidInstance (mempty :: Vector ())
    , LeftDistributiveGCDMonoidInstance (mempty :: Vector Bool)
    , LeftDistributiveGCDMonoidInstance (mempty :: Vector Word)
    , LeftDistributiveGCDMonoidInstance (mempty :: ByteString)
    , LeftDistributiveGCDMonoidInstance (mempty :: Lazy.ByteString)
    , LeftDistributiveGCDMonoidInstance (mempty :: Text)
    , LeftDistributiveGCDMonoidInstance (mempty :: Lazy.Text)
      -- Instances for commutative monoids:
    , LeftDistributiveGCDMonoidInstance (mempty :: ())
    , LeftDistributiveGCDMonoidInstance (mempty :: Product Natural)
    , LeftDistributiveGCDMonoidInstance (mempty :: Sum Natural)
    , LeftDistributiveGCDMonoidInstance (mempty :: IntSet)
    , LeftDistributiveGCDMonoidInstance (mempty :: Set ())
    , LeftDistributiveGCDMonoidInstance (mempty :: Set Bool)
    , LeftDistributiveGCDMonoidInstance (mempty :: Set Word)
      -- Instances for monoid transformers:
    , LeftDistributiveGCDMonoidInstance (mempty :: Dual [()])
    , LeftDistributiveGCDMonoidInstance (mempty :: Dual [Bool])
    , LeftDistributiveGCDMonoidInstance (mempty :: Dual [Word])
    ]

rightDistributiveGCDMonoidInstances :: [RightDistributiveGCDMonoidInstance]
rightDistributiveGCDMonoidInstances =
    [ -- Instances for non-commutative monoids:
      RightDistributiveGCDMonoidInstance (mempty :: [()])
    , RightDistributiveGCDMonoidInstance (mempty :: [Bool])
    , RightDistributiveGCDMonoidInstance (mempty :: [Word])
    , RightDistributiveGCDMonoidInstance (mempty :: Seq ())
    , RightDistributiveGCDMonoidInstance (mempty :: Seq Bool)
    , RightDistributiveGCDMonoidInstance (mempty :: Seq Word)
    , RightDistributiveGCDMonoidInstance (mempty :: Vector ())
    , RightDistributiveGCDMonoidInstance (mempty :: Vector Bool)
    , RightDistributiveGCDMonoidInstance (mempty :: Vector Word)
    , RightDistributiveGCDMonoidInstance (mempty :: ByteString)
    , RightDistributiveGCDMonoidInstance (mempty :: Lazy.ByteString)
    , RightDistributiveGCDMonoidInstance (mempty :: Text)
    , RightDistributiveGCDMonoidInstance (mempty :: Lazy.Text)
      -- Instances for commutative monoids:
    , RightDistributiveGCDMonoidInstance (mempty :: ())
    , RightDistributiveGCDMonoidInstance (mempty :: Product Natural)
    , RightDistributiveGCDMonoidInstance (mempty :: Sum Natural)
    , RightDistributiveGCDMonoidInstance (mempty :: IntSet)
    , RightDistributiveGCDMonoidInstance (mempty :: Set ())
    , RightDistributiveGCDMonoidInstance (mempty :: Set Bool)
    , RightDistributiveGCDMonoidInstance (mempty :: Set Word)
      -- Instances for monoid transformers:
    , RightDistributiveGCDMonoidInstance (mempty :: Dual [()])
    , RightDistributiveGCDMonoidInstance (mempty :: Dual [Bool])
    , RightDistributiveGCDMonoidInstance (mempty :: Dual [Word])
    ]

lcmInstances =
    [LCMMonoidInstance (mempty :: Product Natural),
     LCMMonoidInstance (mempty :: Sum Natural),
     LCMMonoidInstance (mempty :: Dual (Product Natural)),
     LCMMonoidInstance (mempty :: Dual (Sum Natural)),
     LCMMonoidInstance (mempty :: IntSet),
     LCMMonoidInstance (mempty :: (IntSet, IntSet)),
     LCMMonoidInstance (mempty :: (IntSet, IntSet, IntSet)),
     LCMMonoidInstance (mempty :: (IntSet, IntSet, IntSet, IntSet)),
     -- For sets, test with a variety of different universe sizes, from small
     -- to large:
     LCMMonoidInstance (mempty :: Set ()),
     LCMMonoidInstance (mempty :: Set Bool),
     LCMMonoidInstance (mempty :: Set Ordering),
     LCMMonoidInstance (mempty :: Set Word8)]

distributiveLCMInstances =
    [ DistributiveLCMMonoidInstance (mempty :: ())
    , DistributiveLCMMonoidInstance (mempty :: Product Natural)
    , DistributiveLCMMonoidInstance (mempty :: Sum Natural)
    , DistributiveLCMMonoidInstance (mempty :: IntSet)
    , DistributiveLCMMonoidInstance (mempty :: Set ())
    , DistributiveLCMMonoidInstance (mempty :: Set Bool)
    , DistributiveLCMMonoidInstance (mempty :: Set Word)
    , DistributiveLCMMonoidInstance (mempty :: Dual (Product Natural))
    , DistributiveLCMMonoidInstance (mempty :: Dual (Sum Natural))
    ]

main = defaultMain (testGroup "MonoidSubclasses" $ map expand tests)
  where expand (name, test) = testProperty name (List.foldr1 (.&&.) $ checkInstances test)

checkInstances :: Test -> [Property]
checkInstances (CommutativeTest checkType) = (map checkType commutativeInstances)
checkInstances (NullTest checkType) = (map checkType nullInstances)
checkInstances (PositiveTest checkType) = (map checkType positiveInstances)
checkInstances (FactorialTest checkType) = (map checkType factorialInstances)
checkInstances (StableFactorialTest checkType) = (map checkType stableFactorialInstances)
checkInstances (TextualTest checkType) = (map checkType textualInstances)
checkInstances (LeftReductiveTest checkType) = (map checkType leftReductiveInstances)
checkInstances (RightReductiveTest checkType) = (map checkType rightReductiveInstances)
checkInstances (ReductiveTest checkType) = (map checkType reductiveInstances)
checkInstances (LeftCancellativeTest checkType) = (map checkType leftCancellativeInstances) 
checkInstances (RightCancellativeTest checkType) = (map checkType rightCancellativeInstances) 
checkInstances (CancellativeTest checkType) = (map checkType cancellativeInstances) 
checkInstances (OverlappingGCDTest checkType) = (map checkType overlappingGCDMonoidInstances)
checkInstances (MonusTest checkType) = (map checkType monusInstances)
checkInstances (LeftGCDTest checkType) = (map checkType leftGCDInstances) 
checkInstances (RightGCDTest checkType) = (map checkType rightGCDInstances) 
checkInstances (GCDTest checkType) = (map checkType gcdInstances)  
checkInstances (DistributiveGCDTest checkType) = (map checkType distributiveGCDMonoidInstances)
checkInstances (LeftDistributiveGCDTest checkType) = (map checkType leftDistributiveGCDMonoidInstances)
checkInstances (RightDistributiveGCDTest checkType) = (map checkType rightDistributiveGCDMonoidInstances)
checkInstances (LCMTest checkType) = (map checkType lcmInstances)
checkInstances (DistributiveLCMTest checkType) = (map checkType distributiveLCMInstances)

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
         ("inits", FactorialTest checkInits),
         ("tails", FactorialTest checkTails),
         ("foldl", FactorialTest checkLeftFold),
         ("foldl'", FactorialTest checkLeftFold'),
         ("foldr", FactorialTest checkRightFold),
         ("length", FactorialTest checkLength),
         ("span", FactorialTest checkSpan),
         ("spanMaybe", FactorialTest checkSpanMaybe),
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
         ("Textual.toString", TextualTest checkToString),
         ("Textual.toText", TextualTest checkToText),
         ("Textual.mapAccumL", TextualTest checkTextualMapAccumL),
         ("Textual.mapAccumR", TextualTest checkTextualMapAccumR),
         ("Textual.takeWhile", TextualTest checkTextualTakeWhile),
         ("Textual.dropWhile", TextualTest checkTextualDropWhile),
         ("Textual.span", TextualTest checkTextualSpan),
         ("Textual.break", TextualTest checkTextualBreak),
         ("Textual.spanMaybe", TextualTest checkTextualSpanMaybe),
         ("Textual.split", TextualTest checkTextualSplit),
         ("Textual.find", TextualTest checkTextualFind),
         ("Textual.foldl_", TextualTest checkTextualFoldl_),
         ("Textual.foldr_", TextualTest checkTextualFoldr_),
         ("Textual.foldl_'", TextualTest checkTextualFoldl_'),
         ("Textual.span_", TextualTest checkTextualSpan_),
         ("Textual.break_", TextualTest checkTextualBreak_),
         ("Textual.spanMaybe_", TextualTest checkTextualSpanMaybe_),
         ("Textual.spanMaybe_'", TextualTest checkTextualSpanMaybe_'),
         ("Textual.takeWhile_", TextualTest checkTextualTakeWhile_),
         ("Textual.dropWhile_", TextualTest checkTextualDropWhile_),
         ("stripPrefix", LeftReductiveTest checkStripPrefix),
         ("stripPrefixOverlap 1", OverlappingGCDTest checkStripPrefixOverlap1),
         ("stripPrefixOverlap 2", OverlappingGCDTest checkStripPrefixOverlap2),
         ("stripPrefixOverlap 3", OverlappingGCDTest checkStripPrefixOverlap3),
         ("stripSuffixOverlap 1", OverlappingGCDTest checkStripSuffixOverlap1),
         ("stripSuffixOverlap 2", OverlappingGCDTest checkStripSuffixOverlap2),
         ("stripSuffixOverlap 3", OverlappingGCDTest checkStripSuffixOverlap3),
         ("overlap law 1", OverlappingGCDTest checkOverlapLaw1),
         ("overlap law 2", OverlappingGCDTest checkOverlapLaw2),
         ("overlap law 3", OverlappingGCDTest checkOverlapLaw3),
         ("overlap idempotence", OverlappingGCDTest checkOverlap_idempotence),
         ("overlap identity (left)", OverlappingGCDTest checkOverlap_identity_left),
         ("overlap identity (right)", OverlappingGCDTest checkOverlap_identity_right),
         ("isPrefixOf", LeftReductiveTest checkIsPrefixOf),
         ("stripSuffix", RightReductiveTest checkStripSuffix),
         ("isSuffixOf", RightReductiveTest checkIsSuffixOf),
         ("</>", ReductiveTest checkUnAppend),
         ("cancellative stripPrefix", LeftCancellativeTest checkStripPrefix'),
         ("cancellative stripSuffix", RightCancellativeTest checkStripSuffix'),
         ("cancellative </>", CancellativeTest checkUnAppend'),
         ("stripCommonPrefix 1", LeftGCDTest checkStripCommonPrefix1),
         ("stripCommonPrefix 2", LeftGCDTest checkStripCommonPrefix2),
         ("stripCommonPrefix 3", LeftGCDTest checkStripCommonPrefix3),
         ("stripCommonPrefix 4", LeftGCDTest checkStripCommonPrefix4),
         ("stripCommonSuffix 1", RightGCDTest checkStripCommonSuffix1),
         ("stripCommonSuffix 2", RightGCDTest checkStripCommonSuffix2),
         ("stripCommonSuffix 3", RightGCDTest checkStripCommonSuffix3),
         ("stripCommonSuffix 4", RightGCDTest checkStripCommonSuffix4),
         ("gcd", GCDTest checkGCD),
         ("gcd uniqueness", GCDTest checkGCD_uniqueness),
         ("gcd idempotence", GCDTest checkGCD_idempotence),
         ("gcd identity (left)", GCDTest checkGCD_identity_left),
         ("gcd identity (right)", GCDTest checkGCD_identity_right),
         ("gcd commutativity", GCDTest checkGCD_commutativity),
         ("gcd associativity", GCDTest checkGCD_associativity),
         ("gcd distributivity (left)", DistributiveGCDTest checkGCD_distributivity_left),
         ("gcd distributivity (right)", DistributiveGCDTest checkGCD_distributivity_right),
         ("commonPrefix idempotence", LeftGCDTest checkCommonPrefix_idempotence),
         ("commonPrefix identity (left)", LeftGCDTest checkCommonPrefix_identity_left),
         ("commonPrefix identity (right)", LeftGCDTest checkCommonPrefix_identity_right),
         ("commonPrefix commutativity", LeftGCDTest checkCommonPrefix_commutativity),
         ("commonPrefix associativity", LeftGCDTest checkCommonPrefix_associativity),
         ("commonPrefix distributivity", LeftDistributiveGCDTest checkCommonPrefix_distributivity),
         ("commonSuffix idempotence", RightGCDTest checkCommonSuffix_idempotence),
         ("commonSuffix identity (left)", RightGCDTest checkCommonSuffix_identity_left),
         ("commonSuffix identity (right)", RightGCDTest checkCommonSuffix_identity_right),
         ("commonSuffix commutativity", RightGCDTest checkCommonSuffix_commutativity),
         ("commonSuffix associativity", RightGCDTest checkCommonSuffix_associativity),
         ("commonSuffix distributivity", RightDistributiveGCDTest checkCommonSuffix_distributivity),
         ("lcm reductivity (left)", LCMTest checkLCM_reductivity_left),
         ("lcm reductivity (right)", LCMTest checkLCM_reductivity_right),
         ("lcm uniqueness", LCMTest checkLCM_uniqueness),
         ("lcm idempotence", LCMTest checkLCM_idempotence),
         ("lcm identity (left)", LCMTest checkLCM_identity_left),
         ("lcm identity (right)", LCMTest checkLCM_identity_right),
         ("lcm commutativity", LCMTest checkLCM_commutativity),
         ("lcm associativity", LCMTest checkLCM_associativity),
         ("lcm absorption (gcd-lcm)", LCMTest checkLCM_absorption_gcd_lcm),
         ("lcm absorption (lcm-gcd)", LCMTest checkLCM_absorption_lcm_gcd),
         ("lcm distributivity (left)", DistributiveLCMTest checkLCM_distributivity_left),
         ("lcm distributivity (right)", DistributiveLCMTest checkLCM_distributivity_right),
         ("lcm distributivity (gcd-lcm)", DistributiveLCMTest checkLCM_distributivity_gcd_lcm),
         ("lcm distributivity (lcm-gcd)", DistributiveLCMTest checkLCM_distributivity_lcm_gcd)
        ]

checkCommutative (CommutativeMonoidInstance (e :: a)) = forAll (arbitrary :: Gen (a, a)) (\(a, b)-> a <> b == b <> a)

checkNull (NullMonoidInstance (e :: a)) = null e .&&. forAll (arbitrary :: Gen a) (\a-> null a == (a == mempty))

checkPositive (PositiveMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen (a, a)) (\(a, b)-> null a && null b || not (null (mappend a b)))

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

checkInits (FactorialMonoidInstance (_ :: a)) =
   mapSize (`div` 5) $ forAll (arbitrary :: Gen a) (\a-> inits a == List.map mconcat (List.inits $ factors a))

checkTails (FactorialMonoidInstance (_ :: a)) =
   mapSize (`div` 5) $ forAll (arbitrary :: Gen a) (\a-> tails a == List.map mconcat (List.tails $ factors a))

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

checkSpanMaybe (FactorialMonoidInstance (_ :: a)) = property $ \(f, s)-> forAll (arbitrary :: Gen a) (check f (s :: Bool))
   where check f s0 a = a == prefix <> suffix
                        && foldMaybe prefix == Just s'
                        && (null suffix || f s' (primePrefix suffix) == Nothing)
            where (prefix, suffix, s') = spanMaybe s0 f a
                  foldMaybe = foldl g (Just s0)
                  g s m = s >>= flip f m

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
   where check1 a = Textual.map wrapSucc a == Textual.concatMap (Textual.singleton . wrapSucc) a
                    && Textual.map id a == a
         check2 s = Textual.map wrapSucc (fromString s :: a) == fromString (List.map wrapSucc s)
         wrapSucc c
            | c == maxBound = minBound
            | otherwise = succ c

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
         check2 s = Textual.foldr undefined (:) [] (fromString s :: a) == s

checkTextualFoldl' (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl' (\l a-> Left a : l) (\l c-> Right c : l) [] a == List.reverse (textualFactors a)
                    && Textual.foldl' (<>) (\a-> (a <>) . Textual.singleton) mempty a == a
         check2 s = Textual.foldl' undefined (flip (:)) [] s == List.foldl' (flip (:)) [] s

checkTextualFoldl_ (TextualMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl_ (\l c-> c : l) [] a == List.reverse (rights $ textualFactors a)
         check2 s = Textual.foldl_ (flip (:)) [] s == List.foldl (flip (:)) [] s

checkTextualFoldr_ (TextualMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldr_ (\c l-> c : l) [] a == rights (textualFactors a)
         check2 s = Textual.foldr_ (:) [] (fromString s :: a) == s

checkTextualFoldl_' (TextualMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.foldl_' (\l c-> c : l) [] a == List.reverse (rights $ textualFactors a)
         check2 s = Textual.foldl_' (flip (:)) [] s == List.foldl (flip (:)) [] s

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

checkTextualScanr1 (TextualMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = Textual.scanr1 const a == a
         check2 s = Textual.scanr1 f (fromString s :: a) == fromString (List.scanr1 f s)
         f c1 c2 = min c1 c2

checkToString (TextualMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = forAll arbitrary $ \f-> Textual.toString f a == Textual.foldr (\t s-> f t ++ s) (:) "" a
         check2 s = Textual.toString undefined (fromString s :: a) == s

checkToText (TextualMonoidInstance (_ :: a)) =
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen Text) check2
   where check1 a = forAll arbitrary $ \f-> Textual.toText f a == Textual.foldr (\t s-> f t <> s) Text.cons Text.empty a
         check2 s = Textual.toText undefined (Textual.fromText s :: a) == s

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
         check2 s = Textual.toString undefined (Textual.dropWhile undefined isLetter (fromString s :: a))
                    == List.dropWhile isLetter s

checkTextualSpan (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = Textual.span pt pc a == (Textual.takeWhile pt pc a, Textual.dropWhile pt pc a)
            where pt = (== primePrefix a)
         pc = isLetter

checkTextualBreak (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen a) check
   where check a = Textual.break pt pc a == Textual.span (not . pt) (not . pc) a
            where pt = (/= primePrefix a)
         pc = isLetter

checkTextualSpanMaybe (TextualMonoidInstance (_ :: a)) =
   property $ \(ft, fc, s)-> forAll (arbitrary :: Gen a) (check ft fc (s :: Bool))
   where check ft fc s0 a = a == prefix <> suffix
                            && foldMaybe prefix == Just s'
                            && (null suffix
                                || maybe (ft s' (primePrefix suffix)) (fc s') (Textual.characterPrefix suffix) == Nothing)
            where (prefix, suffix, s') = Textual.spanMaybe s0 ft fc a
                  foldMaybe = Textual.foldl gt gc (Just s0)
                  gt s m = s >>= flip ft m
                  gc s c = s >>= flip fc c

checkTextualSpan_ (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, Bool)) check
   where check (a, bt) = Textual.span_ bt isLetter a == (Textual.takeWhile_ bt isLetter a, Textual.dropWhile_ bt isLetter a)

checkTextualBreak_ (TextualMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, Bool)) check
   where check (a, bt) = Textual.break_ bt isLetter a == Textual.span_ (not bt) (not . isLetter) a

checkTextualSpanMaybe_ (TextualMonoidInstance (_ :: a)) =
   property $ \(fc, s)-> forAll (arbitrary :: Gen a) (check fc (s :: Bool))
   where check fc s0 a = a == prefix <> suffix
                         && foldMaybe prefix == Just s'
                         && (null suffix || (Textual.characterPrefix suffix >>= fc s') == Nothing)
            where (prefix, suffix, s') = Textual.spanMaybe_ s0 fc a
                  foldMaybe = Textual.foldl_ gc (Just s0)
                  gc s c = s >>= flip fc c

checkTextualSpanMaybe_' (TextualMonoidInstance (_ :: a)) =
   property $ \(fc, s)-> forAll (arbitrary :: Gen a) (check fc (s :: Bool))
   where check fc s0 a = a == prefix <> suffix
                         && foldMaybe prefix == Just s'
                         && (null suffix || (Textual.characterPrefix suffix >>= fc s') == Nothing)
            where (prefix, suffix, s') = Textual.spanMaybe_' s0 fc a
                  foldMaybe = Textual.foldl_' gc (Just s0)
                  gc s c = s >>= flip fc c

checkTextualTakeWhile_ (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = textualFactors (Textual.takeWhile_ True isLetter a)
                    == List.takeWhile (either (const True) isLetter) (textualFactors a)
                    && Textual.takeWhile_ True (const True) a == a
         check2 s = Textual.takeWhile_ undefined isLetter (fromString s :: a) == fromString (List.takeWhile isLetter s)

checkTextualDropWhile_ (TextualMonoidInstance (_ :: a)) = 
   forAll (arbitrary :: Gen a) check1 .&&. forAll (arbitrary :: Gen String) check2
   where check1 a = textualFactors (Textual.dropWhile_ True isLetter a)
                    == List.dropWhile (either (const True) isLetter) (textualFactors a)
                    && Textual.dropWhile_ False (const False) a == a
         check2 s = Textual.toString undefined (Textual.dropWhile_ undefined isLetter (fromString s :: a))
                    == List.dropWhile isLetter s

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

checkOverlapLaw1 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripOverlap a b == (stripSuffixOverlap b a, overlap a b, stripPrefixOverlap a b)

checkOverlapLaw2 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripSuffixOverlap b a <> overlap a b == a

checkOverlapLaw3 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = overlap a b <> stripPrefixOverlap a b == b

checkOverlap_idempotence (OverlappingGCDMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen a) $ \a -> overlap a a === a

checkOverlap_identity_left (OverlappingGCDMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen a) $ \a -> overlap mempty a === mempty

checkOverlap_identity_right (OverlappingGCDMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen a) $ \a -> overlap a mempty === mempty

checkStripPrefixOverlap1 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = o `isSuffixOf` b && b `isSuffixOf` (a <> o)
            where o = stripPrefixOverlap a b

checkStripPrefixOverlap2 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (ap, o, bs) = b `isSuffixOf` (a <> b') && b' `isSuffixOf` bs
            where a = ap <> o
                  b = o <> bs
                  b' = stripPrefixOverlap a b

checkStripPrefixOverlap3 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = all (\(_, s)-> null s || not (b `isSuffixOf` (a <> s))) (splitPrimePrefix b')
            where b' = stripPrefixOverlap a b

checkStripSuffixOverlap1 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = o `isPrefixOf` a && a `isPrefixOf` (o <> b)
            where o = stripSuffixOverlap b a

checkStripSuffixOverlap2 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (ap, o, bs) = a `isPrefixOf` (a' <> b) && a' `isPrefixOf` ap
            where a = ap <> o
                  b = o <> bs
                  a' = stripSuffixOverlap b a

checkStripSuffixOverlap3 (OverlappingGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = all (\(p, _)-> null p || not (a `isPrefixOf` (p <> b))) (splitPrimeSuffix a')
            where a' = stripSuffixOverlap b a

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

checkStripCommonPrefix3 (LeftGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (p, as, bs) = p `isPrefixOf` commonPrefix a b
            where a = p <> as
                  b = p <> bs

checkStripCommonPrefix4 (LeftGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (p, a, b) = not (c /= c' && c' `isPrefixOf` a && c' `isPrefixOf` b)
            where c = commonPrefix a b
                  c' = p <> c

checkStripCommonSuffix1 (RightGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = stripCommonSuffix a b == (a', b', s)
            where s = commonSuffix a b
                  Just a' = stripSuffix s a
                  Just b' = stripSuffix s b

checkStripCommonSuffix2 (RightGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = s == commonSuffix a b && a' <> s == a && b' <> s == b
            where (a', b', s) = stripCommonSuffix a b

checkStripCommonSuffix3 (RightGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (ap, bp, s) = s `isSuffixOf` commonSuffix a b
            where a = ap <> s
                  b = bp <> s

checkStripCommonSuffix4 (RightGCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a, a)) check
   where check (a, b, s) = not (c /= c' && c' `isSuffixOf` a && c' `isSuffixOf` b)
            where c = commonSuffix a b
                  c' = c <> s

checkGCD (GCDMonoidInstance (_ :: a)) = forAll (arbitrary :: Gen (a, a)) check
   where check (a, b) = d == commonPrefix a b
                        && d == commonSuffix a b
                        && isJust (a </> d)
                        && isJust (b </> d)
            where d = gcd a b

checkGCD_uniqueness
    (GCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \(a, b, c) ->
            all isJust [a </> c, b </> c, c </> gcd a b] === (gcd a b == c)

checkGCD_idempotence
    (GCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> gcd a a === a

checkGCD_identity_left
    (GCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> gcd mempty a === mempty

checkGCD_identity_right
    (GCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> gcd a mempty === mempty

checkGCD_commutativity
    (GCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a)) $
        \a b -> gcd a b === gcd b a

checkGCD_associativity
    (GCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \a b c -> gcd a (gcd b c) === gcd (gcd a b) c

checkGCD_distributivity_left
    (DistributiveGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \(a, b, c) -> gcd (a <> b) (a <> c) == a <> gcd b c

checkGCD_distributivity_right
    (DistributiveGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \(a, b, c) -> gcd (a <> c) (b <> c) == gcd a b <> c

checkCommonPrefix_idempotence
    (LeftGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> commonPrefix a a === a

checkCommonPrefix_identity_left
    (LeftGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> commonPrefix mempty a === mempty

checkCommonPrefix_identity_right
    (LeftGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> commonPrefix a mempty === mempty

checkCommonPrefix_commutativity
    (LeftGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a)) $
        \a b -> commonPrefix a b === commonPrefix b a

checkCommonPrefix_associativity
    (LeftGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \a b c ->
            (commonPrefix a (commonPrefix b c)) ===
            (commonPrefix (commonPrefix a b) c)

checkCommonPrefix_distributivity
    (LeftDistributiveGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \(a, b, c) -> commonPrefix (a <> b) (a <> c) == a <> commonPrefix b c

checkCommonSuffix_idempotence
    (RightGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> commonSuffix a a === a

checkCommonSuffix_identity_left
    (RightGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> commonSuffix mempty a === mempty

checkCommonSuffix_identity_right
    (RightGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen a) $
        \a -> commonSuffix a mempty === mempty

checkCommonSuffix_commutativity
    (RightGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a)) $
        \a b -> commonSuffix a b === commonSuffix b a

checkCommonSuffix_associativity
    (RightGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \a b c ->
            (commonSuffix a (commonSuffix b c)) ===
            (commonSuffix (commonSuffix a b) c)

checkCommonSuffix_distributivity
    (RightDistributiveGCDMonoidInstance (_ :: a)) =
        forAll (arbitrary :: Gen (a, a, a)) $
        \(a, b, c) -> commonSuffix (a <> c) (b <> c) == commonSuffix a b <> c

checkLCM_reductivity_left (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a)) check
  where
    check a b = isJust (lcm a b </> a)

checkLCM_reductivity_right (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a)) check
  where
    check a b = isJust (lcm a b </> b)

checkLCM_uniqueness (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a, a)) check
  where
    check a b c =
        all isJust [c </> a, c </> b, lcm a b </> c] === (lcm a b == c)

checkLCM_idempotence (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen a) check
  where
    check a = lcm a a === a

checkLCM_identity_left (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen a) check
  where
    check a = lcm mempty a === a

checkLCM_identity_right (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen a) check
  where
    check a = lcm a mempty === a

checkLCM_commutativity (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a)) check
  where
    check a b = lcm a b === lcm b a

checkLCM_associativity (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a, a)) check
  where
    check a b c = lcm (lcm a b) c === lcm a (lcm b c)

checkLCM_absorption_gcd_lcm (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a)) check
  where
    check a b = lcm a (gcd a b) === a

checkLCM_absorption_lcm_gcd (LCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a)) check
  where
    check a b = gcd a (lcm a b) === a

checkLCM_distributivity_left (DistributiveLCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a, a)) check
  where
    check a b c = lcm (a <> b) (a <> c) === a <> lcm b c

checkLCM_distributivity_right (DistributiveLCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a, a)) check
  where
    check a b c = lcm (a <> c) (b <> c) === lcm a b <> c

checkLCM_distributivity_gcd_lcm (DistributiveLCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a, a)) check
  where
    check a b c = lcm a (gcd b c) === gcd (lcm a b) (lcm a c)

checkLCM_distributivity_lcm_gcd (DistributiveLCMMonoidInstance (_ :: a)) =
    forAll (arbitrary :: Gen (a, a, a)) check
  where
    check a b c = gcd a (lcm b c) === lcm (gcd a b) (gcd a c)

textualFactors :: TextualMonoid t => t -> [Either t Char]
textualFactors = map characterize . factors
   where characterize prime = maybe (Left prime) Right (Textual.characterPrefix prime)

newtype TestString = TestString String deriving (Eq, Show, Arbitrary, CoArbitrary, 
                                                 Semigroup, LeftReductive, LeftCancellative, StableFactorial,
                                                 Monoid, LeftGCDMonoid,
                                                 MonoidNull, PositiveMonoid, IsString)

newtype TestOffsetPositionedString = TestOffsetPositionedString (OffsetPositioned String)
                                     deriving (Show, Arbitrary, CoArbitrary,
                                               Semigroup, LeftReductive,
                                               Monoid, LeftGCDMonoid,
                                               MonoidNull, PositiveMonoid, IsString)

newtype TestLinePositionedString = TestLinePositionedString (LinePositioned String)
                               deriving (Show, Arbitrary, CoArbitrary,
                                         Semigroup, LeftReductive,
                                         Monoid, LeftGCDMonoid,
                                         MonoidNull, PositiveMonoid, IsString)

instance Factorial TestString where
   factors (TestString s) = TestString <$> factors s

instance FactorialMonoid TestString where
   splitPrimePrefix (TestString []) = Nothing
   splitPrimePrefix (TestString (x:xs)) = Just (TestString [x], TestString xs)

instance TextualMonoid TestString where
   splitCharacterPrefix (TestString []) = Nothing
   splitCharacterPrefix (TestString (x:xs)) = Just (x, TestString xs)

instance Eq TestOffsetPositionedString where
   TestOffsetPositionedString a == TestOffsetPositionedString b =
      a == b && Positioned.position a == Positioned.position b

instance Factorial TestOffsetPositionedString where
   factors (TestOffsetPositionedString s) = TestOffsetPositionedString <$> factors s

instance FactorialMonoid TestOffsetPositionedString where
   splitPrimePrefix (TestOffsetPositionedString s) = rewrap <$> splitPrimePrefix s
      where rewrap (x, xs) = (TestOffsetPositionedString x, TestOffsetPositionedString xs)

instance TextualMonoid TestOffsetPositionedString where
   splitCharacterPrefix (TestOffsetPositionedString x) = (TestOffsetPositionedString <$>) <$> Textual.splitCharacterPrefix x

instance Eq TestLinePositionedString where
   TestLinePositionedString a == TestLinePositionedString b =
      a == b && Positioned.line a == Positioned.line b && Positioned.column a == Positioned.column b
      && Positioned.position a == Positioned.position b

instance Factorial TestLinePositionedString where
   factors (TestLinePositionedString s) = TestLinePositionedString <$> factors s

instance FactorialMonoid TestLinePositionedString where
   splitPrimePrefix (TestLinePositionedString s) = rewrap <$> splitPrimePrefix s
      where rewrap (x, xs) = (TestLinePositionedString x, TestLinePositionedString xs)

instance TextualMonoid TestLinePositionedString where
   splitCharacterPrefix (TestLinePositionedString x) = (TestLinePositionedString <$>) <$> Textual.splitCharacterPrefix x

instance Arbitrary ByteStringUTF8 where
   arbitrary = fmap ByteStringUTF8 arbitrary

instance (Arbitrary a, MonoidNull a, PositiveMonoid a) => Arbitrary (Concat a) where
   arbitrary = fmap (foldMap pure) (arbitrary :: Gen [a])

instance (Arbitrary a, FactorialMonoid a) => Arbitrary (Measured a) where
   arbitrary = fmap Measured.measure arbitrary

instance (Arbitrary a, Monoid a) => Arbitrary (PrefixMemory.Shadowed a) where
   arbitrary = fmap PrefixMemory.shadowed arbitrary

instance (Arbitrary a, FactorialMonoid a) => Arbitrary (OffsetPositioned a) where
   arbitrary = fmap pure arbitrary

instance (Arbitrary a, TextualMonoid a) => Arbitrary (LinePositioned a) where
   arbitrary = fmap pure arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Stateful a b) where
   arbitrary = Stateful.Stateful <$> liftA2 (,) arbitrary arbitrary

instance CoArbitrary ByteStringUTF8 where
   coarbitrary (ByteStringUTF8 bs) = coarbitrary bs

instance CoArbitrary a => CoArbitrary (Concat a) where
   coarbitrary = coarbitrary . toList

instance CoArbitrary a => CoArbitrary (Measured a) where
   coarbitrary = coarbitrary . Measured.extract

instance CoArbitrary a => CoArbitrary (PrefixMemory.Shadowed a) where
   coarbitrary = coarbitrary . PrefixMemory.content

instance CoArbitrary a => CoArbitrary (OffsetPositioned a) where
   coarbitrary = coarbitrary . Positioned.extract

instance CoArbitrary a => CoArbitrary (LinePositioned a) where
   coarbitrary = coarbitrary . Positioned.extract

instance CoArbitrary b => CoArbitrary (Stateful a b) where
   coarbitrary = coarbitrary . Stateful.extract

instance (PositiveMonoid a, MonoidNull b) => PositiveMonoid (a, b)

#if MIN_VERSION_containers(0,5,2)
#else
instance Applicative Seq where
   pure = Sequence.singleton
   fs <*> xs = Foldable.foldl' add mempty fs
      where add ys f = ys <> fmap f xs

#endif
