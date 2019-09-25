{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'OverlappingGCDMonoid' => 'Monus' subclasses of the 'Monoid' class.

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}

module Data.Monoid.Monus (
   Monus(..), OverlappingGCDMonoid(..)
   )
where
   
import Data.Monoid -- (Monoid, Dual(..), Sum(..), Product(..))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Data.Sequence (ViewL((:<)), ViewR((:>)), (<|), (|>))
import qualified Data.Vector as Vector
import Numeric.Natural (Natural)

import Data.Semigroup.Cancellative
import Data.Monoid.Null (MonoidNull(null))

import Prelude hiding (null)

-- | Class of Abelian monoids with monus. The monus operation '<\>' is a synonym for both 'stripPrefixOverlap' and
-- 'stripSuffixOverlap', which must be equivalent as '<>' is both associative and commutative:
--
-- > (<\>) = flip stripPrefixOverlap
-- > (<\>) = flip stripSuffixOverlap
class (Commutative m, Monoid m, OverlappingGCDMonoid m) => Monus m where
   (<\>) :: m -> m -> m

infix 5 <\>

-- | Class of monoids for which the greatest overlap can be found between any two values, such that
--
-- > a == a' <> overlap a b
-- > b == overlap a b <> b'
--
-- The methods must satisfy the following laws:
--
-- > stripOverlap a b == (stripSuffixOverlap b a, overlap a b, stripPrefixOverlap a b)
-- > stripSuffixOverlap b a <> overlap a b == a
-- > overlap a b <> stripPrefixOverlap a b == b
--
-- The result of @overlap a b@ must be the largest prefix of @b@ and suffix of @a@, in the sense that it is contained
-- in any other value @x@ that satifies the property @(x `isPrefixOf` b) && (x `isSuffixOf` a)@:
--
-- > (x `isPrefixOf` overlap a b) && (x `isSuffixOf` overlap a b)
--
-- and it must be unique so it's not contained in any other value @y@ that satisfies the same property @(y
-- `isPrefixOf` b) && (y `isSuffixOf` a)@:
--
-- > not ((y `isPrefixOf` overlap a b) && (y `isSuffixOf` overlap a b) && y /= overlap a b)
class (Monoid m, LeftReductive m, RightReductive m) => OverlappingGCDMonoid m where
   stripPrefixOverlap :: m -> m -> m
   stripSuffixOverlap :: m -> m -> m
   overlap :: m -> m -> m
   stripOverlap :: m -> m -> (m, m, m)

-- Unit instances

instance Monus () where
   () <\> () = ()

instance OverlappingGCDMonoid () where
   overlap () () = ()
   stripOverlap () () = ((), (), ())
   stripPrefixOverlap () () = ()
   stripSuffixOverlap () () = ()

-- Dual instances

instance Monus a => Monus (Dual a) where
   Dual a <\> Dual b = Dual (a <\> b)

instance OverlappingGCDMonoid a => OverlappingGCDMonoid (Dual a) where
   overlap (Dual a) (Dual b) = Dual (overlap b a)
   stripOverlap (Dual a) (Dual b) = (Dual s, Dual o, Dual p)
      where (p, o, s) = stripOverlap b a
   stripPrefixOverlap (Dual a) (Dual b) = Dual (stripSuffixOverlap a b)
   stripSuffixOverlap (Dual a) (Dual b) = Dual (stripPrefixOverlap a b)

-- Sum instances

instance Monus (Sum Natural) where
   Sum a <\> Sum b
      | a > b = Sum (a - b)
      | otherwise = Sum 0

instance OverlappingGCDMonoid (Sum Natural) where
   overlap (Sum a) (Sum b) = Sum (min a b)
   stripOverlap (Sum a) (Sum b) = (Sum $ a - c, Sum c, Sum $ b - c)
      where c = min a b
   stripPrefixOverlap = flip (<\>)
   stripSuffixOverlap = flip (<\>)

-- Product instances

instance Monus (Product Natural) where
   Product 0 <\> Product 0 = Product 1
   Product a <\> Product b = Product (a `div` Prelude.gcd a b)

instance OverlappingGCDMonoid (Product Natural) where
   overlap (Product a) (Product b) = Product (gcd a b)
   stripOverlap (Product 0) (Product 0) = (Product 1, Product 0, Product 1)
   stripOverlap (Product a) (Product b) = (Product $ div a c, Product c, Product $ div b c)
      where c = gcd a b
   stripPrefixOverlap = flip (<\>)
   stripSuffixOverlap = flip (<\>)

-- Pair instances

instance (Monus a, Monus b) => Monus (a, b) where
   (a1, b1) <\> (a2, b2) = (a1 <\> a2, b1 <\> b2)

instance (OverlappingGCDMonoid a, OverlappingGCDMonoid b) => OverlappingGCDMonoid (a, b) where
   overlap (a1, b1) (a2, b2) = (overlap a1 a2, overlap b1 b2)
   stripOverlap (a1, b1) (a2, b2) = ((ap, bp), (ao, bo), (as, bs))
      where (ap, ao, as) = stripOverlap a1 a2
            (bp, bo, bs) = stripOverlap b1 b2
   stripPrefixOverlap (a1, b1) (a2, b2) = (stripPrefixOverlap a1 a2, stripPrefixOverlap b1 b2)
   stripSuffixOverlap (a1, b1) (a2, b2) = (stripSuffixOverlap a1 a2, stripSuffixOverlap b1 b2)

-- Triple instances

instance (Monus a, Monus b, Monus c) => Monus (a, b, c) where
   (a1, b1, c1) <\> (a2, b2, c2) = (a1 <\> a2, b1 <\> b2, c1 <\> c2)

instance (OverlappingGCDMonoid a, OverlappingGCDMonoid b, OverlappingGCDMonoid c) =>
         OverlappingGCDMonoid (a, b, c) where
   overlap (a1, b1, c1) (a2, b2, c2) = (overlap a1 a2, overlap b1 b2, overlap c1 c2)
   stripOverlap (a1, b1, c1) (a2, b2, c2) = ((ap, bp, cp), (ao, bo, co), (as, bs, cs))
      where (ap, ao, as) = stripOverlap a1 a2
            (bp, bo, bs) = stripOverlap b1 b2
            (cp, co, cs) = stripOverlap c1 c2
   stripPrefixOverlap (a1, b1, c1) (a2, b2, c2) = (stripPrefixOverlap a1 a2, stripPrefixOverlap b1 b2, stripPrefixOverlap c1 c2)
   stripSuffixOverlap (a1, b1, c1) (a2, b2, c2) = (stripSuffixOverlap a1 a2, stripSuffixOverlap b1 b2, stripSuffixOverlap c1 c2)

-- Quadruple instances

instance (Monus a, Monus b, Monus c, Monus d) => Monus (a, b, c, d) where
   (a1, b1, c1, d1) <\> (a2, b2, c2, d2) = (a1 <\> a2, b1 <\> b2, c1 <\> c2, d1 <\> d2)

instance (OverlappingGCDMonoid a, OverlappingGCDMonoid b, OverlappingGCDMonoid c, OverlappingGCDMonoid d) =>
         OverlappingGCDMonoid (a, b, c, d) where
   overlap (a1, b1, c1, d1) (a2, b2, c2, d2) = (overlap a1 a2, overlap b1 b2, overlap c1 c2, overlap d1 d2)
   stripOverlap (a1, b1, c1, d1) (a2, b2, c2, d2) = ((ap, bp, cp, dp), (ao, bo, co, dm), (as, bs, cs, ds))
      where (ap, ao, as) = stripOverlap a1 a2
            (bp, bo, bs) = stripOverlap b1 b2
            (cp, co, cs) = stripOverlap c1 c2
            (dp, dm, ds) = stripOverlap d1 d2
   stripPrefixOverlap (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (stripPrefixOverlap a1 a2, stripPrefixOverlap b1 b2, stripPrefixOverlap c1 c2, stripPrefixOverlap d1 d2)
   stripSuffixOverlap (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (stripSuffixOverlap a1 a2, stripSuffixOverlap b1 b2, stripSuffixOverlap c1 c2, stripSuffixOverlap d1 d2)

-- Maybe instances

instance (Monus a, MonoidNull a) => Monus (Maybe a) where
   Just a <\> Just b = Just (a <\> b)
   Nothing <\> _ = Nothing
   x <\> Nothing = x

instance (OverlappingGCDMonoid a, MonoidNull a) => OverlappingGCDMonoid (Maybe a) where
   overlap (Just a) (Just b) = Just (overlap a b)
   overlap _ _ = Nothing
   stripOverlap (Just a) (Just b) = (Just a', Just o, Just b')
      where (a', o, b') = stripOverlap a b
   stripOverlap a b = (a, Nothing, b)
   stripPrefixOverlap (Just a) (Just b)
      | null b' = Nothing
      | otherwise = Just b'
      where b' = stripPrefixOverlap a b
   stripPrefixOverlap Nothing x = x
   stripPrefixOverlap _ Nothing = Nothing
   stripSuffixOverlap (Just a) (Just b)
      | null b' = Nothing
      | otherwise = Just b'
      where b' = stripSuffixOverlap a b
   stripSuffixOverlap Nothing x = x
   stripSuffixOverlap _ Nothing = Nothing

-- Set instances

instance Ord a => Monus (Set.Set a) where
   (<\>) = (Set.\\)

instance Ord a => OverlappingGCDMonoid (Set.Set a) where
   overlap = Set.intersection
   stripOverlap a b = (Set.difference a b, Set.intersection a b, Set.difference b a)
   stripPrefixOverlap a b = b <\> a
   stripSuffixOverlap a b = b <\> a

-- IntSet instances

instance Monus IntSet.IntSet where
   (<\>) = (IntSet.\\)

instance OverlappingGCDMonoid IntSet.IntSet where
   overlap = IntSet.intersection
   stripOverlap a b = (IntSet.difference a b, IntSet.intersection a b, IntSet.difference b a)
   stripPrefixOverlap a b = b <\> a
   stripSuffixOverlap a b = b <\> a

-- Map instances

instance (Ord k, Eq v) => OverlappingGCDMonoid (Map.Map k v) where
    overlap = Map.intersection
    stripOverlap a b = (stripPrefixOverlap b a, overlap a b, stripSuffixOverlap a b)
    stripPrefixOverlap = flip Map.difference
    stripSuffixOverlap a b = Map.differenceWith (\x y-> if x == y then Nothing else Just x) b a

-- IntMap instances

instance Eq a => OverlappingGCDMonoid (IntMap.IntMap a) where
    overlap = IntMap.intersection
    stripOverlap a b = (stripPrefixOverlap b a, overlap a b, stripSuffixOverlap a b)
    stripPrefixOverlap = flip IntMap.difference
    stripSuffixOverlap a b = IntMap.differenceWith (\x y-> if x == y then Nothing else Just x) b a

-- List instances

-- Seq instances

-- Vector instances

-- ByteString instances

-- Lazy ByteString instances

-- Text instances

-- Lazy Text instances
