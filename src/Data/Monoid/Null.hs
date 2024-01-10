{-
    Copyright 2013-2015 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the MonoidNull class and some of its instances.
--

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}

module Data.Monoid.Null (
   MonoidNull(..), PositiveMonoid
   )
where

import Data.Monoid -- (Monoid, First(..), Last(..), Dual(..), Sum(..), Product(..), All(getAll), Any(getAny))
import qualified Data.List as List
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Numeric.Natural (Natural)

import Prelude hiding (null)

-- | Extension of 'Monoid' that allows testing a value for equality with 'mempty'. The following law must hold:
--
-- prop> null x == (x == mempty)
--
-- Furthermore, the performance of this method should be constant, /i.e./, independent of the length of its argument.
class Monoid m => MonoidNull m where
   null :: m -> Bool

-- | Subclass of 'Monoid' for types whose values have no inverse, with the exception of 'Data.Monoid.mempty'. More
-- formally, the class instances must satisfy the following law:
--
-- prop> null (x <> y) == (null x && null y)
class MonoidNull m => PositiveMonoid m

instance MonoidNull () where
   null () = True

instance MonoidNull Ordering where
   null = (== EQ)

instance MonoidNull All where
   null = getAll

instance MonoidNull Any where
   null = not . getAny

instance MonoidNull (First a) where
   null (First Nothing) = True
   null _ = False

instance MonoidNull (Last a) where
   null (Last Nothing) = True
   null _ = False

instance MonoidNull a => MonoidNull (Dual a) where
   null (Dual a) = null a

instance (Num a, Eq a) => MonoidNull (Sum a) where
   null (Sum a) = a == 0

instance (Num a, Eq a) => MonoidNull (Product a) where
   null (Product a) = a == 1

instance Monoid a => MonoidNull (Maybe a) where
   null Nothing = True
   null _ = False

instance (MonoidNull a, MonoidNull b) => MonoidNull (a, b) where
   null (a, b) = null a && null b

instance (MonoidNull a, MonoidNull b, MonoidNull c) => MonoidNull (a, b, c) where
   null (a, b, c) = null a && null b && null c

instance (MonoidNull a, MonoidNull b, MonoidNull c, MonoidNull d) => MonoidNull (a, b, c, d) where
   null (a, b, c, d) = null a && null b && null c && null d

instance MonoidNull [x] where
   null = List.null

instance MonoidNull ByteString.ByteString where
   null = ByteString.null
   {-# INLINE null #-}

instance MonoidNull LazyByteString.ByteString where
   null = LazyByteString.null
   {-# INLINE null #-}

instance MonoidNull Text.Text where
   null = Text.null
   {-# INLINE null #-}

instance MonoidNull LazyText.Text where
   null = LazyText.null
   {-# INLINE null #-}

instance Ord k => MonoidNull (Map.Map k v) where
   null = Map.null

instance MonoidNull (IntMap.IntMap v) where
   null = IntMap.null

instance MonoidNull IntSet.IntSet where
   null = IntSet.null

instance MonoidNull (Sequence.Seq a) where
   null = Sequence.null

instance Ord a => MonoidNull (Set.Set a) where
   null = Set.null

instance MonoidNull (Vector.Vector a) where
   null = Vector.null

instance PositiveMonoid ()
instance PositiveMonoid Ordering
instance PositiveMonoid All
instance PositiveMonoid Any
instance PositiveMonoid ByteString.ByteString
instance PositiveMonoid LazyByteString.ByteString
instance PositiveMonoid Text.Text
instance PositiveMonoid LazyText.Text
instance PositiveMonoid (Product Natural)
instance PositiveMonoid (Sum Natural)
instance Monoid a => PositiveMonoid (Maybe a)
instance PositiveMonoid (First a)
instance PositiveMonoid (Last a)
instance PositiveMonoid a => PositiveMonoid (Dual a)
instance PositiveMonoid [x]
instance Ord k => PositiveMonoid (Map.Map k v)
instance PositiveMonoid (IntMap.IntMap v)
instance PositiveMonoid IntSet.IntSet
instance PositiveMonoid (Sequence.Seq a)
instance Ord a => PositiveMonoid (Set.Set a)
instance PositiveMonoid (Vector.Vector a)

-- The possible tuple instances would be overlapping, so we leave the choice to the user.
--
-- instance (PositiveMonoid a, Monoid b) => PositiveMonoid (a, b)
-- instance (Monoid a, PositiveMonoid b) => PositiveMonoid (a, b)
