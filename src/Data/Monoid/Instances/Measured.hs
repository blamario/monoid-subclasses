{- 
    Copyright 2013-2022 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Measured'.
-- 

{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

module Data.Monoid.Instances.Measured (
   Measured, measure, extract
   )
where

import Data.Functor -- ((<$>))
import Data.Data (Data, Typeable)
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup.Cancellative (LeftReductive(..), RightReductive(..))
import Data.Semigroup.Factorial (Factorial(..), StableFactorial)
import Data.Monoid.GCD (LeftGCDMonoid(..), RightGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual

import Prelude hiding (all, any, break, filter, foldl, foldl1, foldr, foldr1, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt)

-- | @'Measured' a@ is a wrapper around the 'FactorialMonoid' @a@ that memoizes the monoid's 'length' so it becomes a
-- constant-time operation. The parameter is restricted to the 'StableFactorial' class, which guarantees that
-- @'length' (a <> b) == 'length' a + 'length' b@.

data Measured a = Measured{_measuredLength :: Int, extract :: a} deriving (Data, Eq, Show, Typeable)

-- | Create a new 'Measured' value.
measure :: Factorial a => a -> Measured a
measure x = Measured (length x) x

instance Ord a => Ord (Measured a) where
   compare (Measured _ x) (Measured _ y) = compare x y

instance StableFactorial a => Semigroup (Measured a) where
   Measured m a <> Measured n b = Measured (m + n) (a <> b)

instance (StableFactorial a, Monoid a) => Monoid (Measured a) where
   mempty = Measured 0 mempty
   mappend = (<>)

instance (StableFactorial a, Monoid a) => MonoidNull (Measured a) where
   null (Measured n _) = n == 0

instance (StableFactorial a, Monoid a) => PositiveMonoid (Measured a)

instance (LeftReductive a, StableFactorial a) => LeftReductive (Measured a) where
   stripPrefix (Measured m x) (Measured n y) = fmap (Measured (n - m)) (stripPrefix x y)

instance (RightReductive a, StableFactorial a) => RightReductive (Measured a) where
   stripSuffix (Measured m x) (Measured n y) = fmap (Measured (n - m)) (stripSuffix x y)

instance (LeftGCDMonoid a, StableFactorial a) => LeftGCDMonoid (Measured a) where
   commonPrefix (Measured _ x) (Measured _ y) = measure (commonPrefix x y)

instance (RightGCDMonoid a, StableFactorial a) => RightGCDMonoid (Measured a) where
   commonSuffix (Measured _ x) (Measured _ y) = measure (commonSuffix x y)

instance (StableFactorial a, MonoidNull a) => Factorial (Measured a) where
   factors (Measured _ x) = List.map (Measured 1) (factors x)
   primePrefix m@(Measured _ x) = if null x then m else Measured 1 (primePrefix x)
   primeSuffix m@(Measured _ x) = if null x then m else Measured 1 (primeSuffix x)
   foldl f a0 (Measured _ x) = Factorial.foldl g a0 x
      where g a = f a . Measured 1
   foldl' f a0 (Measured _ x) = Factorial.foldl' g a0 x
      where g a = f a . Measured 1
   foldr f a0 (Measured _ x) = Factorial.foldr g a0 x
      where g = f . Measured 1
   foldMap f (Measured _ x) = Factorial.foldMap (f . Measured 1) x
   length (Measured n _) = n
   reverse (Measured n x) = Measured n (reverse x)

instance (StableFactorial a, FactorialMonoid a) => FactorialMonoid (Measured a) where
   splitPrimePrefix (Measured n x) = case splitPrimePrefix x
                                     of Nothing -> Nothing
                                        Just (p, s) -> Just (Measured 1 p, Measured (n - 1) s)
   splitPrimeSuffix (Measured n x) = case splitPrimeSuffix x
                                     of Nothing -> Nothing
                                        Just (p, s) -> Just (Measured (n - 1) p, Measured 1 s)
   span p (Measured n x) = (xp', xs')
      where (xp, xs) = Factorial.span (p . Measured 1) x
            xp' = measure xp
            xs' = Measured (n - length xp') xs
   split p (Measured _ x) = measure <$> Factorial.split (p . Measured 1) x
   splitAt m (Measured n x) | m <= 0 = (mempty, Measured n x)
                            | m >= n = (Measured n x, mempty)
                            | otherwise = (Measured m xp, Measured (n - m) xs)
      where (xp, xs) = splitAt m x

instance (StableFactorial a, MonoidNull a) => StableFactorial (Measured a)

instance (FactorialMonoid a, IsString a) => IsString (Measured a) where
   fromString = measure . fromString

instance (Eq a, StableFactorial a, TextualMonoid a) => TextualMonoid (Measured a) where
   fromText = measure . fromText
   singleton = Measured 1 . singleton
   splitCharacterPrefix (Measured n x) = (Measured (n - 1) <$>) <$> splitCharacterPrefix x
   characterPrefix (Measured _ x) = characterPrefix x
   map f (Measured n x) = Measured n (map f x)
   any p (Measured _ x) = any p x
   all p (Measured _ x) = all p x

   foldl ft fc a0 (Measured _ x) = Textual.foldl (\a-> ft a . Measured 1) fc a0 x
   foldl' ft fc a0 (Measured _ x) = Textual.foldl' (\a-> ft a . Measured 1) fc a0 x
   foldr ft fc a0 (Measured _ x) = Textual.foldr (ft . Measured 1) fc a0 x
   toString ft (Measured _ x) = toString (ft . Measured 1) x
   toText ft (Measured _ x) = toText (ft . Measured 1) x

   span pt pc (Measured n x) = (xp', xs')
      where (xp, xs) = Textual.span (pt . Measured 1) pc x
            xp' = measure xp
            xs' = Measured (n - length xp') xs
   break pt pc = Textual.span (not . pt) (not . pc)

   find p (Measured _ x) = find p x
