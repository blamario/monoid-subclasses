{- 
    Copyright 2013-2014 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Measured'.
-- 

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.Measured (
   Measured, inject, measure, extract
   )
where

import Prelude hiding (all, any, break, filter, foldl, foldl1, foldr, foldr1, map, concatMap, 
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt)
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Monoid (Monoid(..), (<>), First(..), Sum(..))
import Data.Monoid.Cancellative (LeftReductiveMonoid(..), RightReductiveMonoid(..),
                                 LeftGCDMonoid(..), RightGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..), StableFactorialMonoid)
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual

-- | @'Measured' a@ is a wrapper around the 'FactorialMonoid' @a@ that memoizes the monoid's 'length' so it becomes a
-- constant-time operation. The parameter is restricted to the 'StableFactorialMonoid' class, which guarantees that
-- @'length' (a <> b) == 'length' a + 'length' b@.

data Measured a = Measured{measuredLength :: Int, extract :: a} deriving (Eq, Show)

-- | Create a new 'Measured' value.
measure :: FactorialMonoid a => a -> Measured a
measure x = Measured (length x) x

inject :: FactorialMonoid a => a -> Measured a
inject = measure
{-# DEPRECATED inject "Use measure instead." #-}

instance Ord a => Ord (Measured a) where
   compare (Measured _ x) (Measured _ y) = compare x y

instance StableFactorialMonoid a => Monoid (Measured a) where
   mempty = Measured 0 mempty
   mappend (Measured m a) (Measured n b) = Measured (m + n) (mappend a b)

instance StableFactorialMonoid a => MonoidNull (Measured a) where
   null (Measured n x) = n == 0

instance StableFactorialMonoid a => PositiveMonoid (Measured a)

instance (LeftReductiveMonoid a, StableFactorialMonoid a) => LeftReductiveMonoid (Measured a) where
   stripPrefix (Measured m x) (Measured n y) = fmap (Measured (n - m)) (stripPrefix x y)

instance (RightReductiveMonoid a, StableFactorialMonoid a) => RightReductiveMonoid (Measured a) where
   stripSuffix (Measured m x) (Measured n y) = fmap (Measured (n - m)) (stripSuffix x y)

instance (LeftGCDMonoid a, StableFactorialMonoid a) => LeftGCDMonoid (Measured a) where
   commonPrefix (Measured _ x) (Measured _ y) = inject (commonPrefix x y)

instance (RightGCDMonoid a, StableFactorialMonoid a) => RightGCDMonoid (Measured a) where
   commonSuffix (Measured _ x) (Measured _ y) = inject (commonSuffix x y)

instance StableFactorialMonoid a => FactorialMonoid (Measured a) where
   factors (Measured _ x) = List.map (Measured 1) (factors x)
   primePrefix m@(Measured _ x) = if null x then m else Measured 1 (primePrefix x)
   primeSuffix m@(Measured _ x) = if null x then m else Measured 1 (primeSuffix x)
   splitPrimePrefix (Measured n x) = case splitPrimePrefix x
                                     of Nothing -> Nothing
                                        Just (p, s) -> Just (Measured 1 p, Measured (n - 1) s)
   splitPrimeSuffix (Measured n x) = case splitPrimeSuffix x
                                     of Nothing -> Nothing
                                        Just (p, s) -> Just (Measured (n - 1) p, Measured 1 s)
   foldl f a (Measured _ x) = Factorial.foldl g a x
      where g a = f a . Measured 1
   foldl' f a (Measured _ x) = Factorial.foldl' g a x
      where g a = f a . Measured 1
   foldr f a (Measured _ x) = Factorial.foldr g a x
      where g = f . Measured 1
   length (Measured n _) = n
   foldMap f (Measured _ x) = Factorial.foldMap (f . Measured 1) x
   span p (Measured n x) = (xp', xs')
      where (xp, xs) = Factorial.span (p . Measured 1) x
            xp' = inject xp
            xs' = Measured (n - length xp') xs
   split p (Measured _ x) = inject <$> Factorial.split (p . Measured 1) x
   splitAt m (Measured n x) | m <= 0 = (mempty, Measured n x)
                            | m >= n = (Measured n x, mempty)
                            | otherwise = (Measured m xp, Measured (n - m) xs)
      where (xp, xs) = splitAt m x
   reverse (Measured n x) = Measured n (reverse x)

instance StableFactorialMonoid a => StableFactorialMonoid (Measured a)

instance (FactorialMonoid a, IsString a) => IsString (Measured a) where
   fromString = inject . fromString

instance (Eq a, TextualMonoid a, StableFactorialMonoid a) => TextualMonoid (Measured a) where
   fromText = inject . fromText
   singleton = Measured 1 . singleton
   splitCharacterPrefix (Measured n x) = (Measured (n - 1) <$>) <$> splitCharacterPrefix x
   characterPrefix (Measured _ x) = characterPrefix x
   map f (Measured n x) = Measured n (map f x)
   any p (Measured _ x) = any p x
   all p (Measured _ x) = all p x

   foldl ft fc a (Measured _ x) = Textual.foldl (\a-> ft a . Measured 1) fc a x
   foldl' ft fc a (Measured _ x) = Textual.foldl' (\a-> ft a . Measured 1) fc a x
   foldr ft fc a (Measured _ x) = Textual.foldr (ft . Measured 1) fc a x

   span pt pc (Measured n x) = (xp', xs')
      where (xp, xs) = Textual.span (pt . Measured 1) pc x
            xp' = inject xp
            xs' = Measured (n - length xp') xs
   break pt pc = Textual.span (not . pt) (not . pc)

   find p (Measured _ x) = find p x
