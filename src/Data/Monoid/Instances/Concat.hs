{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Concat'.
-- 

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.Concat (
   Concat, concatenate, extract, force
   )
where

import Control.Applicative -- (Applicative(..))
import Control.Arrow (first)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..), First(..), Sum(..))
import Data.Semigroup.Cancellative (LeftReductive(..), RightReductive(..))
import Data.Semigroup.Factorial (Factorial(..), StableFactorial)
import Data.Monoid.GCD (LeftGCDMonoid(..), RightGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as Text

import Prelude hiding (all, any, break, filter, foldl, foldl1, foldr, foldr1, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt, pi)

-- | @'Concat'@ is a transparent monoid transformer. The behaviour of the @'Concat' a@ instances of monoid subclasses is
-- identical to the behaviour of their @a@ instances, up to the 'pure' isomorphism.
--
-- The only purpose of 'Concat' then is to change the performance characteristics of various operations. Most
-- importantly, injecting a monoid into 'Concat' has the effect of making 'mappend' a constant-time operation. The
-- `splitPrimePrefix` and `splitPrimeSuffix` operations are amortized to constant time, provided that only one or the
-- other is used. Using both operations alternately will trigger the worst-case behaviour of O(n).
--
data Concat a = Leaf a
              | Concat a :<> Concat a
              deriving Show

{-# DEPRECATED concatenate, extract "Concat is not wrapping Seq any more, don't use concatenate nor extract." #-}
concatenate :: PositiveMonoid a => Seq a -> Concat a
concatenate q
   | Foldable.all null q = mempty
   | otherwise = Foldable.foldr (\a c-> if null a then c else Leaf a <> c) mempty q

extract :: Concat a -> Seq a
extract = Seq.fromList . Foldable.toList

force :: Semigroup a => Concat a -> a
force (Leaf x) = x
force (x :<> y) = force x <> force y

instance (Eq a, Semigroup a) => Eq (Concat a) where
   x == y = force x == force y

instance (Ord a, Semigroup a) => Ord (Concat a) where
   compare x y = compare (force x) (force y)

instance Functor Concat where
   fmap f (Leaf x) = Leaf (f x)
   fmap f (l :<> r) = fmap f l :<> fmap f r

instance Applicative Concat where
   pure = Leaf
   Leaf f <*> x = f <$> x
   (f1 :<> f2) <*> x = (f1 <*> x) :<> (f2 <*> x)

instance Foldable.Foldable Concat where
   fold (Leaf x) = x
   fold (x :<> y) = Foldable.fold x `mappend` Foldable.fold y
   foldMap f (Leaf x) = f x
   foldMap f (x :<> y) = Foldable.foldMap f x `mappend` Foldable.foldMap f y
   foldl f a (Leaf x) = f a x
   foldl f a (x :<> y) = Foldable.foldl f (Foldable.foldl f a x) y
   foldl' f a (Leaf x) = f a x
   foldl' f a (x :<> y) = let a' = Foldable.foldl' f a x in a' `seq` Foldable.foldl' f a' y
   foldr f a (Leaf x) = f x a
   foldr f a (x :<> y) = Foldable.foldr f (Foldable.foldr f a y) x
   foldr' f a (Leaf x) = f x a
   foldr' f a (x :<> y) = let a' = Foldable.foldr' f a y in Foldable.foldr' f a' x

instance PositiveMonoid a => Semigroup (Concat a) where
   x <> y 
      | null x = y
      | null y = x
      | otherwise = x :<> y

instance PositiveMonoid a => Monoid (Concat a) where
   mempty = Leaf mempty
   mappend = (<>)

instance PositiveMonoid a => MonoidNull (Concat a) where
   null (Leaf x) = null x
   null _ = False

instance PositiveMonoid a => PositiveMonoid (Concat a)

instance (LeftReductive a, StableFactorial a, PositiveMonoid a) => LeftReductive (Concat a) where
   stripPrefix (Leaf x) (Leaf y) = Leaf <$> stripPrefix x y
   stripPrefix (xp :<> xs) y = stripPrefix xp y >>= stripPrefix xs
   stripPrefix x (yp :<> ys) = case (stripPrefix x yp, stripPrefix yp x)
                               of (Just yps, _) -> Just (yps <> ys)
                                  (Nothing, Nothing) -> Nothing
                                  (Nothing, Just xs) -> stripPrefix xs ys

instance (RightReductive a, StableFactorial a, PositiveMonoid a) => RightReductive (Concat a) where
   stripSuffix (Leaf x) (Leaf y) = Leaf <$> stripSuffix x y
   stripSuffix (xp :<> xs) y = stripSuffix xs y >>= stripSuffix xp
   stripSuffix x (yp :<> ys) = case (stripSuffix x ys, stripSuffix ys x)
                               of (Just ysp, _) -> Just (yp <> ysp)
                                  (Nothing, Nothing) -> Nothing
                                  (Nothing, Just xp) -> stripSuffix xp yp

instance (LeftGCDMonoid a, StableFactorial a, PositiveMonoid a) => LeftGCDMonoid (Concat a) where
   stripCommonPrefix (Leaf x) (Leaf y) = map3 Leaf (stripCommonPrefix x y)
   stripCommonPrefix (xp :<> xs) y
      | null xps = (xp <> xsp, xss, yss)
      | otherwise = (xpp, xps <> xs, ys)
      where (xpp, xps, ys) = stripCommonPrefix xp y
            (xsp, xss, yss) = stripCommonPrefix xs ys
   stripCommonPrefix x (yp :<> ys)
      | null yps = (yp <> ysp, xss, yss)
      | otherwise = (ypp, xs, yps <> ys)
      where (ypp, xs, yps) = stripCommonPrefix x yp
            (ysp, xss, yss) = stripCommonPrefix xs ys

instance (RightGCDMonoid a, StableFactorial a, PositiveMonoid a) => RightGCDMonoid (Concat a) where
   stripCommonSuffix (Leaf x) (Leaf y) = map3 Leaf (stripCommonSuffix x y)
   stripCommonSuffix (xp :<> xs) y
      | null xsp = (xpp, ypp, xps <> xs)
      | otherwise = (xp <> xsp, yp, xss)
      where (xsp, yp, xss) = stripCommonSuffix xs y
            (xpp, ypp, xps) = stripCommonSuffix xp yp
   stripCommonSuffix x (yp :<> ys)
      | null ysp = (xpp, ypp, yps <> ys)
      | otherwise = (xp, yp <> ysp, yss)
      where (xp, ysp, yss) = stripCommonSuffix x ys
            (xpp, ypp, yps) = stripCommonSuffix xp yp

instance (Factorial a, PositiveMonoid a) => Factorial (Concat a) where
   factors c = toList c []
      where toList (Leaf x) rest
               | null x = rest
               | otherwise = (Leaf <$> factors x) ++ rest
            toList (x :<> y) rest = toList x (toList y rest)
   primePrefix (Leaf x) = Leaf (primePrefix x)
   primePrefix (x :<> _) = primePrefix x
   primeSuffix (Leaf x) = Leaf (primeSuffix x)
   primeSuffix (_ :<> y) = primeSuffix y

   foldl f = Foldable.foldl g
      where g = Factorial.foldl (\a-> f a . Leaf)
   foldl' f = Foldable.foldl' g
      where g = Factorial.foldl' (\a-> f a . Leaf)
   foldr f = Foldable.foldr g
      where g a b = Factorial.foldr (f . Leaf) b a
   foldMap f = Foldable.foldMap (Factorial.foldMap (f . Leaf))
   length x = getSum $ Foldable.foldMap (Sum . length) x
   reverse (Leaf x) = Leaf (reverse x)
   reverse (x :<> y) = reverse y :<> reverse x

instance (FactorialMonoid a, PositiveMonoid a) => FactorialMonoid (Concat a) where
   splitPrimePrefix (Leaf x) = map2 Leaf <$> splitPrimePrefix x
   splitPrimePrefix (x :<> y) = ((<> y) <$>) <$> splitPrimePrefix x
   splitPrimeSuffix (Leaf x) = map2 Leaf <$> splitPrimeSuffix x
   splitPrimeSuffix (x :<> y) = first (x <>) <$> splitPrimeSuffix y
   span p (Leaf x) = map2 Leaf (Factorial.span (p . Leaf) x)
   span p (x :<> y)
      | null xs = (x <> yp, ys)
      | otherwise = (xp, xs :<> y)
      where (xp, xs) = Factorial.span p x
            (yp, ys) = Factorial.span p y
   spanMaybe s0 f (Leaf x) = first2 Leaf (Factorial.spanMaybe s0 (\s-> f s . Leaf) x)
   spanMaybe s0 f (x :<> y)
      | null xs = (x :<> yp, ys, s2)
      | otherwise = (xp, xs :<> y, s1)
      where (xp, xs, s1) = Factorial.spanMaybe s0 f x
            (yp, ys, s2) = Factorial.spanMaybe s1 f y
   spanMaybe' s0 f c = seq s0 $
      case c
      of Leaf x -> first2 Leaf (Factorial.spanMaybe' s0 (\s-> f s . Leaf) x)
         x :<> y -> let (xp, xs, s1) = Factorial.spanMaybe' s0 f x
                        (yp, ys, s2) = Factorial.spanMaybe' s1 f y
                    in if null xs then (x :<> yp, ys, s2) else (xp, xs :<> y, s1)

   split p = Foldable.foldr splitNext [mempty]
      where splitNext a ~(xp:xs) =
               let as = Leaf <$> Factorial.split (p . Leaf) a
               in if null xp
                  then as ++ xs
                  else init as ++ (last as <> xp):xs
   splitAt 0 c = (mempty, c)
   splitAt n (Leaf x) = map2 Leaf (Factorial.splitAt n x)
   splitAt n (x :<> y)
      | k < n = (x :<> yp, ys)
      | k > n = (xp, xs :<> y)
      | otherwise = (x, y)
      where k = length x
            (yp, ys) = splitAt (n - k) y
            (xp, xs) = splitAt n x

instance (Factorial a, PositiveMonoid a) => StableFactorial (Concat a)

instance (IsString a) => IsString (Concat a) where
   fromString s = Leaf (fromString s)

instance (Eq a, TextualMonoid a, StableFactorial a, PositiveMonoid a) => TextualMonoid (Concat a) where
   fromText t = Leaf (fromText t)
   singleton = Leaf . singleton
   splitCharacterPrefix (Leaf x) = (Leaf <$>) <$> splitCharacterPrefix x
   splitCharacterPrefix (x :<> y) = ((<> y) <$>) <$> splitCharacterPrefix x
   characterPrefix (Leaf x) = characterPrefix x
   characterPrefix (x :<> _) = characterPrefix x
   map f x = map f <$> x
   toString ft x = List.concatMap (toString $ ft . Leaf) (Foldable.toList x)
   toText ft x = Text.concat (toText (ft . Leaf) <$> Foldable.toList x)

   foldl ft fc = Foldable.foldl g
      where g = Textual.foldl (\a-> ft a . Leaf) fc
   foldl' ft fc = Foldable.foldl' g
      where g = Textual.foldl' (\a-> ft a . Leaf) fc
   foldr ft fc = Foldable.foldr g
      where g a b = Textual.foldr (ft . Leaf) fc b a
   any p = Foldable.any (any p)
   all p = Foldable.all (all p)

   span pt pc (Leaf x) = map2 Leaf (Textual.span (pt . Leaf) pc x)
   span pt pc (x :<> y)
      | null xs = (x <> yp, ys)
      | otherwise = (xp, xs :<> y)
      where (xp, xs) = Textual.span pt pc x
            (yp, ys) = Textual.span pt pc y
   span_ bt pc (Leaf x) = map2 Leaf (Textual.span_ bt pc x)
   span_ bt pc (x :<> y)
      | null xs = (x <> yp, ys)
      | otherwise = (xp, xs :<> y)
      where (xp, xs) = Textual.span_ bt pc x
            (yp, ys) = Textual.span_ bt pc y
   break pt pc = Textual.span (not . pt) (not . pc)
   takeWhile_ bt pc = fst . span_ bt pc
   dropWhile_ bt pc = snd . span_ bt pc
   break_ bt pc = span_ (not bt) (not . pc)

   spanMaybe s0 ft fc (Leaf x) = first2 Leaf (Textual.spanMaybe s0 (\s-> ft s . Leaf) fc x)
   spanMaybe s0 ft fc (x :<> y)
      | null xs = (x :<> yp, ys, s2)
      | otherwise = (xp, xs :<> y, s1)
      where (xp, xs, s1) = Textual.spanMaybe s0 ft fc x
            (yp, ys, s2) = Textual.spanMaybe s1 ft fc y
   spanMaybe' s0 ft fc c = seq s0 $
      case c
      of Leaf x -> first2 Leaf (Textual.spanMaybe' s0 (\s-> ft s . Leaf) fc x)
         x :<> y -> let (xp, xs, s1) = Textual.spanMaybe' s0 ft fc x
                        (yp, ys, s2) = Textual.spanMaybe' s1 ft fc y
                    in if null xs then (x :<> yp, ys, s2) else (xp, xs :<> y, s1)
   spanMaybe_ s0 fc (Leaf x) = first2 Leaf (Textual.spanMaybe_ s0 fc x)
   spanMaybe_ s0 fc (x :<> y)
      | null xs = (x :<> yp, ys, s2)
      | otherwise = (xp, xs :<> y, s1)
      where (xp, xs, s1) = Textual.spanMaybe_ s0 fc x
            (yp, ys, s2) = Textual.spanMaybe_ s1 fc y
   spanMaybe_' s0 fc c = seq s0 $
      case c
      of Leaf x -> first2 Leaf (Textual.spanMaybe_' s0 fc x)
         x :<> y -> let (xp, xs, s1) = Textual.spanMaybe_' s0 fc x
                        (yp, ys, s2) = Textual.spanMaybe_' s1 fc y
                    in if null xs then (x :<> yp, ys, s2) else (xp, xs :<> y, s1)

   split p = Foldable.foldr splitNext [mempty]
      where splitNext a ~(xp:xs) =
               let as = Leaf <$> Textual.split p a
               in if null xp
                  then as ++ xs
                  else init as ++ (last as <> xp):xs
   find p x = getFirst $ Foldable.foldMap (First . find p) x
   elem i = Foldable.any (Textual.elem i)

-- Utility functions

map2 :: (a -> b) -> (a, a) -> (b, b)
map2 f (x, y) = (f x, f y)

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (x, y, z) = (f x, f y, f z)

first2 :: (a -> b) -> (a, a, c) -> (b, b, c)
first2 f (x, y, z) = (f x, f y, z)
