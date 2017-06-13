{-
    Copyright 2013-2018 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Stateful'.
--
-- >> let s = setState [4] $ pure "data" :: Stateful [Int] String
-- >> s
-- >Stateful ("data",[4])
-- >> factors s
-- >[Stateful ("d",[]),Stateful ("a",[]),Stateful ("t",[]),Stateful ("a",[]),Stateful ("",[4])]

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.Stateful (
   Stateful(Stateful), extract, state, setState
   )
where

import Control.Applicative -- (Applicative(..))
import Data.Functor -- ((<$>))
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup.Cancellative (LeftReductiveSemigroup(..), RightReductiveSemigroup(..))
import Data.Semigroup.Factorial (FactorialSemigroup(..), StableFactorialSemigroup)
import Data.Monoid.Cancellative (LeftReductiveMonoid, LeftGCDMonoid(..), RightReductiveMonoid, RightGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..), StableFactorialMonoid)
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Semigroup.Factorial as Factorial
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual

import Prelude hiding (all, any, break, elem, drop, filter, foldl, foldl1, foldr, foldr1, gcd, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt, take)

-- | @'Stateful' a b@ is a wrapper around the 'Monoid' @b@ that carries the state @a@ along. The state type @a@ must be
-- a monoid as well if 'Stateful' is to be of any use. In the 'FactorialMonoid' and 'TextualMonoid' class instances, the
-- monoid @b@ has the priority and the state @a@ is left for the end.
newtype Stateful a b = Stateful (b, a) deriving (Eq, Ord, Show)

extract :: Stateful a b -> b
extract (Stateful (t, _)) = t

state :: Stateful a b -> a
state (Stateful (_, x)) = x

setState :: a -> Stateful a b -> Stateful a b
setState s (Stateful (t, _)) = Stateful (t, s)

instance Functor (Stateful a) where
   fmap f (Stateful (x, s)) = Stateful (f x, s)

instance Monoid a => Applicative (Stateful a) where
   pure m = Stateful (m, mempty)
   Stateful (f, s1) <*> Stateful (x, s2) = Stateful (f x, mappend s1 s2)

instance (Semigroup a, Semigroup b) => Semigroup (Stateful a b) where
   Stateful x <> Stateful y = Stateful (x <> y)
   {-# INLINE (<>) #-}

instance (Semigroup a, Semigroup b) => Semigroup (Stateful a b) where
   Stateful x <> Stateful y = Stateful (x <> y)
   {-# INLINE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (Stateful a b) where
   mempty = Stateful mempty
   mappend = (<>)
   {-# INLINE mempty #-}
   {-# INLINE mappend #-}

instance (MonoidNull a, MonoidNull b) => MonoidNull (Stateful a b) where
   null (Stateful x) = null x
   {-# INLINE null #-}

instance (PositiveMonoid a, PositiveMonoid b) => PositiveMonoid (Stateful a b)

instance (LeftReductiveSemigroup a, LeftReductiveSemigroup b) => LeftReductiveSemigroup (Stateful a b) where
   isPrefixOf (Stateful x) (Stateful x') = isPrefixOf x x'
   stripPrefix (Stateful x) (Stateful x') = Stateful <$> stripPrefix x x'
   {-# INLINE isPrefixOf #-}
   {-# INLINE stripPrefix #-}

instance (RightReductiveSemigroup a, RightReductiveSemigroup b) => RightReductiveSemigroup (Stateful a b) where
   isSuffixOf (Stateful x) (Stateful x') = isSuffixOf x x'
   stripSuffix (Stateful x) (Stateful x') = Stateful <$> stripSuffix x x'
   {-# INLINE stripSuffix #-}
   {-# INLINE isSuffixOf #-}

instance (LeftReductiveMonoid a, LeftReductiveMonoid b) => LeftReductiveMonoid (Stateful a b)
instance (RightReductiveMonoid a, RightReductiveMonoid b) => RightReductiveMonoid (Stateful a b)

instance (LeftGCDMonoid a, LeftGCDMonoid b) => LeftGCDMonoid (Stateful a b) where
   commonPrefix (Stateful x) (Stateful x') = Stateful (commonPrefix x x')
   stripCommonPrefix (Stateful x) (Stateful x') = (Stateful prefix, Stateful suffix1, Stateful suffix2)
      where (prefix, suffix1, suffix2) = stripCommonPrefix x x'
   {-# INLINE commonPrefix #-}
   {-# INLINE stripCommonPrefix #-}

instance (RightGCDMonoid a, RightGCDMonoid b) => RightGCDMonoid (Stateful a b) where
   commonSuffix (Stateful x) (Stateful x') = Stateful (commonSuffix x x')
   {-# INLINE commonSuffix #-}

instance (MonoidNull a, MonoidNull b, FactorialSemigroup a, FactorialSemigroup b) =>
         FactorialSemigroup (Stateful a b) where
   factors (Stateful x) = List.map Stateful (factors x)
   length (Stateful x) = length x
   reverse (Stateful x) = Stateful (reverse x)
   primePrefix (Stateful x) = Stateful (primePrefix x)
   primeSuffix (Stateful x) = Stateful (primeSuffix x)
   foldl f a0 (Stateful x) = Factorial.foldl f' a0 x
      where f' a x1 = f a (Stateful x1)
   foldl' f a0 (Stateful x) = Factorial.foldl' f' a0 x
      where f' a x1 = f a (Stateful x1)
   foldr f a (Stateful x) = Factorial.foldr (f . Stateful) a x
   foldMap f (Stateful x) = Factorial.foldMap (f . Stateful) x
   {-# INLINE primePrefix #-}
   {-# INLINE primeSuffix #-}
   {-# INLINE foldl' #-}
   {-# INLINE foldr #-}
   {-# INLINE foldMap #-}
   {-# INLINE length #-}

instance (FactorialMonoid a, FactorialMonoid b) => FactorialMonoid (Stateful a b) where
   splitPrimePrefix (Stateful x) = do (xp, xs) <- splitPrimePrefix x
                                      return (Stateful xp, Stateful xs)
   splitPrimeSuffix (Stateful x) = do (xp, xs) <- splitPrimeSuffix x
                                      return (Stateful xp, Stateful xs)
   span p (Stateful x) = (Stateful xp, Stateful xs)
      where (xp, xs) = Factorial.span (p . Stateful) x
   spanMaybe s0 f (Stateful x) = (Stateful xp, Stateful xs, s')
      where (xp, xs, s') = Factorial.spanMaybe s0 f' x
            f' s x1 = f s (Stateful x1)
   spanMaybe' s0 f (Stateful x) = (Stateful xp, Stateful xs, s')
      where (xp, xs, s') = Factorial.spanMaybe' s0 f' x
            f' s x1 = f s (Stateful x1)
   split p (Stateful x) = List.map Stateful (Factorial.split (p . Stateful) x)
   splitAt n (Stateful x) = (Stateful xp, Stateful xs)
      where (xp, xs) = splitAt n x
   take n (Stateful x) = Stateful (take n x)
   drop n (Stateful x) = Stateful (drop n x)
   {-# INLINE splitPrimePrefix #-}
   {-# INLINE splitPrimeSuffix #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE splitAt #-}
   {-# INLINE take #-}
   {-# INLINE drop #-}

instance (StableFactorialMonoid a, StableFactorialMonoid b) => StableFactorialSemigroup (Stateful a b)

instance (StableFactorialMonoid a, StableFactorialMonoid b) => StableFactorialMonoid (Stateful a b)

instance (Monoid a, IsString b) => IsString (Stateful a b) where
   fromString = pure . fromString

instance (LeftGCDMonoid a, FactorialMonoid a, TextualMonoid b) => TextualMonoid (Stateful a b) where
   fromText t = Stateful (fromText t, mempty)
   singleton c = Stateful (singleton c, mempty)

   characterPrefix = characterPrefix . extract
   splitCharacterPrefix (Stateful (t, x)) = do (c, t') <- splitCharacterPrefix t
                                               return (c, Stateful (t', x))

   map f (Stateful (t, x)) = Stateful (Textual.map f t, x)
   all p = all p . extract
   any p = any p . extract

   foldl fx fc a0 (Stateful (t, x)) = Factorial.foldl f2 (Textual.foldl f1 fc a0 t) x
      where f1 a = fx a . fromFst
            f2 a = fx a . fromSnd
   foldr fx fc a (Stateful (t, x)) = Textual.foldr (fx . fromFst) fc (Factorial.foldr (fx . fromSnd) a x) t
   foldl' fx fc a0 (Stateful (t, x)) = a' `seq` Factorial.foldl' f2 a' x
      where a' = Textual.foldl' f1 fc a0 t
            f1 a = fx a . fromFst
            f2 a = fx a . fromSnd
   foldl_' fc a (Stateful (t, _)) = foldl_' fc a t
   foldr_ fc a (Stateful (t, _)) = Textual.foldr_ fc a t
   toString fx (Stateful (t, x)) = toString (fx . fromFst) t ++ Factorial.foldMap (fx . fromSnd) x

   scanl f c (Stateful (t, x)) = Stateful (Textual.scanl f c t, x)
   scanl1 f (Stateful (t, x)) = Stateful (Textual.scanl1 f t, x)
   scanr f c (Stateful (t, x)) = Stateful (Textual.scanr f c t, x)
   scanr1 f (Stateful (t, x)) = Stateful (Textual.scanr1 f t, x)
   mapAccumL f a (Stateful (t, x)) = (a', Stateful (t', x))
      where (a', t') = Textual.mapAccumL f a t
   mapAccumR f a (Stateful (t, x)) = (a', Stateful (t', x))
      where (a', t') = Textual.mapAccumR f a t

   span pt pc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs))
      where (tp, ts) = Textual.span (pt . fromFst) pc t
            (xp, xs) | null ts = Factorial.span (pt . fromSnd) x
                     | otherwise = (mempty, x)
   span_ bt pc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs))
      where (tp, ts) = Textual.span_ bt pc t
            (xp, xs) | null ts && bt = (x, mempty)
                     | otherwise = (mempty, x)
   break pt pc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs))
      where (tp, ts) = Textual.break (pt . fromFst) pc t
            (xp, xs) | null ts = Factorial.break (pt . fromSnd) x
                     | otherwise = (mempty, x)
   spanMaybe s0 ft fc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs), s'')
      where (tp, ts, s') = Textual.spanMaybe s0 ft' fc t
            (xp, xs, s'') | null ts = Factorial.spanMaybe s' ft'' x
                          | otherwise = (mempty, x, s')
            ft' s t1 = ft s (Stateful (t1, mempty))
            ft'' s x1 = ft s (Stateful (mempty, x1))
   spanMaybe' s0 ft fc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs), s'')
      where (tp, ts, s') = Textual.spanMaybe' s0 ft' fc t
            (xp, xs, s'') | null ts = Factorial.spanMaybe' s' ft'' x
                          | otherwise = (mempty, x, s')
            ft' s t1 = ft s (Stateful (t1, mempty))
            ft'' s x1 = ft s (Stateful (mempty, x1))
   spanMaybe_' s0 fc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs), s')
      where (tp, ts, s') = Textual.spanMaybe_' s0 fc t
            (xp, xs) | null ts = (x, mempty)
                     | otherwise = (mempty, x)
   split p (Stateful (t, x)) = restore id ts
      where ts = Textual.split p t
            restore f [t1] = f [Stateful (t1, x)]
            restore f ~(hd:tl) = restore (f . (Stateful (hd, mempty):)) tl
   find p = find p . extract
   elem c = elem c . extract

   {-# INLINE characterPrefix #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE map #-}
   {-# INLINE foldl' #-}
   {-# INLINE foldr #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe_' #-}
   {-# INLINE span_ #-}
   {-# INLINE any #-}
   {-# INLINE all #-}
   {-# INLINE split #-}
   {-# INLINE find #-}
   {-# INLINE elem #-}

{-# INLINE fromFst #-}
fromFst :: Monoid b => a -> Stateful b a
fromFst a = Stateful (a, mempty)

{-# INLINE fromSnd #-}
fromSnd :: Monoid a => b -> Stateful b a
fromSnd b = Stateful (mempty, b)
