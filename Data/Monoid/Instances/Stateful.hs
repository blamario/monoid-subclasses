{-
    Copyright 2013-2015 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Stateful'.
--

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.Stateful (
   Stateful(Stateful), extract, state, setState
   )
where

import Prelude hiding (all, any, break, elem, filter, foldl, foldl1, foldr, foldr1, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt)
import Control.Applicative (Applicative(..))
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Monoid (Monoid(..), (<>), First(..), Sum(..))
import Data.Monoid.Cancellative (LeftReductiveMonoid(..), RightReductiveMonoid(..), ReductiveMonoid(..),
                                 LeftGCDMonoid(..), RightGCDMonoid(..), GCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..), StableFactorialMonoid)
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual

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
   Stateful (f, s1) <*> Stateful (x, s2) = Stateful (f x, s1 <> s2)

instance (Monoid a, Monoid b) => Monoid (Stateful a b) where
   mempty = Stateful mempty
   mappend (Stateful x) (Stateful y) = Stateful (x <> y)

instance (MonoidNull a, MonoidNull b) => MonoidNull (Stateful a b) where
   null (Stateful x) = null x

instance (PositiveMonoid a, PositiveMonoid b) => PositiveMonoid (Stateful a b)

instance (LeftReductiveMonoid a, LeftReductiveMonoid b) => LeftReductiveMonoid (Stateful a b) where
   stripPrefix (Stateful x) (Stateful x') = Stateful <$> stripPrefix x x'

instance (RightReductiveMonoid a, RightReductiveMonoid b) => RightReductiveMonoid (Stateful a b) where
   stripSuffix (Stateful x) (Stateful x') = Stateful <$> stripSuffix x x'

instance (LeftGCDMonoid a, LeftGCDMonoid b) => LeftGCDMonoid (Stateful a b) where
   commonPrefix (Stateful x) (Stateful x') = Stateful (commonPrefix x x')

instance (RightGCDMonoid a, RightGCDMonoid b) => RightGCDMonoid (Stateful a b) where
   commonSuffix (Stateful x) (Stateful x') = Stateful (commonSuffix x x')

instance (FactorialMonoid a, FactorialMonoid b) => FactorialMonoid (Stateful a b) where
   factors (Stateful x) = List.map Stateful (factors x)
   length (Stateful x) = length x
   reverse (Stateful x) = Stateful (reverse x)
   primePrefix (Stateful x) = Stateful (primePrefix x)
   primeSuffix (Stateful x) = Stateful (primeSuffix x)
   splitPrimePrefix (Stateful x) = do (xp, xs) <- splitPrimePrefix x
                                      return (Stateful xp, Stateful xs)
   splitPrimeSuffix (Stateful x) = do (xp, xs) <- splitPrimeSuffix x
                                      return (Stateful xp, Stateful xs)
   foldl f a (Stateful x) = Factorial.foldl f' a x
      where f' a x = f a (Stateful x)
   foldl' f a (Stateful x) = Factorial.foldl' f' a x
      where f' a x = f a (Stateful x)
   foldr f a (Stateful x) = Factorial.foldr (f . Stateful) a x
   foldMap f (Stateful x) = Factorial.foldMap (f . Stateful) x
   span p (Stateful x) = (Stateful xp, Stateful xs)
      where (xp, xs) = Factorial.span (p . Stateful) x
   split p (Stateful x) = List.map Stateful (Factorial.split (p . Stateful) x)
   splitAt m (Stateful x) = (Stateful xp, Stateful xs)
      where (xp, xs) = splitAt m x

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
   all p = Textual.all p . extract
   any p = Textual.any p . extract

   foldl fx fc a (Stateful (t, x)) = Factorial.foldl f2 (Textual.foldl f1 fc a t) x
      where f1 a = fx a . fromFst
            f2 a = fx a . fromSnd
   foldr fx fc a (Stateful (t, x)) = Textual.foldr (fx . fromFst) fc (Factorial.foldr (fx . fromSnd) a x) t
   foldl' fx fc a (Stateful (t, x)) = a' `seq` Factorial.foldl' f2 a' x
      where a' = Textual.foldl' f1 fc a t
            f1 a = fx a . fromFst
            f2 a = fx a . fromSnd

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
   break pt pc (Stateful (t, x)) = (Stateful (tp, xp), Stateful (ts, xs))
      where (tp, ts) = Textual.break (pt . fromFst) pc t
            (xp, xs) | null ts = Factorial.break (pt . fromSnd) x
                     | otherwise = (mempty, x)
   split p (Stateful (t, x)) = restore id ts
      where ts = Textual.split p t
            restore f [t] = f [Stateful (t, x)]
            restore f (hd:tl) = restore (f . (Stateful (hd, mempty):)) tl
   find p = find p . extract
   elem c = elem c . extract

{-# INLINE fromFst #-}
fromFst :: Monoid b => a -> Stateful b a
fromFst a = Stateful (a, mempty)

{-# INLINE fromSnd #-}
fromSnd :: Monoid a => b -> Stateful b a
fromSnd b = Stateful (mempty, b)
