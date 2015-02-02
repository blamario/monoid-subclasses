{- 
    Copyright 2013-2015 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Concat'.
-- 

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.Concat (
   Concat, concatenate, extract
   )
where

import Prelude hiding (all, any, break, filter, foldl, foldl1, foldMap, foldr, foldr1, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt)
import Control.Applicative (Applicative(..))
import qualified Data.Foldable as Foldable
import Data.String (IsString(..))
import Data.Monoid (Monoid(..), (<>), First(..), Sum(..))
import Data.Monoid.Cancellative (LeftReductiveMonoid(..), RightReductiveMonoid(..),
                                 LeftGCDMonoid(..), RightGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..), StableFactorialMonoid)
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual
import Data.Sequence (Seq, empty, filter, (<|), (|>), ViewL((:<)), ViewR((:>)))
import qualified Data.Sequence as Seq

-- | @'Concat' a@ is a @newtype@ wrapper around @'Seq' a@. The behaviour of the @'Concat' a@ instances of monoid
-- subclasses is identical to the behaviour of their @a@ instances, up to the 'pure' isomorphism.
--
-- The only purpose of 'Concat' then is to change the performance characteristics of various operations. Most
-- importantly, injecting a monoid into a 'Concat' has the effect of making 'mappend' a constant-time operation.
--
newtype Concat a = Concat {extract :: Seq a} deriving Show

concatenate :: (MonoidNull a, PositiveMonoid a) => Seq a -> Concat a
concatenate = Concat . filter (not . null)

instance (Eq a, Monoid a) => Eq (Concat a) where
   Concat x == Concat y = Foldable.foldMap id x == Foldable.foldMap id y

instance (Ord a, Monoid a) => Ord (Concat a) where
   compare (Concat x) (Concat y) = compare (Foldable.foldMap id x) (Foldable.foldMap id y)

instance Functor Concat where
   fmap f (Concat x) = Concat (fmap f x)

instance Applicative Concat where
   pure a = Concat (Seq.singleton a)
   Concat x <*> Concat y = Concat (x <*> y)
   Concat x *> Concat y = Concat (x *> y)

instance Monoid (Concat a) where
   mempty = Concat Seq.empty
   mappend (Concat a) (Concat b) = Concat (mappend a b)

instance MonoidNull (Concat a) where
   null (Concat x) = Seq.null x

instance PositiveMonoid (Concat a)

instance (LeftReductiveMonoid a, MonoidNull a, StableFactorialMonoid a) => LeftReductiveMonoid (Concat a) where
   stripPrefix c1 c2 = fmap Concat $ strip1 (extract c1) (extract c2)
      where strip1 x y = strip2 (Seq.viewl x) y
            strip2 Seq.EmptyL y = Just y
            strip2 (xp :< xs) y = strip3 xp xs (Seq.viewl y)
            strip3 _ _ Seq.EmptyL = Nothing
            strip3 xp xs (yp :< ys) =
               case (stripPrefix xp yp, stripPrefix yp xp)
               of (Just yps, _) -> strip1 xs (if null yps then ys else yps <| ys)
                  (Nothing, Nothing) -> Nothing
                  (Nothing, Just xps) -> strip3 xps xs (Seq.viewl ys)

instance (MonoidNull a, RightReductiveMonoid a, StableFactorialMonoid a) => RightReductiveMonoid (Concat a) where
   stripSuffix c1 c2 = fmap Concat $ strip1 (extract c1) (extract c2)
      where strip1 x y = strip2 (Seq.viewr x) y
            strip2 Seq.EmptyR y = Just y
            strip2 (xp :> xs) y = strip3 xp xs (Seq.viewr y)
            strip3 _ _ Seq.EmptyR = Nothing
            strip3 xp xs (yp :> ys) =
               case (stripSuffix xs ys, stripSuffix ys xs)
               of (Just ysp, _) -> strip1 xp (if null ysp then yp else yp |> ysp)
                  (Nothing, Nothing) -> Nothing
                  (Nothing, Just xsp) -> strip3 xp xsp (Seq.viewr yp)

instance (Eq a, LeftGCDMonoid a, MonoidNull a, StableFactorialMonoid a) => LeftGCDMonoid (Concat a) where
   stripCommonPrefix (Concat x) (Concat y) = strip cp1 xs1 ys1
      where (cp1, xs1, ys1) = stripCommonPrefix x y
            strip cp xs ys =
               case (Seq.viewl xs, Seq.viewl ys)
               of (Seq.EmptyL, _) -> (Concat cp, mempty, Concat ys)
                  (_, Seq.EmptyL) -> (Concat cp, Concat xs, mempty)
                  (xsp :< xss, ysp :< yss) ->
                     let (cs, xsps, ysps) = stripCommonPrefix xsp ysp
                         cp' = cp |> cs
                         prepend p s = if null p then s else p <| s
                     in if null cs
                        then (Concat cp, Concat xs, Concat ys)
                        else if null xsps && null ysps
                             then strip cp' xss yss
                             else (Concat cp', Concat $ prepend xsps xss, Concat $ prepend ysps yss)

instance (Eq a, RightGCDMonoid a, MonoidNull a, StableFactorialMonoid a) => RightGCDMonoid (Concat a) where
   stripCommonSuffix (Concat x) (Concat y) = strip xp1 yp1 cs1
      where (xp1, yp1, cs1) = stripCommonSuffix x y
            strip xp yp cs =
               case (Seq.viewr xp, Seq.viewr yp)
               of (Seq.EmptyR, _) -> (mempty, Concat yp, Concat cs)
                  (_, Seq.EmptyR) -> (Concat xp, mempty, Concat cs)
                  (xpp :> xps, ypp :> yps) ->
                     let (xpsp, ypsp, cp) = stripCommonSuffix xps yps
                         cs' = cp <| cs
                         append p s = if null s then p else p |> s
                     in if null cp
                        then (Concat xp, Concat yp, Concat cs)
                        else if null xpsp && null ypsp
                             then strip xpp ypp cs'
                             else (Concat $ append xpp xpsp, Concat $ append ypp ypsp, Concat cs')

instance FactorialMonoid a => FactorialMonoid (Concat a) where
   factors (Concat x) = Foldable.foldMap (fmap (Concat . Seq.singleton) . factors) x
   primePrefix (Concat x) = Concat (fmap primePrefix $ primePrefix x)
   primeSuffix (Concat x) = Concat (fmap primeSuffix $ primeSuffix x)
   splitPrimePrefix (Concat x) =
      case Seq.viewl x
           of Seq.EmptyL -> Nothing
              xp :< xs -> Just (Concat $ Seq.singleton xpp, Concat xs')
                 where Just (xpp, xps) = splitPrimePrefix xp
                       xs' = if null xps then xs else xps <| xs
   splitPrimeSuffix (Concat x) =
      case Seq.viewr x
           of Seq.EmptyR -> Nothing
              xp :> xs -> Just (Concat xp', Concat $ Seq.singleton xss)
                 where Just (xsp, xss) = splitPrimeSuffix xs
                       xp' = if null xsp then xp else xp |> xsp
   foldl f a0 (Concat x) = Foldable.foldl g a0 x
      where g = Factorial.foldl (\a-> f a . Concat . Seq.singleton)
   foldl' f a0 (Concat x) = Foldable.foldl' g a0 x
      where g = Factorial.foldl' (\a-> f a . Concat . Seq.singleton)
   foldr f a0 (Concat x) = Foldable.foldr g a0 x
      where g a b = Factorial.foldr (f . Concat . Seq.singleton) b a
   length (Concat x) = getSum $ Foldable.foldMap (Sum . length) x
   foldMap f (Concat x) = Foldable.foldMap (foldMap (f . Concat . Seq.singleton)) x
   span p (Concat x) =
      case Seq.viewl x
      of Seq.EmptyL -> (mempty, mempty)
         xp :< xs | null xps -> (Concat (xp <| xsp), xss)
                  | null xpp -> (mempty, Concat x)
                  | otherwise -> (Concat $ Seq.singleton xpp, Concat (xps <| xs))
            where (xpp, xps) = Factorial.span (p . Concat . Seq.singleton) xp
                  (Concat xsp, xss) = Factorial.span p (Concat xs)
   split p (Concat x) = Foldable.foldr splitNext [mempty] x
      where splitNext a ~(xp:xs) =
               let as = fmap (Concat . Seq.singleton) (Factorial.split (p . Concat . Seq.singleton) a)
               in if null xp
                  then as ++ xs
                  else init as ++ (last as <> xp):xs
   splitAt 0 c = (mempty, c)
   splitAt n (Concat x) =
      case Seq.viewl x
      of Seq.EmptyL -> (mempty, mempty)
         xp :< xs | k < n -> (Concat (xp <| xsp), xss)
                  | otherwise -> (Concat $ Seq.singleton xpp, Concat (if null xps then xs else xps <| xs))
            where k = length xp
                  (Concat xsp, xss) = splitAt (n - k) (Concat xs)
                  (xpp, xps) = splitAt n xp
   reverse (Concat x) = Concat (fmap reverse $ reverse x)


instance (IsString a) => IsString (Concat a) where
   fromString "" = Concat empty
   fromString s = Concat (Seq.singleton $ fromString s)

instance (Eq a, TextualMonoid a, StableFactorialMonoid a) => TextualMonoid (Concat a) where
   fromText t | null t = Concat empty
              | otherwise = Concat (Seq.singleton $ fromText t)
   singleton = Concat . Seq.singleton . singleton
   splitCharacterPrefix (Concat x) =
      case Seq.viewl x
      of Seq.EmptyL -> Nothing
         xp :< xs -> case splitCharacterPrefix xp
                     of Just (c, xps) -> Just (c, Concat $ if null xps then xs else xps <| xs)
                        Nothing -> Nothing
   characterPrefix (Concat x) =
      case Seq.viewl x
      of Seq.EmptyL -> Nothing
         xp :< _ -> characterPrefix xp
   map f (Concat x) = Concat (fmap (map f) x)
   any p (Concat x) = Foldable.any (any p) x
   all p (Concat x) = Foldable.all (all p) x

   foldl ft fc a0 (Concat x) = Foldable.foldl g a0 x
      where g = Textual.foldl (\a-> ft a . Concat . Seq.singleton) fc
   foldl' ft fc a0 (Concat x) = Foldable.foldl' g a0 x
      where g = Textual.foldl' (\a-> ft a . Concat . Seq.singleton) fc
   foldr ft fc a0 (Concat x) = Foldable.foldr g a0 x
      where g a b = Textual.foldr (ft . Concat . Seq.singleton) fc b a

   span pt pc (Concat x) =
      case Seq.viewl x
      of Seq.EmptyL -> (mempty, mempty)
         xp :< xs | null xps -> (Concat (xp <| xsp), xss)
                  | null xpp -> (mempty, Concat x)
                  | otherwise -> (Concat $ Seq.singleton xpp, Concat (xps <| xs))
            where (xpp, xps) = Textual.span (pt . Concat . Seq.singleton) pc xp
                  (Concat xsp, xss) = Textual.span pt pc (Concat xs)
   break pt pc = Textual.span (not . pt) (not . pc)

   find p (Concat x) = getFirst $ Foldable.foldMap (First . find p) x
