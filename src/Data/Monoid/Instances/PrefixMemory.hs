{-
    Copyright 2023 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the monoid transformer data type 'Shadowed'.
{-# LANGUAGE Haskell2010, DeriveDataTypeable #-}

module Data.Monoid.Instances.PrefixMemory (
   Shadowed, shadowed, content, prefix
   )
where

import Control.Applicative -- (Applicative(..))
import qualified Data.List as List
import Data.String (IsString(fromString))

import Data.Data (Data, Typeable)
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..), Endo(..))
import Data.Semigroup.Cancellative (LeftReductive(..), RightReductive(..))
import Data.Semigroup.Factorial (Factorial(..), StableFactorial)
import Data.Monoid.GCD (LeftGCDMonoid(..), RightGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Semigroup.Factorial as Factorial
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual

import Prelude hiding (all, any, break, filter, foldl, foldl1, foldr, foldr1, lines, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt)

-- | Monoid transformer that keeps track of the former 'prefix' of its 'content'. All functions that return a suffix
-- of their argument, such as 'stripPrefix' or 'commonSuffix', preserve the discarded 'prefix'.
data Shadowed m = Shadowed{prefix :: !m,
                           -- ^ used to precede the 'content' but has been consumed
                           content :: !m
                           -- ^ the present value
                          } deriving (Data, Typeable)

-- | The constructor of a 'Shadowed' monoid, with the initial @prefix = null@
shadowed :: Monoid m => m -> Shadowed m
shadowed = Shadowed mempty

instance Eq m => Eq (Shadowed m) where
   Shadowed{content = a} == Shadowed{content = b} = a == b

instance Ord m => Ord (Shadowed m) where
   compare Shadowed{content= a} Shadowed{content= b} = compare a b

instance (MonoidNull m, Show m) => Show (Shadowed m) where
   showsPrec prec (Shadowed p c) rest
      | null p = showsPrec prec c rest
      | otherwise = "Shadowed{prefix=" <> shows p (", content=" <> shows c ("}" <> rest))

instance (MonoidNull m, StableFactorial m) => Semigroup (Shadowed m) where
   Shadowed p1 c1 <> m2@Shadowed{content = c2}
      | null c1 && null p1 = m2
      | otherwise = Shadowed p1 (c1 <> c2)
   {-# INLINE (<>) #-}

instance (MonoidNull m, StableFactorial m) => Monoid (Shadowed m) where
   mempty = shadowed mempty
   mappend = (<>)
   {-# INLINE mempty #-}
   {-# INLINE mappend #-}

instance (MonoidNull m, StableFactorial m) => MonoidNull (Shadowed m) where
   null = null . content
   {-# INLINE null #-}

instance (PositiveMonoid m, StableFactorial m) => PositiveMonoid (Shadowed m)

instance (MonoidNull m, StableFactorial m, LeftReductive m) => LeftReductive (Shadowed m) where
   t1 `isPrefixOf` t2 = content t1 `isPrefixOf` content t2
   stripPrefix (Shadowed _ c1) (Shadowed p c2) = fmap (Shadowed (p <> c1)) (stripPrefix c1 c2)
   {-# INLINE isPrefixOf #-}
   {-# INLINE stripPrefix #-}

instance (Eq m, StableFactorial m, FactorialMonoid m, LeftGCDMonoid m) => LeftGCDMonoid (Shadowed m) where
   stripCommonPrefix (Shadowed p1 c1) (Shadowed p2 c2) =
      (Shadowed prefix' common, Shadowed (p1 <> common) c1', Shadowed (p2 <> common) c2')
      where (common, c1', c2') = stripCommonPrefix c1 c2
            prefix' = if p1 == p2 then p1 <> common else common
   {-# INLINE stripCommonPrefix #-}

instance (StableFactorial m, FactorialMonoid m, RightReductive m) => RightReductive (Shadowed m) where
   isSuffixOf (Shadowed _ c1) (Shadowed _ c2) = isSuffixOf c1 c2
   stripSuffix (Shadowed _ c1) (Shadowed p c2) = fmap (Shadowed p) (stripSuffix c1 c2)
   {-# INLINE isSuffixOf #-}
   {-# INLINE stripSuffix #-}

instance (StableFactorial m, FactorialMonoid m, RightGCDMonoid m) => RightGCDMonoid (Shadowed m) where
   commonSuffix (Shadowed _ c1) (Shadowed _ c2) = shadowed suffix
      where suffix = commonSuffix c1 c2
   stripCommonSuffix (Shadowed p1 c1) (Shadowed p2 c2) =
      (Shadowed p1 c1', Shadowed p2 c2',
       shadowed suffix)
      where (c1', c2', suffix) = stripCommonSuffix c1 c2
   {-# INLINE commonSuffix #-}
   {-# INLINE stripCommonSuffix #-}

instance (FactorialMonoid m, StableFactorial m) => Factorial (Shadowed m) where
   factors (Shadowed p c) = rewrap <$> List.tail (inits c)
      where rewrap t
               | Just (p', prime) <- splitPrimeSuffix t = Shadowed (p <> p') prime
               | otherwise = error "all (not . null) . tail . inits"
   primePrefix (Shadowed p c) = Shadowed p (primePrefix c)
   foldl f a0 (Shadowed p0 c0) = fst $ Factorial.foldl f' (a0, p0) c0
      where f' (a, p) c = (f a (Shadowed p c), p <> c)
   foldl' f a0 (Shadowed p0 c0) = fst $ Factorial.foldl' f' (a0, p0) c0
      where f' (a, p) c = ((,) $! f a (Shadowed p c)) $! p <> c
   foldr f a0 (Shadowed p0 c0) = Factorial.foldr f' (const a0) c0 p0
      where f' c cont p = f (Shadowed p c) (cont $! p <> c)
   foldMap f (Shadowed p0 c) = appEndo (Factorial.foldMap f' c) (const mempty) p0
      where -- f' :: m -> Endo (Int -> m)
            f' prime = Endo (\cont p-> f (Shadowed p prime) `mappend` (cont $! p <> prime))
   length (Shadowed _ c) = length c
   reverse (Shadowed p c) = Shadowed p (Factorial.reverse c)
   {-# INLINE primePrefix #-}
   {-# INLINE foldl #-}
   {-# INLINE foldl' #-}
   {-# INLINE foldr #-}
   {-# INLINE foldMap #-}

instance (StableFactorial m, FactorialMonoid m) => FactorialMonoid (Shadowed m) where
   splitPrimePrefix (Shadowed p c) = fmap rewrap (splitPrimePrefix c)
      where rewrap (cp, cs) = (Shadowed p cp, Shadowed (p <> cp) cs)
   splitPrimeSuffix (Shadowed p c) = fmap rewrap (splitPrimeSuffix c)
      where rewrap (cp, cs) = (Shadowed p cp, Shadowed (p <> cp) cs)
   inits (Shadowed p c) = Shadowed p <$> inits c
   tails (Shadowed p c)
      | null p = zipWith Shadowed (inits c) (tails c)
      | otherwise = zipWith (Shadowed . (p <>)) (inits c) (tails c)
   spanMaybe s0 f (Shadowed p0 c) = rewrap $ Factorial.spanMaybe (s0, p0) f' c
      where f' (s, p) prime = do s' <- f s (Shadowed p prime)
                                 let p' = p <> prime
                                 Just $! seq p' (s', p')
            rewrap (cp, cs, (s, p)) = (Shadowed p0 cp, Shadowed p cs, s)
   spanMaybe' s0 f (Shadowed p0 c) = rewrap $! Factorial.spanMaybe' (s0, p0) f' c
      where f' (s, p) prime = do s' <- f s (Shadowed p prime)
                                 let p' = p <> prime
                                 Just $! s' `seq` p' `seq` (s', p')
            rewrap (cp, cs, (s, p)) = (Shadowed p0 cp, Shadowed p cs, s)
   span f (Shadowed p0 c) = rewrap $ Factorial.spanMaybe' p0 f' c
      where f' p prime = if f (Shadowed p prime)
                         then Just $! p <> prime
                         else Nothing
            rewrap (cp, cs, p) = (Shadowed p0 cp, Shadowed p cs)
   splitAt n (Shadowed p c) = (Shadowed p cp, Shadowed (p <> cp) cs)
      where (cp, cs) = splitAt n c
   take n (Shadowed p c) = Shadowed p (Factorial.take n c)
   {-# INLINE splitPrimePrefix #-}
   {-# INLINE splitPrimeSuffix #-}
   {-# INLINE span #-}
   {-# INLINE splitAt #-}
   {-# INLINE take #-}

instance (StableFactorial m, FactorialMonoid m) => StableFactorial (Shadowed m)

instance (Monoid m, IsString m) => IsString (Shadowed m) where
   fromString = shadowed . fromString

instance (Eq m, StableFactorial m, TextualMonoid m) => TextualMonoid (Shadowed m) where
   splitCharacterPrefix (Shadowed p t) = (Shadowed p <$>) <$> Textual.splitCharacterPrefix t

   fromText = shadowed . fromText
   singleton = shadowed . singleton

   characterPrefix = characterPrefix . content

   map f (Shadowed p c) = Shadowed p (map f c)
   concatMap f (Shadowed p c) = Shadowed p (concatMap (content . f) c)
   all p = all p . content
   any p = any p . content

   foldl ft fc a0 (Shadowed p0 c0) = fst $ Textual.foldl ft' fc' (a0, p0) c0
      where ft' (a, p) c = (ft a (Shadowed p c), p <> c)
            fc' (a, p) c = (fc a c, p <> Textual.singleton c)
   foldl' ft fc a0 (Shadowed p0 c0) = fst $ Textual.foldl' ft' fc' (a0, p0) c0
      where ft' (a, p) c = ((,) $! ft a (Shadowed p c)) $! p <> c
            fc' (a, p) c = ((,) $! fc a c) $! p <> Textual.singleton c
   foldr ft fc a0 (Shadowed p0 c0) = snd $ Textual.foldr ft' fc' (p0, a0) c0
      where ft' c (p, a) = ((,) $! p <> c) $! ft (Shadowed p c) a
            fc' c (p, a) = ((,) $! p <> Textual.singleton c) $! fc c a

   scanl f ch (Shadowed p c) = Shadowed p (Textual.scanl f ch c)
   scanl1 f (Shadowed p c) = Shadowed p (Textual.scanl1 f c)
   scanr f ch (Shadowed p c) = Shadowed p (Textual.scanr f ch c)
   scanr1 f (Shadowed p c) = Shadowed p (Textual.scanr1 f c)
   mapAccumL f a0 (Shadowed p c) = fmap (Shadowed p) (Textual.mapAccumL f a0 c)
   mapAccumR f a0 (Shadowed p c) = fmap (Shadowed p) (Textual.mapAccumR f a0 c)

   spanMaybe s0 ft fc (Shadowed p0 t) = rewrap $ Textual.spanMaybe (s0, p0) ft' fc' t
      where ft' (s, p) prime = do s' <- ft s (Shadowed p prime)
                                  let p' = p <> prime
                                  Just $! seq p' (s', p')
            fc' (s, p) c = do s' <- fc s c
                              let p' = p <> Textual.singleton c
                              Just $! seq p' (s', p')
            rewrap (tp, ts, (s, p)) = (Shadowed p0 tp, Shadowed p ts, s)
   spanMaybe' s0 ft fc (Shadowed p0 t) = rewrap $! Textual.spanMaybe' (s0, p0) ft' fc' t
      where ft' (s, p) prime = do s' <- ft s (Shadowed p prime)
                                  let p' = p <> prime
                                  Just $! s' `seq` p' `seq` (s', p')
            fc' (s, p) c = do s' <- fc s c
                              let p' = p <> Textual.singleton c
                              Just $! s' `seq` p' `seq` (s', p')
            rewrap (tp, ts, (s, p)) = (Shadowed p0 tp, Shadowed p ts, s)
   span ft fc (Shadowed p0 t) = rewrap $ Textual.spanMaybe' p0 ft' fc' t
      where ft' p prime = if ft (Shadowed p prime)
                          then Just $! p <> prime
                          else Nothing
            fc' p c = if fc c
                      then Just $! p <> Textual.singleton c
                      else Nothing
            rewrap (tp, ts, p) = (Shadowed p0 tp, Shadowed p ts)

   split f (Shadowed p0 c0) = rewrap p0 (Textual.split f c0)
      where rewrap _ [] = []
            rewrap p (c:rest) = Shadowed p c : rewrap (p <> c) rest
   find p = find p . content

   foldl_ fc a0 (Shadowed _ c) = Textual.foldl_ fc a0 c
   foldl_' fc a0 (Shadowed _ c) = Textual.foldl_' fc a0 c
   foldr_ fc a0 (Shadowed _ c) = Textual.foldr_ fc a0 c

   spanMaybe_ s0 fc (Shadowed p0 t) = rewrap $ Textual.spanMaybe_' (s0, p0) fc' t
      where fc' (s, p) c = do s' <- fc s c
                              let p' = p <> Textual.singleton c
                              Just $! seq p' (s', p')
            rewrap (tp, ts, (s, p)) = (Shadowed p0 tp, Shadowed p ts, s)
   spanMaybe_' s0 fc (Shadowed p0 t) = rewrap $! Textual.spanMaybe_' (s0, p0) fc' t
      where fc' (s, p) c = do s' <- fc s c
                              let p' = p <> Textual.singleton c
                              Just $! s' `seq` p' `seq` (s', p')
            rewrap (tp, ts, (s, p)) = (Shadowed p0 tp, Shadowed p ts, s)
   span_ bt fc (Shadowed p0 t) = rewrap $ Textual.span_ bt fc t
      where rewrap (tp, ts) = (Shadowed p0 tp, Shadowed (p0 <> tp) ts)
   break_ bt fc (Shadowed p0 t) = rewrap $ Textual.break_ bt fc t
      where rewrap (tp, ts) = (Shadowed p0 tp, Shadowed (p0 <> tp) ts)
   dropWhile_ bt fc t = snd (span_ bt fc t)
   takeWhile_ bt fc (Shadowed p t) = Shadowed p (takeWhile_ bt fc t)
   toString ft (Shadowed _ t) = toString (ft . shadowed) t
   toText ft (Shadowed _ t) = toText (ft . shadowed) t

   {-# INLINE characterPrefix #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE map #-}
   {-# INLINE concatMap #-}
   {-# INLINE foldl' #-}
   {-# INLINE foldr #-}
   {-# INLINABLE spanMaybe #-}
   {-# INLINABLE spanMaybe' #-}
   {-# INLINABLE span #-}
   {-# INLINE foldl_' #-}
   {-# INLINE foldr_ #-}
   {-# INLINE any #-}
   {-# INLINE all #-}
   {-# INLINABLE spanMaybe_ #-}
   {-# INLINABLE spanMaybe_' #-}
   {-# INLINE span_ #-}
   {-# INLINE break_ #-}
   {-# INLINE dropWhile_ #-}
   {-# INLINE takeWhile_ #-}
   {-# INLINE split #-}
   {-# INLINE find #-}
