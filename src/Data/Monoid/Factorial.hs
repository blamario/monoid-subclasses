{- 
    Copyright 2013-2017 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'FactorialMonoid' class and some of its instances.
-- 

{-# LANGUAGE Haskell2010, ConstraintKinds, FlexibleInstances, Trustworthy #-}

module Data.Monoid.Factorial (
   module Data.Semigroup.Factorial,
   FactorialMonoid(..), StableFactorialMonoid,
   )
where

import Control.Arrow (first)
import Data.Monoid -- (Monoid (..), Dual(..), Sum(..), Product(..), Endo(Endo, appEndo))
import qualified Data.Foldable as Foldable
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
import Data.Int (Int64)

import Data.Semigroup.Factorial
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)

import Prelude hiding (break, drop, dropWhile, foldl, foldr, last, length, map, max, min,
                       null, reverse, span, splitAt, take, takeWhile)


-- | Class of monoids that can be split into irreducible (/i.e./, atomic or prime) 'factors' in a unique way. Note that
-- 'mempty' is not considered a factor. Factors of a 'Product' are literally its prime factors:
--
-- prop> factors (Product 12) == [Product 2, Product 2, Product 3]
--
-- Factors of a list are /not/ its elements but all its single-item sublists:
--
-- prop> factors "abc" == ["a", "b", "c"]
-- 
-- The methods of this class satisfy the following laws in addition to those of 'Factorial':
-- 
-- > null == List.null . factors
-- > factors == unfoldr splitPrimePrefix == List.reverse . unfoldr (fmap swap . splitPrimeSuffix)
-- > reverse == mconcat . List.reverse . factors
-- > primePrefix == maybe mempty fst . splitPrimePrefix
-- > primeSuffix == maybe mempty snd . splitPrimeSuffix
-- > inits == List.map mconcat . List.inits . factors
-- > tails == List.map mconcat . List.tails . factors
-- > span p m == (mconcat l, mconcat r) where (l, r) = List.span p (factors m)
-- > List.all (List.all (not . pred) . factors) . split pred
-- > mconcat . intersperse prime . split (== prime) == id
-- > splitAt i m == (mconcat l, mconcat r) where (l, r) = List.splitAt i (factors m)
-- > spanMaybe () (const $ bool Nothing (Maybe ()) . p) m == (takeWhile p m, dropWhile p m, ())
-- > spanMaybe s0 (\s m-> Just $ f s m) m0 == (m0, mempty, foldl f s0 m0)
-- > let (prefix, suffix, s') = spanMaybe s f m
-- >     foldMaybe = foldl g (Just s)
-- >     g s m = s >>= flip f m
-- > in all ((Nothing ==) . foldMaybe) (inits prefix)
-- >    && prefix == last (filter (isJust . foldMaybe) $ inits m)
-- >    && Just s' == foldMaybe prefix
-- >    && m == prefix <> suffix
--
-- A minimal instance definition should implement 'splitPrimePrefix' for performance reasons, and other methods where
-- beneficial.
class (Factorial m, MonoidNull m) => FactorialMonoid m where
   -- | Splits the argument into its prime prefix and the remaining suffix. Returns 'Nothing' for 'mempty'.
   splitPrimePrefix :: m -> Maybe (m, m)
   -- | Splits the argument into its prime suffix and the remaining prefix. Returns 'Nothing' for 'mempty'.
   splitPrimeSuffix :: m -> Maybe (m, m)
   -- | Returns the list of all prefixes of the argument, 'mempty' first.
   inits :: m -> [m]
   -- | Returns the list of all suffixes of the argument, 'mempty' last.
   tails :: m -> [m]
   -- | Like 'List.span' from "Data.List" on the list of prime 'factors'.
   span :: (m -> Bool) -> m -> (m, m)
   -- | Equivalent to 'List.break' from "Data.List".
   break :: (m -> Bool) -> m -> (m, m)
   -- | Splits the monoid into components delimited by prime separators satisfying the given predicate. The primes
   -- satisfying the predicate are not a part of the result.
   split :: (m -> Bool) -> m -> [m]
   -- | Equivalent to 'List.takeWhile' from "Data.List".
   takeWhile :: (m -> Bool) -> m -> m
   -- | Equivalent to 'List.dropWhile' from "Data.List".
   dropWhile :: (m -> Bool) -> m -> m
   -- | A stateful variant of 'span', threading the result of the test function as long as it returns 'Just'.
   spanMaybe :: s -> (s -> m -> Maybe s) -> m -> (m, m, s)
   -- | Strict version of 'spanMaybe'.
   spanMaybe' :: s -> (s -> m -> Maybe s) -> m -> (m, m, s)
   -- | Like 'List.splitAt' from "Data.List" on the list of prime 'factors'.
   splitAt :: Int -> m -> (m, m)
   -- | Equivalent to 'List.drop' from "Data.List".
   drop :: Int -> m -> m
   -- | Equivalent to 'List.take' from "Data.List".
   take :: Int -> m -> m

   splitPrimePrefix x = case factors x
                        of [] -> Nothing
                           prefix : rest -> Just (prefix, mconcat rest)
   splitPrimeSuffix x = case factors x
                        of [] -> Nothing
                           fs -> Just (mconcat (List.init fs), List.last fs)
   inits = foldr (\m l-> mempty : List.map (mappend m) l) [mempty]
   tails m = m : maybe [] (tails . snd) (splitPrimePrefix m)
   span p m0 = spanAfter id m0
      where spanAfter f m = case splitPrimePrefix m
                            of Just (prime, rest) | p prime -> spanAfter (f . mappend prime) rest
                               _ -> (f mempty, m)
   break = span . (not .)
   spanMaybe s0 f m0 = spanAfter id s0 m0
      where spanAfter g s m = case splitPrimePrefix m
                              of Just (prime, rest) | Just s' <- f s prime -> spanAfter (g . mappend prime) s' rest
                                                    | otherwise -> (g mempty, m, s)
                                 Nothing -> (m0, m, s)
   spanMaybe' s0 f m0 = spanAfter id s0 m0
      where spanAfter g s m = seq s $
                              case splitPrimePrefix m
                              of Just (prime, rest) | Just s' <- f s prime -> spanAfter (g . mappend prime) s' rest
                                                    | otherwise -> (g mempty, m, s)
                                 Nothing -> (m0, m, s)
   split p m = prefix : splitRest
      where (prefix, rest) = break p m
            splitRest = case splitPrimePrefix rest
                        of Nothing -> []
                           Just (_, tl) -> split p tl
   takeWhile p = fst . span p
   dropWhile p = snd . span p
   splitAt n0 m0 | n0 <= 0 = (mempty, m0)
                 | otherwise = split' n0 id m0
      where split' 0 f m = (f mempty, m)
            split' n f m = case splitPrimePrefix m
                           of Nothing -> (f mempty, m)
                              Just (prime, rest) -> split' (pred n) (f . mappend prime) rest
   drop n p = snd (splitAt n p)
   take n p = fst (splitAt n p)
   {-# MINIMAL #-}

{-# DEPRECATED StableFactorialMonoid "Use Data.Semigroup.Factorial.StableFactorial instead." #-}
type StableFactorialMonoid m = (StableFactorial m, FactorialMonoid m, PositiveMonoid m)

instance FactorialMonoid () where
   splitPrimePrefix () = Nothing
   splitPrimeSuffix () = Nothing

instance FactorialMonoid a => FactorialMonoid (Dual a) where
   splitPrimePrefix (Dual a) = case splitPrimeSuffix a
                               of Nothing -> Nothing
                                  Just (p, s) -> Just (Dual s, Dual p)
   splitPrimeSuffix (Dual a) = case splitPrimePrefix a
                               of Nothing -> Nothing
                                  Just (p, s) -> Just (Dual s, Dual p)
   inits (Dual a) = fmap Dual (reverse $ tails a)
   tails (Dual a) = fmap Dual (reverse $ inits a)

instance (Integral a, Eq a) => FactorialMonoid (Sum a) where
   splitPrimePrefix (Sum 0) = Nothing
   splitPrimePrefix (Sum a) = Just (Sum (signum a), Sum (a - signum a))
   splitPrimeSuffix (Sum 0) = Nothing
   splitPrimeSuffix (Sum a) = Just (Sum (a - signum a), Sum (signum a))

instance Integral a => FactorialMonoid (Product a)

instance FactorialMonoid a => FactorialMonoid (Maybe a) where
   splitPrimePrefix Nothing = Nothing
   splitPrimePrefix (Just a) = case splitPrimePrefix a
                               of Nothing -> Just (Just a, Nothing)
                                  Just (p, s) -> Just (Just p, if null s then Nothing else Just s)


instance (FactorialMonoid a, FactorialMonoid b) => FactorialMonoid (a, b) where
   splitPrimePrefix (a, b) = case (splitPrimePrefix a, splitPrimePrefix b)
                             of (Just (ap, as), _) -> Just ((ap, mempty), (as, b))
                                (Nothing, Just (bp, bs)) -> Just ((a, bp), (a, bs))
                                (Nothing, Nothing) -> Nothing
   splitPrimeSuffix (a, b) = case (splitPrimeSuffix a, splitPrimeSuffix b)
                             of (_, Just (bp, bs)) -> Just ((a, bp), (mempty, bs))
                                (Just (ap, as), Nothing) -> Just ((ap, b), (as, b))
                                (Nothing, Nothing) -> Nothing
   inits (a, b) = List.map (flip (,) mempty) (inits a) ++ List.map ((,) a) (List.tail $ inits b)
   tails (a, b) = List.map (flip (,) b) (tails a) ++ List.map ((,) mempty) (List.tail $ tails b)
   span p (x, y) = ((xp, yp), (xs, ys))
      where (xp, xs) = span (p . fromFst) x
            (yp, ys) | null xs = span (p . fromSnd) y
                     | otherwise = (mempty, y)
   spanMaybe s0 f (x, y) | null xs = ((xp, yp), (xs, ys), s2)
                         | otherwise = ((xp, mempty), (xs, y), s1)
     where (xp, xs, s1) = spanMaybe s0 (\s-> f s . fromFst) x
           (yp, ys, s2) = spanMaybe s1 (\s-> f s . fromSnd) y
   spanMaybe' s0 f (x, y) | null xs = ((xp, yp), (xs, ys), s2)
                          | otherwise = ((xp, mempty), (xs, y), s1)
     where (xp, xs, s1) = spanMaybe' s0 (\s-> f s . fromFst) x
           (yp, ys, s2) = spanMaybe' s1 (\s-> f s . fromSnd) y
   split p (x0, y0) = fst $ List.foldr combine (ys, False) xs
      where xs = List.map fromFst $ split (p . fromFst) x0
            ys = List.map fromSnd $ split (p . fromSnd) y0
            combine x (~(y:rest), False) = (mappend x y : rest, True)
            combine x (rest, True) = (x:rest, True)
   splitAt n (x, y) = ((xp, yp), (xs, ys))
      where (xp, xs) = splitAt n x
            (yp, ys) | null xs = splitAt (n - length x) y
                     | otherwise = (mempty, y)

{-# INLINE fromFst #-}
fromFst :: Monoid b => a -> (a, b)
fromFst a = (a, mempty)

{-# INLINE fromSnd #-}
fromSnd :: Monoid a => b -> (a, b)
fromSnd b = (mempty, b)

instance (FactorialMonoid a, FactorialMonoid b, FactorialMonoid c) => FactorialMonoid (a, b, c) where
   splitPrimePrefix (a, b, c) = case (splitPrimePrefix a, splitPrimePrefix b, splitPrimePrefix c)
                                of (Just (ap, as), _, _) -> Just ((ap, mempty, mempty), (as, b, c))
                                   (Nothing, Just (bp, bs), _) -> Just ((a, bp, mempty), (a, bs, c))
                                   (Nothing, Nothing, Just (cp, cs)) -> Just ((a, b, cp), (a, b, cs))
                                   (Nothing, Nothing, Nothing) -> Nothing
   splitPrimeSuffix (a, b, c) = case (splitPrimeSuffix a, splitPrimeSuffix b, splitPrimeSuffix c)
                                of (_, _, Just (cp, cs)) -> Just ((a, b, cp), (mempty, mempty, cs))
                                   (_, Just (bp, bs), Nothing) -> Just ((a, bp, c), (mempty, bs, c))
                                   (Just (ap, as), Nothing, Nothing) -> Just ((ap, b, c), (as, b, c))
                                   (Nothing, Nothing, Nothing) -> Nothing
   inits (a, b, c) = List.map (\a1-> (a1, mempty, mempty)) (inits a)
                     ++ List.map (\b1-> (a, b1, mempty)) (List.tail $ inits b)
                     ++ List.map (\c1-> (a, b, c1)) (List.tail $ inits c)
   tails (a, b, c) = List.map (\a1-> (a1, b, c)) (tails a)
                     ++ List.map (\b1-> (mempty, b1, c)) (List.tail $ tails b)
                     ++ List.map (\c1-> (mempty, mempty, c1)) (List.tail $ tails c)
   span p (a, b, c) = ((ap, bp, cp), (as, bs, cs))
      where (ap, as) = span (p . fromFstOf3) a
            (bp, bs) | null as = span (p . fromSndOf3) b
                     | otherwise = (mempty, b)
            (cp, cs) | null as && null bs = span (p . fromThdOf3) c
                     | otherwise = (mempty, c)
   spanMaybe s0 f (a, b, c) | not (null as) = ((ap, mempty, mempty), (as, b, c), s1)
                            | not (null bs) = ((ap, bp, mempty), (as, bs, c), s2)
                            | otherwise = ((ap, bp, cp), (as, bs, cs), s3)
     where (ap, as, s1) = spanMaybe s0 (\s-> f s . fromFstOf3) a
           (bp, bs, s2) = spanMaybe s1 (\s-> f s . fromSndOf3) b
           (cp, cs, s3) = spanMaybe s2 (\s-> f s . fromThdOf3) c
   spanMaybe' s0 f (a, b, c) | not (null as) = ((ap, mempty, mempty), (as, b, c), s1)
                             | not (null bs) = ((ap, bp, mempty), (as, bs, c), s2)
                             | otherwise = ((ap, bp, cp), (as, bs, cs), s3)
     where (ap, as, s1) = spanMaybe' s0 (\s-> f s . fromFstOf3) a
           (bp, bs, s2) = spanMaybe' s1 (\s-> f s . fromSndOf3) b
           (cp, cs, s3) = spanMaybe' s2 (\s-> f s . fromThdOf3) c
   splitAt n (a, b, c) = ((ap, bp, cp), (as, bs, cs))
      where (ap, as) = splitAt n a
            (bp, bs) | null as = splitAt (n - length a) b
                     | otherwise = (mempty, b)
            (cp, cs) | null as && null bs = splitAt (n - length a - length b) c
                     | otherwise = (mempty, c)

{-# INLINE fromFstOf3 #-}
fromFstOf3 :: (Monoid b, Monoid c) => a -> (a, b, c)
fromFstOf3 a = (a, mempty, mempty)

{-# INLINE fromSndOf3 #-}
fromSndOf3 :: (Monoid a, Monoid c) => b -> (a, b, c)
fromSndOf3 b = (mempty, b, mempty)

{-# INLINE fromThdOf3 #-}
fromThdOf3 :: (Monoid a, Monoid b) => c -> (a, b, c)
fromThdOf3 c = (mempty, mempty, c)

instance (FactorialMonoid a, FactorialMonoid b, FactorialMonoid c, FactorialMonoid d) =>
         FactorialMonoid (a, b, c, d) where
   splitPrimePrefix (a, b, c, d) = case (splitPrimePrefix a, splitPrimePrefix b, splitPrimePrefix c, splitPrimePrefix d)
                                   of (Just (ap, as), _, _, _) -> Just ((ap, mempty, mempty, mempty), (as, b, c, d))
                                      (Nothing, Just (bp, bs), _, _) -> Just ((a, bp, mempty, mempty), (a, bs, c, d))
                                      (Nothing, Nothing, Just (cp, cs), _) -> Just ((a, b, cp, mempty), (a, b, cs, d))
                                      (Nothing, Nothing, Nothing, Just (dp, ds)) -> Just ((a, b, c, dp), (a, b, c, ds))
                                      (Nothing, Nothing, Nothing, Nothing) -> Nothing
   splitPrimeSuffix (a, b, c, d) = case (splitPrimeSuffix a, splitPrimeSuffix b, splitPrimeSuffix c, splitPrimeSuffix d)
                                   of (_, _, _, Just (dp, ds)) -> Just ((a, b, c, dp), (mempty, mempty, mempty, ds))
                                      (_, _, Just (cp, cs), Nothing) -> Just ((a, b, cp, d), (mempty, mempty, cs, d))
                                      (_, Just (bp, bs), Nothing, Nothing) -> Just ((a, bp, c, d), (mempty, bs, c, d))
                                      (Just (ap, as), Nothing, Nothing, Nothing) -> Just ((ap, b, c, d), (as, b, c, d))
                                      (Nothing, Nothing, Nothing, Nothing) -> Nothing
   inits (a, b, c, d) = List.map (\a1-> (a1, mempty, mempty, mempty)) (inits a)
                        ++ List.map (\b1-> (a, b1, mempty, mempty)) (List.tail $ inits b)
                        ++ List.map (\c1-> (a, b, c1, mempty)) (List.tail $ inits c)
                        ++ List.map (\d1-> (a, b, c, d1)) (List.tail $ inits d)
   tails (a, b, c, d) = List.map (\a1-> (a1, b, c, d)) (tails a)
                        ++ List.map (\b1-> (mempty, b1, c, d)) (List.tail $ tails b)
                        ++ List.map (\c1-> (mempty, mempty, c1, d)) (List.tail $ tails c)
                        ++ List.map (\d1-> (mempty, mempty, mempty, d1)) (List.tail $ tails d)
   span p (a, b, c, d) = ((ap, bp, cp, dp), (as, bs, cs, ds))
      where (ap, as) = span (p . fromFstOf4) a
            (bp, bs) | null as = span (p . fromSndOf4) b
                     | otherwise = (mempty, b)
            (cp, cs) | null as && null bs = span (p . fromThdOf4) c
                     | otherwise = (mempty, c)
            (dp, ds) | null as && null bs && null cs = span (p . fromFthOf4) d
                     | otherwise = (mempty, d)
   spanMaybe s0 f (a, b, c, d) | not (null as) = ((ap, mempty, mempty, mempty), (as, b, c, d), s1)
                               | not (null bs) = ((ap, bp, mempty, mempty), (as, bs, c, d), s2)
                               | not (null cs) = ((ap, bp, cp, mempty), (as, bs, cs, d), s3)
                               | otherwise = ((ap, bp, cp, dp), (as, bs, cs, ds), s4)
     where (ap, as, s1) = spanMaybe s0 (\s-> f s . fromFstOf4) a
           (bp, bs, s2) = spanMaybe s1 (\s-> f s . fromSndOf4) b
           (cp, cs, s3) = spanMaybe s2 (\s-> f s . fromThdOf4) c
           (dp, ds, s4) = spanMaybe s3 (\s-> f s . fromFthOf4) d
   spanMaybe' s0 f (a, b, c, d) | not (null as) = ((ap, mempty, mempty, mempty), (as, b, c, d), s1)
                               | not (null bs) = ((ap, bp, mempty, mempty), (as, bs, c, d), s2)
                               | not (null cs) = ((ap, bp, cp, mempty), (as, bs, cs, d), s3)
                               | otherwise = ((ap, bp, cp, dp), (as, bs, cs, ds), s4)
     where (ap, as, s1) = spanMaybe' s0 (\s-> f s . fromFstOf4) a
           (bp, bs, s2) = spanMaybe' s1 (\s-> f s . fromSndOf4) b
           (cp, cs, s3) = spanMaybe' s2 (\s-> f s . fromThdOf4) c
           (dp, ds, s4) = spanMaybe' s3 (\s-> f s . fromFthOf4) d
   splitAt n (a, b, c, d) = ((ap, bp, cp, dp), (as, bs, cs, ds))
      where (ap, as) = splitAt n a
            (bp, bs) | null as = splitAt (n - length a) b
                     | otherwise = (mempty, b)
            (cp, cs) | null as && null bs = splitAt (n - length a - length b) c
                     | otherwise = (mempty, c)
            (dp, ds) | null as && null bs && null cs = splitAt (n - length a - length b - length c) d
                     | otherwise = (mempty, d)

{-# INLINE fromFstOf4 #-}
fromFstOf4 :: (Monoid b, Monoid c, Monoid d) => a -> (a, b, c, d)
fromFstOf4 a = (a, mempty, mempty, mempty)

{-# INLINE fromSndOf4 #-}
fromSndOf4 :: (Monoid a, Monoid c, Monoid d) => b -> (a, b, c, d)
fromSndOf4 b = (mempty, b, mempty, mempty)

{-# INLINE fromThdOf4 #-}
fromThdOf4 :: (Monoid a, Monoid b, Monoid d) => c -> (a, b, c, d)
fromThdOf4 c = (mempty, mempty, c, mempty)

{-# INLINE fromFthOf4 #-}
fromFthOf4 :: (Monoid a, Monoid b, Monoid c) => d -> (a, b, c, d)
fromFthOf4 d = (mempty, mempty, mempty, d)

instance FactorialMonoid [x] where
   splitPrimePrefix [] = Nothing
   splitPrimePrefix (x:xs) = Just ([x], xs)
   splitPrimeSuffix [] = Nothing
   splitPrimeSuffix xs = Just (splitLast id xs)
      where splitLast f last@[_] = (f [], last)
            splitLast f ~(x:rest) = splitLast (f . (x:)) rest
   inits = List.inits
   tails = List.tails
   break f = List.break (f . (:[]))
   span f = List.span (f . (:[]))
   dropWhile f = List.dropWhile (f . (:[]))
   takeWhile f = List.takeWhile (f . (:[]))
   spanMaybe s0 f l = (prefix' [], suffix' [], s')
      where (prefix', suffix', s', _) = List.foldl' g (id, id, s0, True) l
            g (prefix, suffix, s1, live) x | live, Just s2 <- f s1 [x] = (prefix . (x:), id, s2, True)
                                           | otherwise = (prefix, suffix . (x:), s1, False)
   spanMaybe' s0 f l = (prefix' [], suffix' [], s')
      where (prefix', suffix', s', _) = List.foldl' g (id, id, s0, True) l
            g (prefix, suffix, s1, live) x | live, Just s2 <- f s1 [x] = seq s2 $ (prefix . (x:), id, s2, True)
                                           | otherwise = (prefix, suffix . (x:), s1, False)
   splitAt = List.splitAt
   drop = List.drop
   take = List.take

instance FactorialMonoid ByteString.ByteString where
   splitPrimePrefix x = if ByteString.null x then Nothing else Just (ByteString.splitAt 1 x)
   splitPrimeSuffix x = if ByteString.null x then Nothing else Just (ByteString.splitAt (ByteString.length x - 1) x)
   inits = ByteString.inits
   tails = ByteString.tails
   break f = ByteString.break (f . ByteString.singleton)
   span f = ByteString.span (f . ByteString.singleton)
   spanMaybe s0 f b = case ByteString.foldr g id b (0, s0)
                      of (i, s') | (prefix, suffix) <- ByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s) | Just s' <- f s (ByteString.singleton w) = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f b = case ByteString.foldr g id b (0, s0)
                       of (i, s') | (prefix, suffix) <- ByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s) | Just s' <- f s (ByteString.singleton w) = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   dropWhile f = ByteString.dropWhile (f . ByteString.singleton)
   takeWhile f = ByteString.takeWhile (f . ByteString.singleton)
   split f = ByteString.splitWith f'
      where f' = f . ByteString.singleton
   splitAt = ByteString.splitAt
   drop = ByteString.drop
   take = ByteString.take

instance FactorialMonoid LazyByteString.ByteString where
   splitPrimePrefix x = if LazyByteString.null x then Nothing
                        else Just (LazyByteString.splitAt 1 x)
   splitPrimeSuffix x = if LazyByteString.null x then Nothing
                        else Just (LazyByteString.splitAt (LazyByteString.length x - 1) x)
   inits = LazyByteString.inits
   tails = LazyByteString.tails
   break f = LazyByteString.break (f . LazyByteString.singleton)
   span f = LazyByteString.span (f . LazyByteString.singleton)
   spanMaybe s0 f b = case LazyByteString.foldr g id b (0, s0)
                      of (i, s') | (prefix, suffix) <- LazyByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s) | Just s' <- f s (LazyByteString.singleton w) = let i' = succ i :: Int64 in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f b = case LazyByteString.foldr g id b (0, s0)
                       of (i, s') | (prefix, suffix) <- LazyByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s)
              | Just s' <- f s (LazyByteString.singleton w) = let i' = succ i :: Int64 in seq i' $ seq s' $ cont (i', s')
              | otherwise = (i, s)
   dropWhile f = LazyByteString.dropWhile (f . LazyByteString.singleton)
   takeWhile f = LazyByteString.takeWhile (f . LazyByteString.singleton)
   split f = LazyByteString.splitWith f'
      where f' = f . LazyByteString.singleton
   splitAt = LazyByteString.splitAt . fromIntegral
   drop n = LazyByteString.drop (fromIntegral n)
   take n = LazyByteString.take (fromIntegral n)

instance FactorialMonoid Text.Text where
   splitPrimePrefix = fmap (first Text.singleton) . Text.uncons
   splitPrimeSuffix x = if Text.null x then Nothing else Just (Text.init x, Text.singleton (Text.last x))
   inits = Text.inits
   tails = Text.tails
   span f = Text.span (f . Text.singleton)
   break f = Text.break (f . Text.singleton)
   dropWhile f = Text.dropWhile (f . Text.singleton)
   takeWhile f = Text.takeWhile (f . Text.singleton)
   spanMaybe s0 f t = case Text.foldr g id t (0, s0)
                      of (i, s') | (prefix, suffix) <- Text.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (Text.singleton c) = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f t = case Text.foldr g id t (0, s0)
                       of (i, s') | (prefix, suffix) <- Text.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (Text.singleton c) = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   split f = Text.split f'
      where f' = f . Text.singleton
   splitAt = Text.splitAt
   drop = Text.drop
   take = Text.take

instance FactorialMonoid LazyText.Text where
   splitPrimePrefix = fmap (first LazyText.singleton) . LazyText.uncons
   splitPrimeSuffix x = if LazyText.null x
                        then Nothing
                        else Just (LazyText.init x, LazyText.singleton (LazyText.last x))
   inits = LazyText.inits
   tails = LazyText.tails
   span f = LazyText.span (f . LazyText.singleton)
   break f = LazyText.break (f . LazyText.singleton)
   dropWhile f = LazyText.dropWhile (f . LazyText.singleton)
   takeWhile f = LazyText.takeWhile (f . LazyText.singleton)
   spanMaybe s0 f t = case LazyText.foldr g id t (0, s0)
                      of (i, s') | (prefix, suffix) <- LazyText.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (LazyText.singleton c) = let i' = succ i :: Int64 in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f t = case LazyText.foldr g id t (0, s0)
                       of (i, s') | (prefix, suffix) <- LazyText.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (LazyText.singleton c) = let i' = succ i :: Int64 in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   split f = LazyText.split f'
      where f' = f . LazyText.singleton
   splitAt = LazyText.splitAt . fromIntegral
   drop n = LazyText.drop (fromIntegral n)
   take n = LazyText.take (fromIntegral n)

instance Ord k => FactorialMonoid (Map.Map k v) where
   splitPrimePrefix = fmap singularize . Map.minViewWithKey
      where singularize ((k, v), rest) = (Map.singleton k v, rest)
   splitPrimeSuffix = fmap singularize . Map.maxViewWithKey
      where singularize ((k, v), rest) = (rest, Map.singleton k v)

instance FactorialMonoid (IntMap.IntMap a) where
   splitPrimePrefix = fmap singularize . IntMap.minViewWithKey
      where singularize ((k, v), rest) = (IntMap.singleton k v, rest)
   splitPrimeSuffix = fmap singularize . IntMap.maxViewWithKey
      where singularize ((k, v), rest) = (rest, IntMap.singleton k v)

instance FactorialMonoid IntSet.IntSet where
   splitPrimePrefix = fmap singularize . IntSet.minView
      where singularize (min, rest) = (IntSet.singleton min, rest)
   splitPrimeSuffix = fmap singularize . IntSet.maxView
      where singularize (max, rest) = (rest, IntSet.singleton max)

instance FactorialMonoid (Sequence.Seq a) where
   splitPrimePrefix q = case Sequence.viewl q
                        of Sequence.EmptyL -> Nothing
                           hd Sequence.:< rest -> Just (Sequence.singleton hd, rest)
   splitPrimeSuffix q = case Sequence.viewr q
                        of Sequence.EmptyR -> Nothing
                           rest Sequence.:> last -> Just (rest, Sequence.singleton last)
   inits = Foldable.toList . Sequence.inits
   tails = Foldable.toList . Sequence.tails
   span f = Sequence.spanl (f . Sequence.singleton)
   break f = Sequence.breakl (f . Sequence.singleton)
   dropWhile f = Sequence.dropWhileL (f . Sequence.singleton)
   takeWhile f = Sequence.takeWhileL (f . Sequence.singleton)
   spanMaybe s0 f b = case Foldable.foldr g id b (0, s0)
                      of (i, s') | (prefix, suffix) <- Sequence.splitAt i b -> (prefix, suffix, s')
      where g x cont (i, s) | Just s' <- f s (Sequence.singleton x) = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f b = case Foldable.foldr g id b (0, s0)
                       of (i, s') | (prefix, suffix) <- Sequence.splitAt i b -> (prefix, suffix, s')
      where g x cont (i, s) | Just s' <- f s (Sequence.singleton x) = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   splitAt = Sequence.splitAt
   drop = Sequence.drop
   take = Sequence.take

instance Ord a => FactorialMonoid (Set.Set a) where
   splitPrimePrefix = fmap singularize . Set.minView
      where singularize (min, rest) = (Set.singleton min, rest)
   splitPrimeSuffix = fmap singularize . Set.maxView
      where singularize (max, rest) = (rest, Set.singleton max)

instance FactorialMonoid (Vector.Vector a) where
   splitPrimePrefix x = if Vector.null x then Nothing else Just (Vector.splitAt 1 x)
   splitPrimeSuffix x = if Vector.null x then Nothing else Just (Vector.splitAt (Vector.length x - 1) x)
   inits x0 = initsWith x0 []
      where initsWith x rest | Vector.null x = x:rest
                             | otherwise = initsWith (Vector.unsafeInit x) (x:rest)
   tails x = x : if Vector.null x then [] else tails (Vector.unsafeTail x)
   break f = Vector.break (f . Vector.singleton)
   span f = Vector.span (f . Vector.singleton)
   dropWhile f = Vector.dropWhile (f . Vector.singleton)
   takeWhile f = Vector.takeWhile (f . Vector.singleton)
   spanMaybe s0 f v = case Vector.ifoldr g Left v s0
                      of Left s' -> (v, Vector.empty, s')
                         Right (i, s') | (prefix, suffix) <- Vector.splitAt i v -> (prefix, suffix, s')
      where g i x cont s | Just s' <- f s (Vector.singleton x) = cont s'
                         | otherwise = Right (i, s)
   spanMaybe' s0 f v = case Vector.ifoldr' g Left v s0
                       of Left s' -> (v, Vector.empty, s')
                          Right (i, s') | (prefix, suffix) <- Vector.splitAt i v -> (prefix, suffix, s')
      where g i x cont s | Just s' <- f s (Vector.singleton x) = seq s' (cont s')
                         | otherwise = Right (i, s)
   splitAt = Vector.splitAt
   drop = Vector.drop
   take = Vector.take
