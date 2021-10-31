{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'Semigroup' => 'Factorial' => 'StableFactorial' classes and some of their instances.
-- 

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}

module Data.Semigroup.Factorial (
   -- * Classes
   Factorial(..), StableFactorial,
   -- * Monad function equivalents
   mapM, mapM_
   )
where

import qualified Control.Monad as Monad
import Data.Semigroup -- (Semigroup (..), Dual(..), Sum(..), Product(..), Endo(Endo, appEndo))
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
import Data.List.NonEmpty (nonEmpty)
import Data.Numbers.Primes (primeFactors)
import Numeric.Natural (Natural)

import Data.Monoid.Null (MonoidNull(null))

import Prelude hiding (break, drop, dropWhile, foldl, foldr, last, length, map, mapM, mapM_, null, reverse)

-- | Class of semigroups that can be split into irreducible (/i.e./, atomic or prime) 'factors' in a unique way. Factors of
-- a 'Product' are literally its prime factors:
--
-- prop> factors (Product 12) == [Product 2, Product 2, Product 3]
--
-- Factors of a list are /not/ its elements but all its single-item sublists:
--
-- prop> factors "abc" == ["a", "b", "c"]
-- 
-- The methods of this class satisfy the following laws:
-- 
-- > maybe id sconcat  . nonEmpty . factors == id
-- > List.all (\prime-> factors prime == [prime]) . factors
-- > primePrefix s == foldr const s s
-- > foldl f a == List.foldl f a . factors
-- > foldl' f a == List.foldl' f a . factors
-- > foldr f a == List.foldr f a . factors
--
-- A minimal instance definition must implement 'factors' or 'foldr'. Other methods can and should be implemented only
-- for performance reasons.
class Semigroup m => Factorial m where
   -- | Returns a list of all prime factors; inverse of mconcat.
   factors :: m -> [m]
   -- | The prime prefix; @primePrefix mempty == mempty@ for monoids.
   primePrefix :: m -> m
   -- | The prime suffix; @primeSuffix mempty == mempty@ for monoids.
   primeSuffix :: m -> m
   -- | Like 'List.foldl' from "Data.List" on the list of prime 'factors'.
   foldl :: (a -> m -> a) -> a -> m -> a
   -- | Like 'List.foldl'' from "Data.List" on the list of prime 'factors'.
   foldl' :: (a -> m -> a) -> a -> m -> a
   -- | Like 'List.foldr' from "Data.List" on the list of prime 'factors'.
   foldr :: (m -> a -> a) -> a -> m -> a
   -- | The 'length' of the list of prime 'factors'.
   length :: m -> Int
   -- | Generalizes 'Foldable.foldMap' from "Data.Foldable", except the function arguments are prime 'factors' rather
   -- than the structure elements.
   foldMap :: Monoid n => (m -> n) -> m -> n
   -- | Equivalent to 'List.reverse' from "Data.List".
   reverse :: m -> m

   factors = foldr (:) []
   primePrefix s = foldr const s s
   primeSuffix s = foldl (const id) s s
   foldl f f0 = List.foldl f f0 . factors
   foldl' f f0 = List.foldl' f f0 . factors
   foldr f f0 = List.foldr f f0 . factors
   length = foldl' (const . succ) 0
   foldMap f = foldr (mappend . f) mempty
   reverse s = maybe s sconcat (nonEmpty $ List.reverse $ factors s)
   {-# MINIMAL factors | foldr #-}

-- | A subclass of 'Factorial' whose instances satisfy the following additional laws:
--
-- > factors (a <> b) == factors a <> factors b
-- > factors . reverse == List.reverse . factors
-- > primeSuffix s == primePrefix (reverse s)
class Factorial m => StableFactorial m

instance Factorial () where
   factors () = []
   primePrefix () = ()
   primeSuffix () = ()
   length () = 0
   reverse = id

instance Factorial a => Factorial (Dual a) where
   factors (Dual a) = fmap Dual (reverse $ factors a)
   length (Dual a) = length a
   primePrefix (Dual a) = Dual (primeSuffix a)
   primeSuffix (Dual a) = Dual (primePrefix a)
   reverse (Dual a) = Dual (reverse a)

instance (Integral a, Eq a) => Factorial (Sum a) where
   primePrefix (Sum a) = Sum (signum a )
   primeSuffix = primePrefix
   factors (Sum n) = replicate (fromIntegral $ abs n) (Sum $ signum n)
   length (Sum a) = abs (fromIntegral a)
   reverse = id

instance Integral a => Factorial (Product a) where
   factors (Product a) = List.map Product (primeFactors a)
   reverse = id

instance Factorial a => Factorial (Maybe a) where
   factors Nothing = []
   factors (Just a) = case factors a
                      of [] -> [Just a]
                         as -> List.map Just as
   length Nothing = 0
   length (Just a) = max 1 (length a)
   reverse = fmap reverse

instance (Factorial a, Factorial b, MonoidNull a, MonoidNull b) => Factorial (a, b) where
   factors (a, b) = List.map (\a1-> (a1, mempty)) (factors a) ++ List.map ((,) mempty) (factors b)
   primePrefix (a, b) | null a = (a, primePrefix b)
                      | otherwise = (primePrefix a, mempty)
   primeSuffix (a, b) | null b = (primeSuffix a, b)
                      | otherwise = (mempty, primeSuffix b)
   foldl f a0 (x, y) = foldl f2 (foldl f1 a0 x) y
      where f1 a = f a . fromFst
            f2 a = f a . fromSnd
   foldl' f a0 (x, y) = a' `seq` foldl' f2 a' y
      where f1 a = f a . fromFst
            f2 a = f a . fromSnd
            a' = foldl' f1 a0 x
   foldr f a (x, y) = foldr (f . fromFst) (foldr (f . fromSnd) a y) x
   foldMap f (x, y) = Data.Semigroup.Factorial.foldMap (f . fromFst) x `mappend`
                      Data.Semigroup.Factorial.foldMap (f . fromSnd) y
   length (a, b) = length a + length b
   reverse (a, b) = (reverse a, reverse b)

{-# INLINE fromFst #-}
fromFst :: Monoid b => a -> (a, b)
fromFst a = (a, mempty)

{-# INLINE fromSnd #-}
fromSnd :: Monoid a => b -> (a, b)
fromSnd b = (mempty, b)

instance (Factorial a, Factorial b, Factorial c,
          MonoidNull a, MonoidNull b, MonoidNull c) => Factorial (a, b, c) where
   factors (a, b, c) = List.map (\a1-> (a1, mempty, mempty)) (factors a)
                       ++ List.map (\b1-> (mempty, b1, mempty)) (factors b)
                       ++ List.map (\c1-> (mempty, mempty, c1)) (factors c)
   primePrefix (a, b, c) | not (null a) = (primePrefix a, mempty, mempty)
                         | not (null b) = (mempty, primePrefix b, mempty)
                         | otherwise = (mempty, mempty, primePrefix c)
   primeSuffix (a, b, c) | not (null c) = (mempty, mempty, primeSuffix c)
                         | not (null b) = (mempty, primeSuffix b, mempty)
                         | otherwise = (primeSuffix a, mempty, mempty)
   foldl f s0 (a, b, c) = foldl f3 (foldl f2 (foldl f1 s0 a) b) c
      where f1 x = f x . fromFstOf3
            f2 x = f x . fromSndOf3
            f3 x = f x . fromThdOf3
   foldl' f s0 (a, b, c) = a' `seq` b' `seq` foldl' f3 b' c
      where f1 x = f x . fromFstOf3
            f2 x = f x . fromSndOf3
            f3 x = f x . fromThdOf3
            a' = foldl' f1 s0 a
            b' = foldl' f2 a' b
   foldr f s (a, b, c) = foldr (f . fromFstOf3) (foldr (f . fromSndOf3) (foldr (f . fromThdOf3) s c) b) a
   foldMap f (a, b, c) = Data.Semigroup.Factorial.foldMap (f . fromFstOf3) a
                         `mappend` Data.Semigroup.Factorial.foldMap (f . fromSndOf3) b
                         `mappend` Data.Semigroup.Factorial.foldMap (f . fromThdOf3) c
   length (a, b, c) = length a + length b + length c
   reverse (a, b, c) = (reverse a, reverse b, reverse c)

{-# INLINE fromFstOf3 #-}
fromFstOf3 :: (Monoid b, Monoid c) => a -> (a, b, c)
fromFstOf3 a = (a, mempty, mempty)

{-# INLINE fromSndOf3 #-}
fromSndOf3 :: (Monoid a, Monoid c) => b -> (a, b, c)
fromSndOf3 b = (mempty, b, mempty)

{-# INLINE fromThdOf3 #-}
fromThdOf3 :: (Monoid a, Monoid b) => c -> (a, b, c)
fromThdOf3 c = (mempty, mempty, c)

instance (Factorial a, Factorial b, Factorial c, Factorial d,
          MonoidNull a, MonoidNull b, MonoidNull c, MonoidNull d) => Factorial (a, b, c, d) where
   factors (a, b, c, d) = List.map (\a1-> (a1, mempty, mempty, mempty)) (factors a)
                          ++ List.map (\b1-> (mempty, b1, mempty, mempty)) (factors b)
                          ++ List.map (\c1-> (mempty, mempty, c1, mempty)) (factors c)
                          ++ List.map (\d1-> (mempty, mempty, mempty, d1)) (factors d)
   primePrefix (a, b, c, d) | not (null a) = (primePrefix a, mempty, mempty, mempty)
                            | not (null b) = (mempty, primePrefix b, mempty, mempty)
                            | not (null c) = (mempty, mempty, primePrefix c, mempty)
                            | otherwise    = (mempty, mempty, mempty, primePrefix d)
   primeSuffix (a, b, c, d) | not (null d) = (mempty, mempty, mempty, primeSuffix d)
                            | not (null c) = (mempty, mempty, primeSuffix c, mempty)
                            | not (null b) = (mempty, primeSuffix b, mempty, mempty)
                            | otherwise    = (primeSuffix a, mempty, mempty, mempty)
   foldl f s0 (a, b, c, d) = foldl f4 (foldl f3 (foldl f2 (foldl f1 s0 a) b) c) d
      where f1 x = f x . fromFstOf4
            f2 x = f x . fromSndOf4
            f3 x = f x . fromThdOf4
            f4 x = f x . fromFthOf4
   foldl' f s0 (a, b, c, d) = a' `seq` b' `seq` c' `seq` foldl' f4 c' d
      where f1 x = f x . fromFstOf4
            f2 x = f x . fromSndOf4
            f3 x = f x . fromThdOf4
            f4 x = f x . fromFthOf4
            a' = foldl' f1 s0 a
            b' = foldl' f2 a' b
            c' = foldl' f3 b' c
   foldr f s (a, b, c, d) =
      foldr (f . fromFstOf4) (foldr (f . fromSndOf4) (foldr (f . fromThdOf4) (foldr (f . fromFthOf4) s d) c) b) a
   foldMap f (a, b, c, d) = Data.Semigroup.Factorial.foldMap (f . fromFstOf4) a
                            `mappend` Data.Semigroup.Factorial.foldMap (f . fromSndOf4) b
                            `mappend` Data.Semigroup.Factorial.foldMap (f . fromThdOf4) c
                            `mappend` Data.Semigroup.Factorial.foldMap (f . fromFthOf4) d
   length (a, b, c, d) = length a + length b + length c + length d
   reverse (a, b, c, d) = (reverse a, reverse b, reverse c, reverse d)

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

instance Factorial [x] where
   factors xs = List.map (:[]) xs
   primePrefix [] = []
   primePrefix (x:_) = [x]
   primeSuffix [] = []
   primeSuffix xs = [List.last xs]
   foldl _ a [] = a
   foldl f a (x:xs) = foldl f (f a [x]) xs
   foldl' _ a [] = a
   foldl' f a (x:xs) = let a' = f a [x] in a' `seq` foldl' f a' xs
   foldr _ f0 [] = f0
   foldr f f0 (x:xs) = f [x] (foldr f f0 xs)
   length = List.length
   foldMap f = mconcat . List.map (f . (:[]))
   reverse = List.reverse

instance Factorial ByteString.ByteString where
   factors x = factorize (ByteString.length x) x
      where factorize 0 _ = []
            factorize n xs = xs1 : factorize (pred n) xs'
              where (xs1, xs') = ByteString.splitAt 1 xs
   primePrefix = ByteString.take 1
   primeSuffix x = ByteString.drop (ByteString.length x - 1) x
   foldl f = ByteString.foldl f'
      where f' a byte = f a (ByteString.singleton byte)
   foldl' f = ByteString.foldl' f'
      where f' a byte = f a (ByteString.singleton byte)
   foldr f = ByteString.foldr (f . ByteString.singleton)
   length = ByteString.length
   reverse = ByteString.reverse

instance Factorial LazyByteString.ByteString where
   factors x = factorize (LazyByteString.length x) x
      where factorize 0 _ = []
            factorize n xs = xs1 : factorize (pred n) xs'
               where (xs1, xs') = LazyByteString.splitAt 1 xs
   primePrefix = LazyByteString.take 1
   primeSuffix x = LazyByteString.drop (LazyByteString.length x - 1) x
   length = fromIntegral . LazyByteString.length
   reverse = LazyByteString.reverse

instance Factorial Text.Text where
   factors = Text.chunksOf 1
   primePrefix = Text.take 1
   primeSuffix x = if Text.null x then Text.empty else Text.singleton (Text.last x)
   foldl f = Text.foldl f'
      where f' a char = f a (Text.singleton char)
   foldl' f = Text.foldl' f'
      where f' a char = f a (Text.singleton char)
   foldr f = Text.foldr f'
      where f' char a = f (Text.singleton char) a
   length = Text.length
   reverse = Text.reverse

instance Factorial LazyText.Text where
   factors = LazyText.chunksOf 1
   primePrefix = LazyText.take 1
   primeSuffix x = if LazyText.null x then LazyText.empty else LazyText.singleton (LazyText.last x)
   foldl f = LazyText.foldl f'
      where f' a char = f a (LazyText.singleton char)
   foldl' f = LazyText.foldl' f'
      where f' a char = f a (LazyText.singleton char)
   foldr f = LazyText.foldr f'
      where f' char a = f (LazyText.singleton char) a
   length = fromIntegral . LazyText.length
   reverse = LazyText.reverse

instance Ord k => Factorial (Map.Map k v) where
   factors = List.map (uncurry Map.singleton) . Map.toAscList
   primePrefix map | Map.null map = map
                   | otherwise = uncurry Map.singleton $ Map.findMin map
   primeSuffix map | Map.null map = map
                   | otherwise = uncurry Map.singleton $ Map.findMax map
   foldl f = Map.foldlWithKey f'
      where f' a k v = f a (Map.singleton k v)
   foldl' f = Map.foldlWithKey' f'
      where f' a k v = f a (Map.singleton k v)
   foldr f = Map.foldrWithKey f'
      where f' k v a = f (Map.singleton k v) a
   length = Map.size
   reverse = id

instance Factorial (IntMap.IntMap a) where
   factors = List.map (uncurry IntMap.singleton) . IntMap.toAscList
   primePrefix map | IntMap.null map = map
                   | otherwise = uncurry IntMap.singleton $ IntMap.findMin map
   primeSuffix map | IntMap.null map = map
                   | otherwise = uncurry IntMap.singleton $ IntMap.findMax map
   foldl f = IntMap.foldlWithKey f'
      where f' a k v = f a (IntMap.singleton k v)
   foldl' f = IntMap.foldlWithKey' f'
      where f' a k v = f a (IntMap.singleton k v)
   foldr f = IntMap.foldrWithKey f'
      where f' k v a = f (IntMap.singleton k v) a
   length = IntMap.size
   reverse = id

instance Factorial IntSet.IntSet where
   factors = List.map IntSet.singleton . IntSet.toAscList
   primePrefix set | IntSet.null set = set
                   | otherwise = IntSet.singleton $ IntSet.findMin set
   primeSuffix set | IntSet.null set = set
                   | otherwise = IntSet.singleton $ IntSet.findMax set
   foldl f = IntSet.foldl f'
      where f' a b = f a (IntSet.singleton b)
   foldl' f = IntSet.foldl' f'
      where f' a b = f a (IntSet.singleton b)
   foldr f = IntSet.foldr f'
      where f' a b = f (IntSet.singleton a) b
   length = IntSet.size
   reverse = id

instance Factorial (Sequence.Seq a) where
   factors = List.map Sequence.singleton . Foldable.toList
   primePrefix = Sequence.take 1
   primeSuffix q = Sequence.drop (Sequence.length q - 1) q
   foldl f = Foldable.foldl f'
      where f' a b = f a (Sequence.singleton b)
   foldl' f = Foldable.foldl' f'
      where f' a b = f a (Sequence.singleton b)
   foldr f = Foldable.foldr f'
      where f' a b = f (Sequence.singleton a) b
   length = Sequence.length
   reverse = Sequence.reverse

instance Ord a => Factorial (Set.Set a) where
   factors = List.map Set.singleton . Set.toAscList
   primePrefix set | Set.null set = set
                   | otherwise = Set.singleton $ Set.findMin set
   primeSuffix set | Set.null set = set
                   | otherwise = Set.singleton $ Set.findMax set
   foldl f = Foldable.foldl f'
      where f' a b = f a (Set.singleton b)
   foldl' f = Foldable.foldl' f'
      where f' a b = f a (Set.singleton b)
   foldr f = Foldable.foldr f'
      where f' a b = f (Set.singleton a) b
   length = Set.size
   reverse = id

instance Factorial (Vector.Vector a) where
   factors x = factorize (Vector.length x) x
      where factorize 0 _ = []
            factorize n xs = xs1 : factorize (pred n) xs'
               where (xs1, xs') = Vector.splitAt 1 xs
   primePrefix = Vector.take 1
   primeSuffix x = Vector.drop (Vector.length x - 1) x
   foldl f = Vector.foldl f'
      where f' a byte = f a (Vector.singleton byte)
   foldl' f = Vector.foldl' f'
      where f' a byte = f a (Vector.singleton byte)
   foldr f = Vector.foldr f'
      where f' byte a = f (Vector.singleton byte) a
   length = Vector.length
   reverse = Vector.reverse

instance StableFactorial ()
instance StableFactorial a => StableFactorial (Dual a)
instance StableFactorial [x]
instance StableFactorial ByteString.ByteString
instance StableFactorial LazyByteString.ByteString
instance StableFactorial Text.Text
instance StableFactorial LazyText.Text
instance StableFactorial (Sequence.Seq a)
instance StableFactorial (Vector.Vector a)
instance StableFactorial (Sum Natural)

-- | A 'Monad.mapM' equivalent.
mapM :: (Factorial a, Semigroup b, Monoid b, Monad m) => (a -> m b) -> a -> m b
mapM f = ($ return mempty) . appEndo . Data.Semigroup.Factorial.foldMap (Endo . Monad.liftM2 mappend . f)

-- | A 'Monad.mapM_' equivalent.
mapM_ :: (Factorial a, Applicative m) => (a -> m b) -> a -> m ()
mapM_ f = foldr ((*>) . f) (pure ())
