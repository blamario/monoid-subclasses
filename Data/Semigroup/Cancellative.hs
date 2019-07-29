{- 
    Copyright 2013-2017 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'Semigroup' => 'ReductiveSemigroup' => 'CancellativeSemigroup' class hierarchy.
--
-- The 'ReductiveSemigroup' class introduces operation '</>' which is the inverse of '<>'. For the 'Sum' monoid, this
-- operation is subtraction; for 'Product' it is division and for 'Set' it's the set difference. A 'ReductiveSemigroup'
-- is not a full group because '</>' may return 'Nothing'.
--
-- The 'CancellativeSemigroup' subclass does not add any operation but it provides the additional guarantee that '<>'
-- can always be undone with '</>'. Thus 'Sum' is a 'CancellativeSemigroup' but 'Product' is not because @(0*n)/0@ is
-- not defined.
--
-- All monoid subclasses listed above are for Abelian, /i.e./, commutative or symmetric monoids. Since most practical
-- monoids in Haskell are not Abelian, each of the these classes has two symmetric superclasses:
-- 
-- * 'LeftReductiveSemigroup'
-- 
-- * 'LeftCancellativeSemigroup'
-- 
-- * 'RightReductiveSemigroup'
-- 
-- * 'RightCancellativeSemigroup'

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}

module Data.Semigroup.Cancellative (
   -- * Symmetric, commutative semigroup classes
   CommutativeSemigroup, ReductiveSemigroup(..), CancellativeSemigroup,
   -- * Asymmetric semigroup classes
   LeftReductiveSemigroup(..), RightReductiveSemigroup(..),
   LeftCancellativeSemigroup, RightCancellativeSemigroup
   )
where

import Data.Semigroup -- (Semigroup, Dual(..), Sum(..), Product(..))
import qualified Data.List as List
import Data.Maybe (isJust)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
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

-- | Class of all Abelian ({i.e.}, commutative) semigroups that satisfy the commutativity property:
-- 
-- > a <> b == b <> a
class Semigroup m => CommutativeSemigroup m

-- | Class of Abelian semigroups with a partial inverse for the Semigroup '<>' operation. The inverse operation '</>' must
-- satisfy the following laws:
-- 
-- > maybe a (b <>) (a </> b) == a
-- > maybe a (<> b) (a </> b) == a
--
-- The '</>' operator is a synonym for both 'stripPrefix' and 'stripSuffix', which must be equivalent as '<>' is both
-- associative and commutative.
--
-- > (</>) = flip stripPrefix
-- > (</>) = flip stripSuffix
class (CommutativeSemigroup m, LeftReductiveSemigroup m, RightReductiveSemigroup m) => ReductiveSemigroup m where
   (</>) :: m -> m -> Maybe m

infix 5 </>

-- | Subclass of 'ReductiveSemigroup' where '</>' is a complete inverse of the Semigroup '<>' operation. The class
-- instances must satisfy the following additional laws:
--
-- > (a <> b) </> a == Just b
-- > (a <> b) </> b == Just a
class (LeftCancellativeSemigroup m, RightCancellativeSemigroup m, ReductiveSemigroup m) => CancellativeSemigroup m

-- | Class of semigroups with a left inverse of 'Data.Semigroup.<>', satisfying the following law:
-- 
-- > isPrefixOf a b == isJust (stripPrefix a b)
-- > maybe b (a <>) (stripPrefix a b) == b
-- > a `isPrefixOf` (a <> b)
-- 
-- | Every instance definition has to implement at least the 'stripPrefix' method. Its complexity should be no worse
-- than linear in the length of the prefix argument.
class Semigroup m => LeftReductiveSemigroup m where
   isPrefixOf :: m -> m -> Bool
   stripPrefix :: m -> m -> Maybe m

   isPrefixOf a b = isJust (stripPrefix a b)
   {-# MINIMAL stripPrefix #-}

-- | Class of semigroups with a right inverse of 'Data.Semigroup.<>', satisfying the following law:
-- 
-- > isSuffixOf a b == isJust (stripSuffix a b)
-- > maybe b (<> a) (stripSuffix a b) == b
-- > b `isSuffixOf` (a <> b)
-- 
-- | Every instance definition has to implement at least the 'stripSuffix' method. Its complexity should be no worse
-- than linear in the length of the suffix argument.
class Semigroup m => RightReductiveSemigroup m where
   isSuffixOf :: m -> m -> Bool
   stripSuffix :: m -> m -> Maybe m

   isSuffixOf a b = isJust (stripSuffix a b)
   {-# MINIMAL stripSuffix #-}

-- | Subclass of 'LeftReductiveSemigroup' where 'stripPrefix' is a complete inverse of '<>', satisfying the following
-- additional law:
--
-- > stripPrefix a (a <> b) == Just b
class LeftReductiveSemigroup m => LeftCancellativeSemigroup m

-- | Subclass of 'LeftReductiveSemigroup' where 'stripPrefix' is a complete inverse of '<>', satisfying the following
-- additional law:
--
-- > stripSuffix b (a <> b) == Just a
class RightReductiveSemigroup m => RightCancellativeSemigroup m

-- Unit instances

instance CommutativeSemigroup ()

instance ReductiveSemigroup () where
   () </> () = Just ()

instance CancellativeSemigroup ()

instance LeftReductiveSemigroup () where
   stripPrefix () () = Just ()

instance RightReductiveSemigroup () where
   stripSuffix () () = Just ()

instance LeftCancellativeSemigroup ()

instance RightCancellativeSemigroup ()

-- Dual instances

instance CommutativeSemigroup a => CommutativeSemigroup (Dual a)

instance ReductiveSemigroup a => ReductiveSemigroup (Dual a) where
   Dual a </> Dual b = fmap Dual (a </> b)

instance CancellativeSemigroup a => CancellativeSemigroup (Dual a)

instance LeftReductiveSemigroup a => RightReductiveSemigroup (Dual a) where
   stripSuffix (Dual a) (Dual b) = fmap Dual (stripPrefix a b)
   Dual a `isSuffixOf` Dual b = a `isPrefixOf` b

instance RightReductiveSemigroup a => LeftReductiveSemigroup (Dual a) where
   stripPrefix (Dual a) (Dual b) = fmap Dual (stripSuffix a b)
   Dual a `isPrefixOf` Dual b = a `isSuffixOf` b

instance LeftCancellativeSemigroup a => RightCancellativeSemigroup (Dual a)

instance RightCancellativeSemigroup a => LeftCancellativeSemigroup (Dual a)

-- Sum instances

instance Num a => CommutativeSemigroup (Sum a)

instance Integral a => ReductiveSemigroup (Sum a) where
   Sum a </> Sum b = Just $ Sum (a - b)

instance Integral a => CancellativeSemigroup (Sum a)

instance Integral a => LeftReductiveSemigroup (Sum a) where
   stripPrefix a b = b </> a

instance Integral a => RightReductiveSemigroup (Sum a) where
   stripSuffix a b = b </> a

instance Integral a => LeftCancellativeSemigroup (Sum a)

instance Integral a => RightCancellativeSemigroup (Sum a)

instance {-# OVERLAPS #-} ReductiveSemigroup (Sum Natural) where
   Sum a </> Sum b
      | a < b = Nothing
      | otherwise = Just $ Sum (a - b)

instance {-# OVERLAPS #-} LeftReductiveSemigroup (Sum Natural) where
   stripPrefix a b = b </> a

instance {-# OVERLAPS #-} RightReductiveSemigroup (Sum Natural) where
   stripSuffix a b = b </> a

-- Product instances

instance Num a => CommutativeSemigroup (Product a)

instance Integral a => ReductiveSemigroup (Product a) where
   Product 0 </> Product 0 = Just (Product 0)
   Product _ </> Product 0 = Nothing
   Product a </> Product b = if remainder == 0 then Just (Product quotient) else Nothing
      where (quotient, remainder) = quotRem a b

instance Integral a => LeftReductiveSemigroup (Product a) where
   stripPrefix a b = b </> a

instance Integral a => RightReductiveSemigroup (Product a) where
   stripSuffix a b = b </> a

-- Pair instances

instance (CommutativeSemigroup a, CommutativeSemigroup b) => CommutativeSemigroup (a, b)

instance (ReductiveSemigroup a, ReductiveSemigroup b) => ReductiveSemigroup (a, b) where
   (a, b) </> (c, d) = case (a </> c, b </> d)
                       of (Just a', Just b') -> Just (a', b')
                          _ -> Nothing

instance (CancellativeSemigroup a, CancellativeSemigroup b) => CancellativeSemigroup (a, b)

instance (LeftReductiveSemigroup a, LeftReductiveSemigroup b) => LeftReductiveSemigroup (a, b) where
   stripPrefix (a, b) (c, d) = case (stripPrefix a c, stripPrefix b d)
                               of (Just a', Just b') -> Just (a', b')
                                  _ -> Nothing
   isPrefixOf (a, b) (c, d) = isPrefixOf a c && isPrefixOf b d

instance (RightReductiveSemigroup a, RightReductiveSemigroup b) => RightReductiveSemigroup (a, b) where
   stripSuffix (a, b) (c, d) = case (stripSuffix a c, stripSuffix b d)
                               of (Just a', Just b') -> Just (a', b')
                                  _ -> Nothing
   isSuffixOf (a, b) (c, d) = isSuffixOf a c && isSuffixOf b d

instance (LeftCancellativeSemigroup a, LeftCancellativeSemigroup b) => LeftCancellativeSemigroup (a, b)

instance (RightCancellativeSemigroup a, RightCancellativeSemigroup b) => RightCancellativeSemigroup (a, b)

-- Triple instances

instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c) => CommutativeSemigroup (a, b, c)

instance (ReductiveSemigroup a, ReductiveSemigroup b, ReductiveSemigroup c) => ReductiveSemigroup (a, b, c) where
   (a1, b1, c1) </> (a2, b2, c2) = (,,) <$> (a1 </> a2) <*> (b1 </> b2) <*> (c1 </> c2)

instance (CancellativeSemigroup a, CancellativeSemigroup b, CancellativeSemigroup c) => CancellativeSemigroup (a, b, c)

instance (LeftReductiveSemigroup a, LeftReductiveSemigroup b, LeftReductiveSemigroup c) =>
         LeftReductiveSemigroup (a, b, c) where
   stripPrefix (a1, b1, c1) (a2, b2, c2) = (,,) <$> stripPrefix a1 a2 <*> stripPrefix b1 b2 <*> stripPrefix c1 c2
   isPrefixOf (a1, b1, c1) (a2, b2, c2) = isPrefixOf a1 a2 && isPrefixOf b1 b2 && isPrefixOf c1 c2

instance (RightReductiveSemigroup a, RightReductiveSemigroup b, RightReductiveSemigroup c) =>
         RightReductiveSemigroup (a, b, c) where
   stripSuffix (a1, b1, c1) (a2, b2, c2) = (,,) <$> stripSuffix a1 a2 <*> stripSuffix b1 b2 <*> stripSuffix c1 c2
   isSuffixOf (a1, b1, c1) (a2, b2, c2) = isSuffixOf a1 a2 && isSuffixOf b1 b2 && isSuffixOf c1 c2

instance (LeftCancellativeSemigroup a, LeftCancellativeSemigroup b, LeftCancellativeSemigroup c) =>
         LeftCancellativeSemigroup (a, b, c)

instance (RightCancellativeSemigroup a, RightCancellativeSemigroup b, RightCancellativeSemigroup c) =>
         RightCancellativeSemigroup (a, b, c)

-- Quadruple instances

instance (CommutativeSemigroup a, CommutativeSemigroup b, CommutativeSemigroup c, CommutativeSemigroup d) =>
         CommutativeSemigroup (a, b, c, d)

instance (ReductiveSemigroup a, ReductiveSemigroup b, ReductiveSemigroup c, ReductiveSemigroup d) =>
         ReductiveSemigroup (a, b, c, d) where
   (a1, b1, c1, d1) </> (a2, b2, c2, d2) = (,,,) <$> (a1 </> a2) <*> (b1 </> b2) <*> (c1 </> c2) <*> (d1 </> d2)

instance (CancellativeSemigroup a, CancellativeSemigroup b, CancellativeSemigroup c, CancellativeSemigroup d) =>
         CancellativeSemigroup (a, b, c, d)

instance (LeftReductiveSemigroup a, LeftReductiveSemigroup b, LeftReductiveSemigroup c, LeftReductiveSemigroup d) =>
         LeftReductiveSemigroup (a, b, c, d) where
   stripPrefix (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (,,,) <$> stripPrefix a1 a2 <*> stripPrefix b1 b2 <*> stripPrefix c1 c2 <*> stripPrefix d1 d2
   isPrefixOf (a1, b1, c1, d1) (a2, b2, c2, d2) =
      isPrefixOf a1 a2 && isPrefixOf b1 b2 && isPrefixOf c1 c2 && isPrefixOf d1 d2

instance (RightReductiveSemigroup a, RightReductiveSemigroup b, RightReductiveSemigroup c, RightReductiveSemigroup d) =>
         RightReductiveSemigroup (a, b, c, d) where
   stripSuffix (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (,,,) <$> stripSuffix a1 a2 <*> stripSuffix b1 b2 <*> stripSuffix c1 c2 <*> stripSuffix d1 d2
   isSuffixOf (a1, b1, c1, d1) (a2, b2, c2, d2) =
      isSuffixOf a1 a2 && isSuffixOf b1 b2 && isSuffixOf c1 c2 && isSuffixOf d1 d2

instance (LeftCancellativeSemigroup a, LeftCancellativeSemigroup b,
          LeftCancellativeSemigroup c, LeftCancellativeSemigroup d) => LeftCancellativeSemigroup (a, b, c, d)

instance (RightCancellativeSemigroup a, RightCancellativeSemigroup b,
          RightCancellativeSemigroup c, RightCancellativeSemigroup d) => RightCancellativeSemigroup (a, b, c, d)

-- Maybe instances

instance LeftReductiveSemigroup x => LeftReductiveSemigroup (Maybe x) where
   stripPrefix Nothing y = Just y
   stripPrefix Just{} Nothing = Nothing
   stripPrefix (Just x) (Just y) = fmap Just $ stripPrefix x y

instance RightReductiveSemigroup x => RightReductiveSemigroup (Maybe x) where
   stripSuffix Nothing y = Just y
   stripSuffix Just{} Nothing = Nothing
   stripSuffix (Just x) (Just y) = fmap Just $ stripSuffix x y

-- Set instances

instance Ord a => CommutativeSemigroup (Set.Set a)

instance Ord a => LeftReductiveSemigroup (Set.Set a) where
   isPrefixOf = Set.isSubsetOf
   stripPrefix a b = b </> a

instance Ord a => RightReductiveSemigroup (Set.Set a) where
   isSuffixOf = Set.isSubsetOf
   stripSuffix a b = b </> a

instance Ord a => ReductiveSemigroup (Set.Set a) where
   a </> b | Set.isSubsetOf b a = Just (a Set.\\ b)
           | otherwise = Nothing

-- IntSet instances

instance CommutativeSemigroup IntSet.IntSet

instance LeftReductiveSemigroup IntSet.IntSet where
   isPrefixOf = IntSet.isSubsetOf
   stripPrefix a b = b </> a

instance RightReductiveSemigroup IntSet.IntSet where
   isSuffixOf = IntSet.isSubsetOf
   stripSuffix a b = b </> a

instance ReductiveSemigroup IntSet.IntSet where
   a </> b | IntSet.isSubsetOf b a = Just (a IntSet.\\ b)
           | otherwise = Nothing

-- Map instances

instance (Ord k, Eq a) => LeftReductiveSemigroup (Map.Map k a) where
   isPrefixOf = Map.isSubmapOf
   stripPrefix a b | Map.isSubmapOf a b = Just (b Map.\\ a)
                   | otherwise = Nothing

instance (Ord k, Eq a) => RightReductiveSemigroup (Map.Map k a) where
   isSuffixOf = Map.isSubmapOfBy (const $ const True)
   stripSuffix a b | a `isSuffixOf` b = Just (Map.differenceWith (\x y-> if x == y then Nothing else Just x) b a)
                   | otherwise = Nothing

-- IntMap instances

instance Eq a => LeftReductiveSemigroup (IntMap.IntMap a) where
   isPrefixOf = IntMap.isSubmapOf
   stripPrefix a b | IntMap.isSubmapOf a b = Just (b IntMap.\\ a)
                   | otherwise = Nothing

instance Eq a => RightReductiveSemigroup (IntMap.IntMap a) where
   isSuffixOf = IntMap.isSubmapOfBy (const $ const True)
   stripSuffix a b | a `isSuffixOf` b = Just (IntMap.differenceWith (\x y-> if x == y then Nothing else Just x) b a)
                   | otherwise = Nothing

-- List instances

instance Eq x => LeftReductiveSemigroup [x] where
   stripPrefix = List.stripPrefix
   isPrefixOf = List.isPrefixOf

instance Eq x => LeftCancellativeSemigroup [x]

-- Seq instances

instance Eq a => LeftReductiveSemigroup (Sequence.Seq a) where
   stripPrefix p s | p == s1 = Just s2
                   | otherwise = Nothing
      where (s1, s2) = Sequence.splitAt (Sequence.length p) s

instance Eq a => RightReductiveSemigroup (Sequence.Seq a) where
   stripSuffix p s | p == s2 = Just s1
                   | otherwise = Nothing
      where (s1, s2) = Sequence.splitAt (Sequence.length s - Sequence.length p) s

instance Eq a => LeftCancellativeSemigroup (Sequence.Seq a)

instance Eq a => RightCancellativeSemigroup (Sequence.Seq a)

-- Vector instances

instance Eq a => LeftReductiveSemigroup (Vector.Vector a) where
   stripPrefix p l | prefixLength > Vector.length l = Nothing
                    | otherwise = strip 0
      where strip i | i == prefixLength = Just (Vector.drop prefixLength l)
                    | l Vector.! i == p Vector.! i = strip (succ i)
                    | otherwise = Nothing
            prefixLength = Vector.length p
   isPrefixOf p l | prefixLength > Vector.length l = False
                  | otherwise = test 0
      where test i | i == prefixLength = True
                   | l Vector.! i == p Vector.! i = test (succ i)
                   | otherwise = False
            prefixLength = Vector.length p

instance Eq a => RightReductiveSemigroup (Vector.Vector a) where
   stripSuffix s l | suffixLength > Vector.length l = Nothing
                   | otherwise = strip (pred suffixLength)
      where strip i | i == -1 = Just (Vector.take lengthDifference l)
                    | l Vector.! (lengthDifference + i) == s Vector.! i = strip (pred i)
                    | otherwise = Nothing
            suffixLength = Vector.length s
            lengthDifference = Vector.length l - suffixLength
   isSuffixOf s l | suffixLength > Vector.length l = False
                  | otherwise = test (pred suffixLength)
      where test i | i == -1 = True
                   | l Vector.! (lengthDifference + i) == s Vector.! i = test (pred i)
                   | otherwise = False
            suffixLength = Vector.length s
            lengthDifference = Vector.length l - suffixLength

instance Eq a => LeftCancellativeSemigroup (Vector.Vector a)

instance Eq a => RightCancellativeSemigroup (Vector.Vector a)

-- ByteString instances

instance LeftReductiveSemigroup ByteString.ByteString where
   stripPrefix p l = if ByteString.isPrefixOf p l
                     then Just (ByteString.unsafeDrop (ByteString.length p) l)
                     else Nothing
   isPrefixOf = ByteString.isPrefixOf

instance RightReductiveSemigroup ByteString.ByteString where
   stripSuffix s l = if ByteString.isSuffixOf s l
                     then Just (ByteString.unsafeTake (ByteString.length l - ByteString.length s) l)
                     else Nothing
   isSuffixOf = ByteString.isSuffixOf

instance LeftCancellativeSemigroup ByteString.ByteString

instance RightCancellativeSemigroup ByteString.ByteString

-- Lazy ByteString instances

instance LeftReductiveSemigroup LazyByteString.ByteString where
   stripPrefix p l = if LazyByteString.isPrefixOf p l
                     then Just (LazyByteString.drop (LazyByteString.length p) l)
                     else Nothing
   isPrefixOf = LazyByteString.isPrefixOf

instance RightReductiveSemigroup LazyByteString.ByteString where
   stripSuffix s l = if LazyByteString.isSuffixOf s l
                     then Just (LazyByteString.take (LazyByteString.length l - LazyByteString.length s) l)
                     else Nothing
   isSuffixOf = LazyByteString.isSuffixOf

instance LeftCancellativeSemigroup LazyByteString.ByteString

instance RightCancellativeSemigroup LazyByteString.ByteString

-- Text instances

instance LeftReductiveSemigroup Text.Text where
   stripPrefix = Text.stripPrefix
   isPrefixOf = Text.isPrefixOf

instance RightReductiveSemigroup Text.Text where
   stripSuffix = Text.stripSuffix
   isSuffixOf = Text.isSuffixOf

instance LeftCancellativeSemigroup Text.Text

instance RightCancellativeSemigroup Text.Text

-- Lazy Text instances

instance LeftReductiveSemigroup LazyText.Text where
   stripPrefix = LazyText.stripPrefix
   isPrefixOf = LazyText.isPrefixOf

instance RightReductiveSemigroup LazyText.Text where
   stripSuffix = LazyText.stripSuffix
   isSuffixOf = LazyText.isSuffixOf

instance LeftCancellativeSemigroup LazyText.Text

instance RightCancellativeSemigroup LazyText.Text
