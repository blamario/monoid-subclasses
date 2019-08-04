{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'Semigroup' => 'Reductive' => 'Cancellative' class hierarchy.
--
-- The 'Reductive' class introduces operation '</>' which is the inverse of '<>'. For the 'Sum' monoid, this
-- operation is subtraction; for 'Product' it is division and for 'Set' it's the set difference. A 'Reductive'
-- is not a full group because '</>' may return 'Nothing'.
--
-- The 'Cancellative' subclass does not add any operation but it provides the additional guarantee that '<>'
-- can always be undone with '</>'. Thus 'Sum' is a 'Cancellative' but 'Product' is not because @(0*n)/0@ is
-- not defined.
--
-- All monoid subclasses listed above are for Abelian, /i.e./, commutative or symmetric monoids. Since most practical
-- monoids in Haskell are not Abelian, each of the these classes has two symmetric superclasses:
-- 
-- * 'LeftReductive'
-- 
-- * 'LeftCancellative'
-- 
-- * 'RightReductive'
-- 
-- * 'RightCancellative'

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}

module Data.Semigroup.Cancellative (
   -- * Symmetric, commutative semigroup classes
   Commutative, Reductive(..), Cancellative,
   -- * Asymmetric semigroup classes
   LeftReductive(..), RightReductive(..),
   LeftCancellative, RightCancellative
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
class Semigroup m => Commutative m

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
class (Commutative m, LeftReductive m, RightReductive m) => Reductive m where
   (</>) :: m -> m -> Maybe m

infix 5 </>

-- | Subclass of 'Reductive' where '</>' is a complete inverse of the Semigroup '<>' operation. The class
-- instances must satisfy the following additional laws:
--
-- > (a <> b) </> a == Just b
-- > (a <> b) </> b == Just a
class (LeftCancellative m, RightCancellative m, Reductive m) => Cancellative m

-- | Class of semigroups with a left inverse of 'Data.Semigroup.<>', satisfying the following law:
-- 
-- > isPrefixOf a b == isJust (stripPrefix a b)
-- > maybe b (a <>) (stripPrefix a b) == b
-- > a `isPrefixOf` (a <> b)
-- 
-- | Every instance definition has to implement at least the 'stripPrefix' method. Its complexity should be no worse
-- than linear in the length of the prefix argument.
class Semigroup m => LeftReductive m where
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
class Semigroup m => RightReductive m where
   isSuffixOf :: m -> m -> Bool
   stripSuffix :: m -> m -> Maybe m

   isSuffixOf a b = isJust (stripSuffix a b)
   {-# MINIMAL stripSuffix #-}

-- | Subclass of 'LeftReductive' where 'stripPrefix' is a complete inverse of '<>', satisfying the following
-- additional law:
--
-- > stripPrefix a (a <> b) == Just b
class LeftReductive m => LeftCancellative m

-- | Subclass of 'LeftReductive' where 'stripPrefix' is a complete inverse of '<>', satisfying the following
-- additional law:
--
-- > stripSuffix b (a <> b) == Just a
class RightReductive m => RightCancellative m

-- Unit instances

instance Commutative ()

instance Reductive () where
   () </> () = Just ()

instance Cancellative ()

instance LeftReductive () where
   stripPrefix () () = Just ()

instance RightReductive () where
   stripSuffix () () = Just ()

instance LeftCancellative ()

instance RightCancellative ()

-- Dual instances

instance Commutative a => Commutative (Dual a)

instance Reductive a => Reductive (Dual a) where
   Dual a </> Dual b = fmap Dual (a </> b)

instance Cancellative a => Cancellative (Dual a)

instance LeftReductive a => RightReductive (Dual a) where
   stripSuffix (Dual a) (Dual b) = fmap Dual (stripPrefix a b)
   Dual a `isSuffixOf` Dual b = a `isPrefixOf` b

instance RightReductive a => LeftReductive (Dual a) where
   stripPrefix (Dual a) (Dual b) = fmap Dual (stripSuffix a b)
   Dual a `isPrefixOf` Dual b = a `isSuffixOf` b

instance LeftCancellative a => RightCancellative (Dual a)

instance RightCancellative a => LeftCancellative (Dual a)

-- Sum instances

instance Num a => Commutative (Sum a)

instance Integral a => Reductive (Sum a) where
   Sum a </> Sum b = Just $ Sum (a - b)

instance Integral a => Cancellative (Sum a)

instance Integral a => LeftReductive (Sum a) where
   stripPrefix a b = b </> a

instance Integral a => RightReductive (Sum a) where
   stripSuffix a b = b </> a

instance Integral a => LeftCancellative (Sum a)

instance Integral a => RightCancellative (Sum a)

instance {-# OVERLAPS #-} Reductive (Sum Natural) where
   Sum a </> Sum b
      | a < b = Nothing
      | otherwise = Just $ Sum (a - b)

instance {-# OVERLAPS #-} LeftReductive (Sum Natural) where
   stripPrefix a b = b </> a

instance {-# OVERLAPS #-} RightReductive (Sum Natural) where
   stripSuffix a b = b </> a

-- Product instances

instance Num a => Commutative (Product a)

instance Integral a => Reductive (Product a) where
   Product 0 </> Product 0 = Just (Product 0)
   Product _ </> Product 0 = Nothing
   Product a </> Product b = if remainder == 0 then Just (Product quotient) else Nothing
      where (quotient, remainder) = quotRem a b

instance Integral a => LeftReductive (Product a) where
   stripPrefix a b = b </> a

instance Integral a => RightReductive (Product a) where
   stripSuffix a b = b </> a

-- Pair instances

instance (Commutative a, Commutative b) => Commutative (a, b)

instance (Reductive a, Reductive b) => Reductive (a, b) where
   (a, b) </> (c, d) = case (a </> c, b </> d)
                       of (Just a', Just b') -> Just (a', b')
                          _ -> Nothing

instance (Cancellative a, Cancellative b) => Cancellative (a, b)

instance (LeftReductive a, LeftReductive b) => LeftReductive (a, b) where
   stripPrefix (a, b) (c, d) = case (stripPrefix a c, stripPrefix b d)
                               of (Just a', Just b') -> Just (a', b')
                                  _ -> Nothing
   isPrefixOf (a, b) (c, d) = isPrefixOf a c && isPrefixOf b d

instance (RightReductive a, RightReductive b) => RightReductive (a, b) where
   stripSuffix (a, b) (c, d) = case (stripSuffix a c, stripSuffix b d)
                               of (Just a', Just b') -> Just (a', b')
                                  _ -> Nothing
   isSuffixOf (a, b) (c, d) = isSuffixOf a c && isSuffixOf b d

instance (LeftCancellative a, LeftCancellative b) => LeftCancellative (a, b)

instance (RightCancellative a, RightCancellative b) => RightCancellative (a, b)

-- Triple instances

instance (Commutative a, Commutative b, Commutative c) => Commutative (a, b, c)

instance (Reductive a, Reductive b, Reductive c) => Reductive (a, b, c) where
   (a1, b1, c1) </> (a2, b2, c2) = (,,) <$> (a1 </> a2) <*> (b1 </> b2) <*> (c1 </> c2)

instance (Cancellative a, Cancellative b, Cancellative c) => Cancellative (a, b, c)

instance (LeftReductive a, LeftReductive b, LeftReductive c) => LeftReductive (a, b, c) where
   stripPrefix (a1, b1, c1) (a2, b2, c2) = (,,) <$> stripPrefix a1 a2 <*> stripPrefix b1 b2 <*> stripPrefix c1 c2
   isPrefixOf (a1, b1, c1) (a2, b2, c2) = isPrefixOf a1 a2 && isPrefixOf b1 b2 && isPrefixOf c1 c2

instance (RightReductive a, RightReductive b, RightReductive c) => RightReductive (a, b, c) where
   stripSuffix (a1, b1, c1) (a2, b2, c2) = (,,) <$> stripSuffix a1 a2 <*> stripSuffix b1 b2 <*> stripSuffix c1 c2
   isSuffixOf (a1, b1, c1) (a2, b2, c2) = isSuffixOf a1 a2 && isSuffixOf b1 b2 && isSuffixOf c1 c2

instance (LeftCancellative a, LeftCancellative b, LeftCancellative c) => LeftCancellative (a, b, c)

instance (RightCancellative a, RightCancellative b, RightCancellative c) => RightCancellative (a, b, c)

-- Quadruple instances

instance (Commutative a, Commutative b, Commutative c, Commutative d) => Commutative (a, b, c, d)

instance (Reductive a, Reductive b, Reductive c, Reductive d) => Reductive (a, b, c, d) where
   (a1, b1, c1, d1) </> (a2, b2, c2, d2) = (,,,) <$> (a1 </> a2) <*> (b1 </> b2) <*> (c1 </> c2) <*> (d1 </> d2)

instance (Cancellative a, Cancellative b, Cancellative c, Cancellative d) => Cancellative (a, b, c, d)

instance (LeftReductive a, LeftReductive b, LeftReductive c, LeftReductive d) => LeftReductive (a, b, c, d) where
   stripPrefix (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (,,,) <$> stripPrefix a1 a2 <*> stripPrefix b1 b2 <*> stripPrefix c1 c2 <*> stripPrefix d1 d2
   isPrefixOf (a1, b1, c1, d1) (a2, b2, c2, d2) =
      isPrefixOf a1 a2 && isPrefixOf b1 b2 && isPrefixOf c1 c2 && isPrefixOf d1 d2

instance (RightReductive a, RightReductive b, RightReductive c, RightReductive d) => RightReductive (a, b, c, d) where
   stripSuffix (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (,,,) <$> stripSuffix a1 a2 <*> stripSuffix b1 b2 <*> stripSuffix c1 c2 <*> stripSuffix d1 d2
   isSuffixOf (a1, b1, c1, d1) (a2, b2, c2, d2) =
      isSuffixOf a1 a2 && isSuffixOf b1 b2 && isSuffixOf c1 c2 && isSuffixOf d1 d2

instance (LeftCancellative a, LeftCancellative b,
          LeftCancellative c, LeftCancellative d) => LeftCancellative (a, b, c, d)

instance (RightCancellative a, RightCancellative b,
          RightCancellative c, RightCancellative d) => RightCancellative (a, b, c, d)

-- Maybe instances

instance LeftReductive x => LeftReductive (Maybe x) where
   stripPrefix Nothing y = Just y
   stripPrefix Just{} Nothing = Nothing
   stripPrefix (Just x) (Just y) = fmap Just $ stripPrefix x y

instance RightReductive x => RightReductive (Maybe x) where
   stripSuffix Nothing y = Just y
   stripSuffix Just{} Nothing = Nothing
   stripSuffix (Just x) (Just y) = fmap Just $ stripSuffix x y

-- Set instances

instance Ord a => Commutative (Set.Set a)

instance Ord a => LeftReductive (Set.Set a) where
   isPrefixOf = Set.isSubsetOf
   stripPrefix a b = b </> a

instance Ord a => RightReductive (Set.Set a) where
   isSuffixOf = Set.isSubsetOf
   stripSuffix a b = b </> a

instance Ord a => Reductive (Set.Set a) where
   a </> b | Set.isSubsetOf b a = Just (a Set.\\ b)
           | otherwise = Nothing

-- IntSet instances

instance Commutative IntSet.IntSet

instance LeftReductive IntSet.IntSet where
   isPrefixOf = IntSet.isSubsetOf
   stripPrefix a b = b </> a

instance RightReductive IntSet.IntSet where
   isSuffixOf = IntSet.isSubsetOf
   stripSuffix a b = b </> a

instance Reductive IntSet.IntSet where
   a </> b | IntSet.isSubsetOf b a = Just (a IntSet.\\ b)
           | otherwise = Nothing

-- Map instances

instance (Ord k, Eq a) => LeftReductive (Map.Map k a) where
   isPrefixOf = Map.isSubmapOf
   stripPrefix a b | Map.isSubmapOf a b = Just (b Map.\\ a)
                   | otherwise = Nothing

instance (Ord k, Eq a) => RightReductive (Map.Map k a) where
   isSuffixOf = Map.isSubmapOfBy (const $ const True)
   stripSuffix a b | a `isSuffixOf` b = Just (Map.differenceWith (\x y-> if x == y then Nothing else Just x) b a)
                   | otherwise = Nothing

-- IntMap instances

instance Eq a => LeftReductive (IntMap.IntMap a) where
   isPrefixOf = IntMap.isSubmapOf
   stripPrefix a b | IntMap.isSubmapOf a b = Just (b IntMap.\\ a)
                   | otherwise = Nothing

instance Eq a => RightReductive (IntMap.IntMap a) where
   isSuffixOf = IntMap.isSubmapOfBy (const $ const True)
   stripSuffix a b | a `isSuffixOf` b = Just (IntMap.differenceWith (\x y-> if x == y then Nothing else Just x) b a)
                   | otherwise = Nothing

-- List instances

instance Eq x => LeftReductive [x] where
   stripPrefix = List.stripPrefix
   isPrefixOf = List.isPrefixOf

instance Eq x => LeftCancellative [x]

-- Seq instances

instance Eq a => LeftReductive (Sequence.Seq a) where
   stripPrefix p s | p == s1 = Just s2
                   | otherwise = Nothing
      where (s1, s2) = Sequence.splitAt (Sequence.length p) s

instance Eq a => RightReductive (Sequence.Seq a) where
   stripSuffix p s | p == s2 = Just s1
                   | otherwise = Nothing
      where (s1, s2) = Sequence.splitAt (Sequence.length s - Sequence.length p) s

instance Eq a => LeftCancellative (Sequence.Seq a)

instance Eq a => RightCancellative (Sequence.Seq a)

-- Vector instances

instance Eq a => LeftReductive (Vector.Vector a) where
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

instance Eq a => RightReductive (Vector.Vector a) where
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

instance Eq a => LeftCancellative (Vector.Vector a)

instance Eq a => RightCancellative (Vector.Vector a)

-- ByteString instances

instance LeftReductive ByteString.ByteString where
   stripPrefix p l = if ByteString.isPrefixOf p l
                     then Just (ByteString.unsafeDrop (ByteString.length p) l)
                     else Nothing
   isPrefixOf = ByteString.isPrefixOf

instance RightReductive ByteString.ByteString where
   stripSuffix s l = if ByteString.isSuffixOf s l
                     then Just (ByteString.unsafeTake (ByteString.length l - ByteString.length s) l)
                     else Nothing
   isSuffixOf = ByteString.isSuffixOf

instance LeftCancellative ByteString.ByteString

instance RightCancellative ByteString.ByteString

-- Lazy ByteString instances

instance LeftReductive LazyByteString.ByteString where
   stripPrefix p l = if LazyByteString.isPrefixOf p l
                     then Just (LazyByteString.drop (LazyByteString.length p) l)
                     else Nothing
   isPrefixOf = LazyByteString.isPrefixOf

instance RightReductive LazyByteString.ByteString where
   stripSuffix s l = if LazyByteString.isSuffixOf s l
                     then Just (LazyByteString.take (LazyByteString.length l - LazyByteString.length s) l)
                     else Nothing
   isSuffixOf = LazyByteString.isSuffixOf

instance LeftCancellative LazyByteString.ByteString

instance RightCancellative LazyByteString.ByteString

-- Text instances

instance LeftReductive Text.Text where
   stripPrefix = Text.stripPrefix
   isPrefixOf = Text.isPrefixOf

instance RightReductive Text.Text where
   stripSuffix = Text.stripSuffix
   isSuffixOf = Text.isSuffixOf

instance LeftCancellative Text.Text

instance RightCancellative Text.Text

-- Lazy Text instances

instance LeftReductive LazyText.Text where
   stripPrefix = LazyText.stripPrefix
   isPrefixOf = LazyText.isPrefixOf

instance RightReductive LazyText.Text where
   stripSuffix = LazyText.stripSuffix
   isSuffixOf = LazyText.isSuffixOf

instance LeftCancellative LazyText.Text

instance RightCancellative LazyText.Text
