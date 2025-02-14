{-
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'Semigroup' => 'Reductive' => 'Cancellative' class hierarchy.
--
-- @since 1.0
--
-- The 'Reductive' class introduces operation '</>' which is the inverse of '<>'. For the 'Sum' semigroup, this
-- operation is subtraction; for 'Product' it is division and for 'Set' it's the set difference. A 'Reductive'
-- semigroup is not a full group because '</>' may return 'Nothing'.
--
-- The 'Cancellative' subclass does not add any operation but it provides the additional guarantee that '<>' can
-- always be undone with '</>'. Thus 'Sum' is 'Cancellative' but 'Product' is not because @(0*n)/0@ is not defined.
--
-- All semigroup subclasses listed above are for Abelian, /i.e./, commutative or symmetric semigroups. Since most
-- practical semigroups in Haskell are not Abelian, each of the these classes has two symmetric superclasses:
--
-- * 'LeftReductive'
--
-- * 'LeftCancellative'
--
-- * 'RightReductive'
--
-- * 'RightCancellative'

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Semigroup.Cancellative (
   -- * Symmetric, commutative semigroup classes
   Commutative, Reductive(..), Cancellative, SumCancellative(..),
   -- * Asymmetric semigroup classes
   LeftReductive(..), RightReductive(..),
   LeftCancellative, RightCancellative
   )
where

import Data.Functor.Const
import Data.Functor.Identity
import Data.Semigroup -- (Semigroup, Dual(..), Sum(..), Product(..))
import Data.Semigroup.Commutative
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
import Numeric.Product.Commutative (CommutativeProduct)

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
-- Every instance definition has to implement at least the 'stripPrefix' method.
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
-- Every instance definition has to implement at least the 'stripSuffix' method.
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

instance Reductive () where
   () </> () = Just ()

instance Cancellative ()

-- | /O(1)/
instance LeftReductive () where
   stripPrefix () () = Just ()

-- | /O(1)/
instance RightReductive () where
   stripSuffix () () = Just ()

instance LeftCancellative ()

instance RightCancellative ()

-- Dual instances

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

-- | Helper class to avoid @FlexibleInstances@
class Num a => SumCancellative a where
   cancelAddition :: a -> a -> Maybe a
   cancelAddition a b = Just (a - b)

instance SumCancellative Int
instance SumCancellative Integer
instance SumCancellative Rational

instance SumCancellative Natural where
   cancelAddition a b
      | a < b = Nothing
      | otherwise = Just (a - b)

-- | /O(1)/
instance SumCancellative a => Reductive (Sum a) where
   Sum a </> Sum b = Sum <$> cancelAddition a b

-- | /O(1)/
instance SumCancellative a => LeftReductive (Sum a) where
   stripPrefix a b = b </> a

-- | /O(1)/
instance SumCancellative a => RightReductive (Sum a) where
   stripSuffix a b = b </> a

instance SumCancellative a => Cancellative (Sum a)
instance SumCancellative a => LeftCancellative (Sum a)
instance SumCancellative a => RightCancellative (Sum a)

-- Product instances

instance (CommutativeProduct a, Integral a) => Reductive (Product a) where
   Product 0 </> Product 0 = Just (Product 0)
   Product _ </> Product 0 = Nothing
   Product a </> Product b = if remainder == 0 then Just (Product quotient) else Nothing
      where (quotient, remainder) = quotRem a b

instance (CommutativeProduct a, Integral a) => LeftReductive (Product a) where
   stripPrefix a b = b </> a

instance (CommutativeProduct a, Integral a) => RightReductive (Product a) where
   stripSuffix a b = b </> a

-- Max & Min instances

instance Ord a => Reductive (Max a) where
   a </> b = if b <= a then Just a else Nothing
instance Ord a => Reductive (Min a) where
   a </> b = if a <= b then Just a else Nothing

instance Ord a => LeftReductive (Max a) where
   isPrefixOf a b = a <= b
   stripPrefix a b = b </> a
instance Ord a => LeftReductive (Min a) where
   isPrefixOf a b = b <= a
   stripPrefix a b = b </> a

instance Ord a => RightReductive (Max a) where
   isSuffixOf a b = a <= b
   stripSuffix a b = b </> a
instance Ord a => RightReductive (Min a) where
   isSuffixOf a b = b <= a
   stripSuffix a b = b </> a

-- Any & All instances

instance Reductive Any where
   a </> b = if b <= a then Just a else Nothing
instance Reductive All where
   a </> b = if a <= b then Just a else Nothing

instance LeftReductive Any where
   isPrefixOf a b = a <= b
   stripPrefix a b = b </> a
instance LeftReductive All where
   isPrefixOf a b = b <= a
   stripPrefix a b = b </> a

instance RightReductive Any where
   isSuffixOf a b = a <= b
   stripSuffix a b = b </> a
instance RightReductive All where
   isSuffixOf a b = b <= a
   stripSuffix a b = b </> a

-- Identity & Const instances

deriving instance Reductive a => Reductive (Identity a)
deriving instance Reductive a => Reductive (Const a b)

instance Cancellative a => Cancellative (Identity a)
instance Cancellative a => Cancellative (Const a x)

deriving instance LeftReductive a => LeftReductive (Identity a)
deriving instance LeftReductive a => LeftReductive (Const a b)

deriving instance RightReductive a => RightReductive (Identity a)
deriving instance RightReductive a => RightReductive (Const a b)

instance LeftCancellative a => LeftCancellative (Identity a)
instance LeftCancellative a => LeftCancellative (Const a x)

instance RightCancellative a => RightCancellative (Identity a)
instance RightCancellative a => RightCancellative (Const a x)

-- Pair instances

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

-- | @since 1.0
instance Reductive x => Reductive (Maybe x) where
   Just x </> Just y = Just <$> x </> y
   x </> Nothing = Just x
   Nothing </> _ = Nothing

instance LeftReductive x => LeftReductive (Maybe x) where
   stripPrefix Nothing y = Just y
   stripPrefix Just{} Nothing = Nothing
   stripPrefix (Just x) (Just y) = fmap Just $ stripPrefix x y

instance RightReductive x => RightReductive (Maybe x) where
   stripSuffix Nothing y = Just y
   stripSuffix Just{} Nothing = Nothing
   stripSuffix (Just x) (Just y) = fmap Just $ stripSuffix x y

-- Set instances

-- | /O(m*log(n/m + 1)), m <= n/
instance Ord a => LeftReductive (Set.Set a) where
   isPrefixOf = Set.isSubsetOf
   stripPrefix a b = b </> a

-- | /O(m*log(n/m + 1)), m <= n/
instance Ord a => RightReductive (Set.Set a) where
   isSuffixOf = Set.isSubsetOf
   stripSuffix a b = b </> a

-- | /O(m*log(n/m + 1)), m <= n/
instance Ord a => Reductive (Set.Set a) where
   a </> b | Set.isSubsetOf b a = Just (a Set.\\ b)
           | otherwise = Nothing

-- IntSet instances

-- | /O(m+n)/
instance LeftReductive IntSet.IntSet where
   isPrefixOf = IntSet.isSubsetOf
   stripPrefix a b = b </> a

-- | /O(m+n)/
instance RightReductive IntSet.IntSet where
   isSuffixOf = IntSet.isSubsetOf
   stripSuffix a b = b </> a

-- | /O(m+n)/
instance Reductive IntSet.IntSet where
   a </> b | IntSet.isSubsetOf b a = Just (a IntSet.\\ b)
           | otherwise = Nothing

-- Map instances

-- | /O(m+n)/
instance (Ord k, Eq a) => LeftReductive (Map.Map k a) where
   isPrefixOf = Map.isSubmapOf
   stripPrefix a b | Map.isSubmapOf a b = Just (b Map.\\ a)
                   | otherwise = Nothing

-- | /O(m+n)/
instance (Ord k, Eq a) => RightReductive (Map.Map k a) where
   isSuffixOf = Map.isSubmapOfBy (const $ const True)
   stripSuffix a b | a `isSuffixOf` b = Just (Map.differenceWith (\x y-> if x == y then Nothing else Just x) b a)
                   | otherwise = Nothing

-- IntMap instances

-- | /O(m+n)/
instance Eq a => LeftReductive (IntMap.IntMap a) where
   isPrefixOf = IntMap.isSubmapOf
   stripPrefix a b | IntMap.isSubmapOf a b = Just (b IntMap.\\ a)
                   | otherwise = Nothing

-- | /O(m+n)/
instance Eq a => RightReductive (IntMap.IntMap a) where
   isSuffixOf = IntMap.isSubmapOfBy (const $ const True)
   stripSuffix a b | a `isSuffixOf` b = Just (IntMap.differenceWith (\x y-> if x == y then Nothing else Just x) b a)
                   | otherwise = Nothing

-- List instances

-- | /O(prefixLength)/
instance Eq x => LeftReductive [x] where
   stripPrefix = List.stripPrefix
   isPrefixOf = List.isPrefixOf

-- | @since 1.0
-- /O(m+n)/
instance Eq x => RightReductive [x] where
   isSuffixOf = List.isSuffixOf
   stripSuffix xs0 ys0 = go1 xs0 ys0
      where go1 (_:xs) (_:ys) = go1 xs ys
            go1 [] ys = go2 id ys ys0
            go1  _ [] = Nothing
            go2 fy (_:zs) (y:ys) = go2 (fy . (y:)) zs ys
            go2 fy [] ys
               | xs0 == ys = Just (fy [])
               | otherwise = Nothing
            go2 _ _ _ = error "impossible"

instance Eq x => LeftCancellative [x]

-- | @since 1.0
instance Eq x => RightCancellative [x]

-- Seq instances

-- | /O(log(min(m,n−m)) + prefixLength)/
instance Eq a => LeftReductive (Sequence.Seq a) where
   stripPrefix p s | p == s1 = Just s2
                   | otherwise = Nothing
      where (s1, s2) = Sequence.splitAt (Sequence.length p) s

-- | /O(log(min(m,n−m)) + suffixLength)/
instance Eq a => RightReductive (Sequence.Seq a) where
   stripSuffix p s | p == s2 = Just s1
                   | otherwise = Nothing
      where (s1, s2) = Sequence.splitAt (Sequence.length s - Sequence.length p) s

instance Eq a => LeftCancellative (Sequence.Seq a)

instance Eq a => RightCancellative (Sequence.Seq a)

-- Vector instances

-- | /O(n)/
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

-- | /O(n)/
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

-- | /O(n)/
instance LeftReductive ByteString.ByteString where
   stripPrefix p l = if ByteString.isPrefixOf p l
                     then Just (ByteString.unsafeDrop (ByteString.length p) l)
                     else Nothing
   isPrefixOf = ByteString.isPrefixOf

-- | /O(n)/
instance RightReductive ByteString.ByteString where
   stripSuffix s l = if ByteString.isSuffixOf s l
                     then Just (ByteString.unsafeTake (ByteString.length l - ByteString.length s) l)
                     else Nothing
   isSuffixOf = ByteString.isSuffixOf

instance LeftCancellative ByteString.ByteString

instance RightCancellative ByteString.ByteString

-- Lazy ByteString instances

-- | /O(n)/
instance LeftReductive LazyByteString.ByteString where
   stripPrefix p l = if LazyByteString.isPrefixOf p l
                     then Just (LazyByteString.drop (LazyByteString.length p) l)
                     else Nothing
   isPrefixOf = LazyByteString.isPrefixOf

-- | /O(n)/
instance RightReductive LazyByteString.ByteString where
   stripSuffix s l = if LazyByteString.isSuffixOf s l
                     then Just (LazyByteString.take (LazyByteString.length l - LazyByteString.length s) l)
                     else Nothing
   isSuffixOf = LazyByteString.isSuffixOf

instance LeftCancellative LazyByteString.ByteString

instance RightCancellative LazyByteString.ByteString

-- Text instances

-- | /O(n)/
instance LeftReductive Text.Text where
   stripPrefix = Text.stripPrefix
   isPrefixOf = Text.isPrefixOf

-- | /O(n)/
instance RightReductive Text.Text where
   stripSuffix = Text.stripSuffix
   isSuffixOf = Text.isSuffixOf

instance LeftCancellative Text.Text

instance RightCancellative Text.Text

-- Lazy Text instances

-- | /O(n)/
instance LeftReductive LazyText.Text where
   stripPrefix = LazyText.stripPrefix
   isPrefixOf = LazyText.isPrefixOf

-- | /O(n)/
instance RightReductive LazyText.Text where
   stripSuffix = LazyText.stripSuffix
   isSuffixOf = LazyText.isSuffixOf

instance LeftCancellative LazyText.Text

instance RightCancellative LazyText.Text
