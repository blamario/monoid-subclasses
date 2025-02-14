{-
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'GCDMonoid' subclass of the 'Monoid' class.
--
-- The 'GCDMonoid' subclass adds the 'gcd' operation which takes two monoidal arguments and finds their greatest
-- common divisor, or (more generally) the greatest monoid that can be extracted with the '</>' operation from both.
--
-- The 'GCDMonoid' class is for Abelian, /i.e./, 'Commutative' monoids.
--
-- == Non-commutative GCD monoids
--
--  Since most practical monoids in Haskell are not Abelian, the 'GCDMonoid'
--  class has three symmetric superclasses:
--
-- * 'LeftGCDMonoid'
--
--      Class of monoids for which it is possible to find the greatest common
--      /prefix/ of two monoidal values.
--
-- * 'RightGCDMonoid'
--
--      Class of monoids for which it is possible to find the greatest common
--      /suffix/ of two monoidal values.
--
-- * 'OverlappingGCDMonoid'
--
--      Class of monoids for which it is possible to find the greatest common
--      /overlap/ of two monoidal values.
--
-- == Distributive GCD monoids
--
-- Since some (but not all) GCD monoids are also distributive, there are three
-- subclasses that add distributivity:
--
-- * 'DistributiveGCDMonoid'
--
--     Subclass of 'GCDMonoid' with /symmetric/ distributivity.
--
-- * 'LeftDistributiveGCDMonoid'
--
--     Subclass of 'LeftGCDMonoid' with /left/-distributivity.
--
-- * 'RightDistributiveGCDMonoid'
--
--     Subclass of 'RightGCDMonoid' with /right/-distributivity.
--
{-# LANGUAGE CPP, Haskell2010, FlexibleInstances, Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Monoid.GCD
    ( GCDMonoid (..)
    , LeftGCDMonoid (..)
    , RightGCDMonoid (..)
    , OverlappingGCDMonoid (..)
    , DistributiveGCDMonoid
    , LeftDistributiveGCDMonoid
    , RightDistributiveGCDMonoid
    )
    where

import qualified Prelude

import Data.Functor.Const (Const (Const))
import Data.Functor.Identity (Identity (Identity))
import Data.Monoid -- (Monoid, Dual(..), Sum(..), Product(..))
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Unsafe as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Internal as Internal
import qualified Data.Text.Internal.Lazy as LazyInternal
import           Data.Text.Unsafe (reverseIter)
#if MIN_VERSION_text(2,0,0)
import           Data.Text.Unsafe (Iter(..))
#endif
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Data.Sequence (ViewL((:<)), ViewR((:>)), (<|), (|>))
import qualified Data.Vector as Vector
import Numeric.Natural (Natural)

import Data.Semigroup.Cancellative
import Data.Monoid.Monus

-- These imports are marked as redundant, but are actually required by haddock:
import Data.Maybe (isJust)

import Prelude hiding (gcd)

-- | Class of Abelian monoids that allow the greatest common divisor to be found for any two given values. The
-- operations must satisfy the following laws:
--
-- > gcd a b == commonPrefix a b == commonSuffix a b
-- > Just a' = a </> p && Just b' = b </> p
-- >    where p = gcd a b
--
-- In addition, the 'gcd' operation must satisfy the following properties:
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ a '</>' c
--     , b '</>' c
--     , c '</>' 'gcd' a b
--     ]
-- ==>
--     (c '==' 'gcd' a b)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'gcd' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'gcd' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'gcd' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'gcd' a b '==' 'gcd' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'gcd' ('gcd' a b) c '==' 'gcd' a ('gcd' b c)
-- @
--
class (Monoid m, Commutative m, Reductive m, LeftGCDMonoid m, RightGCDMonoid m, OverlappingGCDMonoid m) => GCDMonoid m where
   gcd :: m -> m -> m

-- | Class of monoids capable of finding the equivalent of greatest common divisor on the left side of two monoidal
-- values. The following laws must be respected:
--
-- > stripCommonPrefix a b == (p, a', b')
-- >    where p = commonPrefix a b
-- >          Just a' = stripPrefix p a
-- >          Just b' = stripPrefix p b
-- > p == commonPrefix a b && p <> a' == a && p <> b' == b
-- >    where (p, a', b') = stripCommonPrefix a b
--
-- Furthermore, 'commonPrefix' must return the unique greatest common prefix that contains, as its prefix, any other
-- prefix @x@ of both values:
--
-- > not (x `isPrefixOf` a && x `isPrefixOf` b) || x `isPrefixOf` commonPrefix a b
--
-- and it cannot itself be a suffix of any other common prefix @y@ of both values:
--
-- > not (y `isPrefixOf` a && y `isPrefixOf` b && commonPrefix a b `isSuffixOf` y)
--
-- In addition, the 'commonPrefix' operation must satisfy the following
-- properties:
--
-- __/Idempotence/__
--
-- @
-- 'commonPrefix' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'commonPrefix' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'commonPrefix' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'commonPrefix' a b '==' 'commonPrefix' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'commonPrefix' ('commonPrefix' a b) c
-- '=='
-- 'commonPrefix' a ('commonPrefix' b c)
-- @
--
class (Monoid m, LeftReductive m) => LeftGCDMonoid m where
   commonPrefix :: m -> m -> m
   stripCommonPrefix :: m -> m -> (m, m, m)

   commonPrefix x y = p
      where (p, _, _) = stripCommonPrefix x y
   stripCommonPrefix x y = (p, x', y')
      where p = commonPrefix x y
            Just x' = stripPrefix p x
            Just y' = stripPrefix p y
   {-# MINIMAL commonPrefix | stripCommonPrefix #-}

-- | Class of monoids capable of finding the equivalent of greatest common divisor on the right side of two monoidal
-- values. The following laws must be respected:
--
-- > stripCommonSuffix a b == (a', b', s)
-- >    where s = commonSuffix a b
-- >          Just a' = stripSuffix p a
-- >          Just b' = stripSuffix p b
-- > s == commonSuffix a b && a' <> s == a && b' <> s == b
-- >    where (a', b', s) = stripCommonSuffix a b
--
-- Furthermore, 'commonSuffix' must return the unique greatest common suffix that contains, as its suffix, any other
-- suffix @x@ of both values:
--
-- > not (x `isSuffixOf` a && x `isSuffixOf` b) || x `isSuffixOf` commonSuffix a b
--
-- and it cannot itself be a prefix of any other common suffix @y@ of both values:
--
-- > not (y `isSuffixOf` a && y `isSuffixOf` b && commonSuffix a b `isPrefixOf` y)
--
-- In addition, the 'commonSuffix' operation must satisfy the following
-- properties:
--
-- __/Idempotence/__
--
-- @
-- 'commonSuffix' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'commonSuffix' 'mempty' a '==' 'mempty'
-- @
-- @
-- 'commonSuffix' a 'mempty' '==' 'mempty'
-- @
--
-- __/Commutativity/__
--
-- @
-- 'commonSuffix' a b '==' 'commonSuffix' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'commonSuffix' ('commonSuffix' a b) c
-- '=='
-- 'commonSuffix' a ('commonSuffix' b c)
-- @
--
class (Monoid m, RightReductive m) => RightGCDMonoid m where
   commonSuffix :: m -> m -> m
   stripCommonSuffix :: m -> m -> (m, m, m)

   commonSuffix x y = s
      where (_, _, s) = stripCommonSuffix x y
   stripCommonSuffix x y = (x', y', s)
      where s = commonSuffix x y
            Just x' = stripSuffix s x
            Just y' = stripSuffix s y
   {-# MINIMAL commonSuffix | stripCommonSuffix #-}

-- Unit instances

-- | /O(1)/
instance GCDMonoid () where
   gcd () () = ()

-- | /O(1)/
instance LeftGCDMonoid () where
   commonPrefix () () = ()

-- | /O(1)/
instance RightGCDMonoid () where
   commonSuffix () () = ()

-- Identity instances

deriving instance GCDMonoid a => GCDMonoid (Identity a)
deriving instance LeftGCDMonoid a => LeftGCDMonoid (Identity a)
deriving instance RightGCDMonoid a => RightGCDMonoid (Identity a)

-- Const instances

deriving instance GCDMonoid a => GCDMonoid (Const a b)
deriving instance LeftGCDMonoid a => LeftGCDMonoid (Const a b)
deriving instance RightGCDMonoid a => RightGCDMonoid (Const a b)

-- Dual instances

instance GCDMonoid a => GCDMonoid (Dual a) where
   gcd (Dual a) (Dual b) = Dual (gcd a b)

instance LeftGCDMonoid a => RightGCDMonoid (Dual a) where
   commonSuffix (Dual a) (Dual b) = Dual (commonPrefix a b)

instance RightGCDMonoid a => LeftGCDMonoid (Dual a) where
   commonPrefix (Dual a) (Dual b) = Dual (commonSuffix a b)

-- Sum instances

-- | /O(1)/
instance GCDMonoid (Sum Natural) where
   gcd (Sum a) (Sum b) = Sum (min a b)

-- | /O(1)/
instance LeftGCDMonoid (Sum Natural) where
   commonPrefix a b = gcd a b

-- | /O(1)/
instance RightGCDMonoid (Sum Natural) where
   commonSuffix a b = gcd a b

-- Product instances

-- | /O(1)/
instance GCDMonoid (Product Natural) where
   gcd (Product a) (Product b) = Product (Prelude.gcd a b)

-- | /O(1)/
instance LeftGCDMonoid (Product Natural) where
   commonPrefix a b = gcd a b

-- | /O(1)/
instance RightGCDMonoid (Product Natural) where
   commonSuffix a b = gcd a b

-- Pair instances

instance (GCDMonoid a, GCDMonoid b) => GCDMonoid (a, b) where
   gcd (a, b) (c, d) = (gcd a c, gcd b d)

instance (LeftGCDMonoid a, LeftGCDMonoid b) => LeftGCDMonoid (a, b) where
   commonPrefix (a, b) (c, d) = (commonPrefix a c, commonPrefix b d)

instance (RightGCDMonoid a, RightGCDMonoid b) => RightGCDMonoid (a, b) where
   commonSuffix (a, b) (c, d) = (commonSuffix a c, commonSuffix b d)

-- Triple instances

instance (GCDMonoid a, GCDMonoid b, GCDMonoid c) => GCDMonoid (a, b, c) where
   gcd (a1, b1, c1) (a2, b2, c2) = (gcd a1 a2, gcd b1 b2, gcd c1 c2)

instance (LeftGCDMonoid a, LeftGCDMonoid b, LeftGCDMonoid c) => LeftGCDMonoid (a, b, c) where
   commonPrefix (a1, b1, c1) (a2, b2, c2) = (commonPrefix a1 a2, commonPrefix b1 b2, commonPrefix c1 c2)

instance (RightGCDMonoid a, RightGCDMonoid b, RightGCDMonoid c) => RightGCDMonoid (a, b, c) where
   commonSuffix (a1, b1, c1) (a2, b2, c2) = (commonSuffix a1 a2, commonSuffix b1 b2, commonSuffix c1 c2)

-- Quadruple instances

instance (GCDMonoid a, GCDMonoid b, GCDMonoid c, GCDMonoid d) => GCDMonoid (a, b, c, d) where
   gcd (a1, b1, c1, d1) (a2, b2, c2, d2) = (gcd a1 a2, gcd b1 b2, gcd c1 c2, gcd d1 d2)

instance (LeftGCDMonoid a, LeftGCDMonoid b, LeftGCDMonoid c, LeftGCDMonoid d) => LeftGCDMonoid (a, b, c, d) where
   commonPrefix (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (commonPrefix a1 a2, commonPrefix b1 b2, commonPrefix c1 c2, commonPrefix d1 d2)

instance (RightGCDMonoid a, RightGCDMonoid b, RightGCDMonoid c, RightGCDMonoid d) => RightGCDMonoid (a, b, c, d) where
   commonSuffix (a1, b1, c1, d1) (a2, b2, c2, d2) =
      (commonSuffix a1 a2, commonSuffix b1 b2, commonSuffix c1 c2, commonSuffix d1 d2)

-- Maybe instances

instance LeftGCDMonoid x => LeftGCDMonoid (Maybe x) where
   commonPrefix (Just x) (Just y) = Just (commonPrefix x y)
   commonPrefix _ _ = Nothing

   stripCommonPrefix (Just x) (Just y) = (Just p, Just x', Just y')
      where (p, x', y') = stripCommonPrefix x y
   stripCommonPrefix x y = (Nothing, x, y)

instance RightGCDMonoid x => RightGCDMonoid (Maybe x) where
   commonSuffix (Just x) (Just y) = Just (commonSuffix x y)
   commonSuffix _ _ = Nothing

   stripCommonSuffix (Just x) (Just y) = (Just x', Just y', Just s)
      where (x', y', s) = stripCommonSuffix x y
   stripCommonSuffix x y = (x, y, Nothing)

-- Set instances

-- | /O(m*log(n\/m + 1)), m <= n/
instance Ord a => LeftGCDMonoid (Set.Set a) where
   commonPrefix = Set.intersection

-- | /O(m*log(n\/m + 1)), m <= n/
instance Ord a => RightGCDMonoid (Set.Set a) where
   commonSuffix = Set.intersection

-- | /O(m*log(n\/m + 1)), m <= n/
instance Ord a => GCDMonoid (Set.Set a) where
   gcd = Set.intersection

-- IntSet instances

-- | /O(m+n)/
instance LeftGCDMonoid IntSet.IntSet where
   commonPrefix = IntSet.intersection

-- | /O(m+n)/
instance RightGCDMonoid IntSet.IntSet where
   commonSuffix = IntSet.intersection

-- | /O(m+n)/
instance GCDMonoid IntSet.IntSet where
   gcd = IntSet.intersection

-- Map instances

-- | /O(m+n)/
instance (Ord k, Eq a) => LeftGCDMonoid (Map.Map k a) where
   commonPrefix = Map.mergeWithKey (\_ a b -> if a == b then Just a else Nothing) (const Map.empty) (const Map.empty)

-- IntMap instances

-- | /O(m+n)/
instance Eq a => LeftGCDMonoid (IntMap.IntMap a) where
   commonPrefix = IntMap.mergeWithKey (\_ a b -> if a == b then Just a else Nothing)
                                       (const IntMap.empty) (const IntMap.empty)

-- List instances

-- | /O(prefixLength)/
instance Eq x => LeftGCDMonoid [x] where
   commonPrefix (x:xs) (y:ys) | x == y = x : commonPrefix xs ys
   commonPrefix _ _ = []

   stripCommonPrefix x0 y0 = strip' id x0 y0
      where strip' f (x:xs) (y:ys) | x == y = strip' (f . (x :)) xs ys
            strip' f x y = (f [], x, y)

-- | @since 1.0
-- /O(m+n)/
instance Eq x => RightGCDMonoid [x] where
   stripCommonSuffix x0 y0 = go1 x0 y0
      where go1 (_:xs) (_:ys) = go1 xs ys
            go1 [] [] = go2 id id id x0 y0
            go1 [] ys = go2 id yp id x0 yr
               where (yp, yr) = splitAtLengthOf id ys y0
            go1 xs [] = go2 xp id id xr y0
               where (xp, xr) = splitAtLengthOf id xs x0
            go2 xp yp cs [] [] = (xp [], yp [], cs [])
            go2 xp yp cs (x:xs) (y:ys)
               | x == y = go2 xp yp (cs . (x:)) xs ys
               | otherwise = go2 (xp . cs . (x:)) (yp . cs . (y:)) id xs ys
            go2 _ _ _ _ _ = error "impossible"
            splitAtLengthOf yp (_:xs) (y:ys) = splitAtLengthOf (yp . (y:)) xs ys
            splitAtLengthOf yp [] ys = (yp, ys)
            splitAtLengthOf _ _ _ = error "impossible"

-- Seq instances

-- | /O(prefixLength)/
instance Eq a => LeftGCDMonoid (Sequence.Seq a) where
   stripCommonPrefix = findCommonPrefix Sequence.empty
      where findCommonPrefix prefix a b = case (Sequence.viewl a, Sequence.viewl b)
                                          of (a1:<a', b1:<b') | a1 == b1 -> findCommonPrefix (prefix |> a1) a' b'
                                             _ -> (prefix, a, b)

-- | /O(suffixLength)/
instance Eq a => RightGCDMonoid (Sequence.Seq a) where
   stripCommonSuffix = findCommonSuffix Sequence.empty
      where findCommonSuffix suffix a b = case (Sequence.viewr a, Sequence.viewr b)
                                          of (a':>a1, b':>b1) | a1 == b1 -> findCommonSuffix (a1 <| suffix) a' b'
                                             _ -> (a, b, suffix)

-- Vector instances

-- | /O(prefixLength)/
instance Eq a => LeftGCDMonoid (Vector.Vector a) where
   stripCommonPrefix x y = (xp, xs, Vector.drop maxPrefixLength y)
      where maxPrefixLength = prefixLength 0 (Vector.length x `min` Vector.length y)
            prefixLength n len | n < len && x Vector.! n == y Vector.! n = prefixLength (succ n) len
            prefixLength n _ = n
            (xp, xs) = Vector.splitAt maxPrefixLength x

-- | /O(suffixLength)/
instance Eq a => RightGCDMonoid (Vector.Vector a) where
   stripCommonSuffix x y = findSuffix (Vector.length x - 1) (Vector.length y - 1)
      where findSuffix m n | m >= 0 && n >= 0 && x Vector.! m == y Vector.! n =
               findSuffix (pred m) (pred n)
            findSuffix m n = (Vector.take (succ m) x, yp, ys)
               where (yp, ys) = Vector.splitAt (succ n) y

-- ByteString instances

-- | /O(prefixLength)/
instance LeftGCDMonoid ByteString.ByteString where
   stripCommonPrefix x y = (xp, xs, ByteString.unsafeDrop maxPrefixLength y)
      where maxPrefixLength = prefixLength 0 (ByteString.length x `min` ByteString.length y)
            prefixLength n len | n < len,
                                 ByteString.unsafeIndex x n == ByteString.unsafeIndex y n =
                                    prefixLength (succ n) len
                               | otherwise = n
            (xp, xs) = ByteString.splitAt maxPrefixLength x

-- | /O(suffixLength)/
instance RightGCDMonoid ByteString.ByteString where
   stripCommonSuffix x y = findSuffix (ByteString.length x - 1) (ByteString.length y - 1)
      where findSuffix m n | m >= 0, n >= 0,
                             ByteString.unsafeIndex x m == ByteString.unsafeIndex y n =
                                findSuffix (pred m) (pred n)
                           | otherwise = let (yp, ys) = ByteString.splitAt (succ n) y
                                         in (ByteString.unsafeTake (succ m) x, yp, ys)

-- Lazy ByteString instances

-- | /O(prefixLength)/
instance LeftGCDMonoid LazyByteString.ByteString where
   stripCommonPrefix x y = (xp, xs, LazyByteString.drop maxPrefixLength y)
      where maxPrefixLength = prefixLength 0 (LazyByteString.length x `min` LazyByteString.length y)
            prefixLength n len | n < len && LazyByteString.index x n == LazyByteString.index y n =
               prefixLength (succ n) len
            prefixLength n _ = n
            (xp, xs) = LazyByteString.splitAt maxPrefixLength x

-- | /O(suffixLength)/
instance RightGCDMonoid LazyByteString.ByteString where
   stripCommonSuffix x y = findSuffix (LazyByteString.length x - 1) (LazyByteString.length y - 1)
      where findSuffix m n | m >= 0 && n >= 0 && LazyByteString.index x m == LazyByteString.index y n =
               findSuffix (pred m) (pred n)
            findSuffix m n = (LazyByteString.take (succ m) x, yp, ys)
               where (yp, ys) = LazyByteString.splitAt (succ n) y

-- Text instances

-- | /O(prefixLength)/
instance LeftGCDMonoid Text.Text where
   stripCommonPrefix x y = maybe (Text.empty, x, y) id (Text.commonPrefixes x y)

-- | @since 1.0
-- /O(suffixLength)/, except on GHCjs where it is /O(m+n)/
instance RightGCDMonoid Text.Text where
#if !ghcjs_HOST_OS
  stripCommonSuffix x@(Internal.Text xarr xoff xlen) y@(Internal.Text yarr yoff ylen) = go (pred xlen) (pred ylen)
      where go i j | i >= 0 && j >= 0 && xc == yc = go (i+xd) (j+yd)
                   | otherwise = (Internal.text xarr xoff (succ i),
                                  Internal.text yarr yoff (succ j),
                                  Internal.text xarr (xoff+i+1) (xlen-i-1))
#if MIN_VERSION_text(2,0,0)
               where Iter xc xd = reverseIter x i
                     Iter yc yd = reverseIter y j
#else
               where (xc, xd) = reverseIter x i
                     (yc, yd) = reverseIter y j
#endif
#else
  stripCommonSuffix x y =
    let (xlist, ylist, slist) =
          stripCommonSuffix (TextEncoding.encodeUtf8 x) (TextEncoding.encodeUtf8 y)
    in (TextEncoding.decodeUtf8 xlist, TextEncoding.decodeUtf8 ylist, TextEncoding.decodeUtf8 slist)
#endif

-- Lazy Text instances

-- | /O(prefixLength)/
instance LeftGCDMonoid LazyText.Text where
   stripCommonPrefix x y = maybe (LazyText.empty, x, y) id (LazyText.commonPrefixes x y)

-- | @since 1.0
-- /O(m+n)/
instance RightGCDMonoid LazyText.Text where
#if !ghcjs_HOST_OS
   stripCommonSuffix x0 y0
      | x0len < y0len = go id y0p id x0 y0s
      | x0len > y0len = go x0p id id x0s y0
      | otherwise = go id id id x0 y0
      where (y0p, y0s) = splitWord16 id (y0len - x0len) y0
            (x0p, x0s) = splitWord16 id (x0len - y0len) x0
            x0len = lazyLengthWord16 x0
            y0len = lazyLengthWord16 y0
            lazyLengthWord16 = LazyText.foldlChunks addLength 0
            addLength n x = n + (\(Internal.Text _ _ l) -> l) x
            splitWord16 xp 0 x = (xp, x)
            splitWord16 xp n (LazyInternal.Chunk x@(Internal.Text arr off len) xs)
               | n < len = (xp . LazyInternal.chunk (Internal.Text arr off n),
                            LazyInternal.chunk (Internal.Text arr (off+n) (len-n)) xs)
               | otherwise = splitWord16 (xp . LazyInternal.chunk x) (n - len) xs
            splitWord16 _ _ LazyInternal.Empty = error "impossible"
            go xp yp cs LazyInternal.Empty LazyInternal.Empty = (xp mempty, yp mempty, cs mempty)
            go xp yp cs (LazyInternal.Chunk x@(Internal.Text xarr xoff xlen) xs)
                        (LazyInternal.Chunk y@(Internal.Text yarr yoff ylen) ys)
               | xlen < ylen = go xp yp cs (LazyInternal.Chunk x xs)
                                           (LazyInternal.Chunk (Internal.Text yarr yoff xlen) $
                                            LazyInternal.Chunk (Internal.Text yarr (yoff+xlen) (ylen-xlen)) ys)
               | xlen > ylen = go xp yp cs (LazyInternal.Chunk (Internal.Text xarr xoff ylen) $
                                            LazyInternal.Chunk (Internal.Text xarr (xoff+ylen) (xlen-ylen)) xs)
                                           (LazyInternal.Chunk y ys)
               | x == y = go xp yp (cs . LazyInternal.chunk x) xs ys
               | (x1p, y1p, c1s) <- stripCommonSuffix x y =
                    go (xp . cs . LazyInternal.chunk x1p) (yp . cs . LazyInternal.chunk y1p) (LazyInternal.chunk c1s) xs ys
            go _ _ _ _ _ = error "impossible"
#else
  stripCommonSuffix x y =
    let (xlist, ylist, slist) =
          stripCommonSuffix (LazyEncoding.encodeUtf8 x) (LazyEncoding.encodeUtf8 y)
    in (LazyEncoding.decodeUtf8 xlist, LazyEncoding.decodeUtf8 ylist, LazyEncoding.decodeUtf8 slist)
#endif

--------------------------------------------------------------------------------
-- DistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | Class of /commutative/ GCD monoids with /symmetric/ distributivity.
--
-- In addition to the general 'GCDMonoid' laws, instances of this class
-- must also satisfy the following laws:
--
-- @
-- 'gcd' (a '<>' b) (a '<>' c) '==' a '<>' 'gcd' b c
-- @
-- @
-- 'gcd' (a '<>' c) (b '<>' c) '==' 'gcd' a b '<>' c
-- @
--
class (LeftDistributiveGCDMonoid m, RightDistributiveGCDMonoid m, GCDMonoid m)
    => DistributiveGCDMonoid m

instance DistributiveGCDMonoid ()
instance DistributiveGCDMonoid a => DistributiveGCDMonoid (Identity a)
instance DistributiveGCDMonoid a => DistributiveGCDMonoid (Const a b)
instance DistributiveGCDMonoid (Product Natural)
instance DistributiveGCDMonoid (Sum Natural)
instance DistributiveGCDMonoid IntSet.IntSet
instance DistributiveGCDMonoid a => DistributiveGCDMonoid (Dual a)
instance Ord a => DistributiveGCDMonoid (Set.Set a)

-------------------------------------------------------------------------------
-- LeftDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | Class of /left/ GCD monoids with /left/-distributivity.
--
-- In addition to the general 'LeftGCDMonoid' laws, instances of this class
-- must also satisfy the following law:
--
-- @
-- 'commonPrefix' (a '<>' b) (a '<>' c) '==' a '<>' 'commonPrefix' b c
-- @
--
class LeftGCDMonoid m => LeftDistributiveGCDMonoid m

-- Instances for non-commutative monoids:
instance Eq a => LeftDistributiveGCDMonoid [a]
instance Eq a => LeftDistributiveGCDMonoid (Sequence.Seq a)
instance Eq a => LeftDistributiveGCDMonoid (Vector.Vector a)
instance LeftDistributiveGCDMonoid ByteString.ByteString
instance LeftDistributiveGCDMonoid LazyByteString.ByteString
instance LeftDistributiveGCDMonoid Text.Text
instance LeftDistributiveGCDMonoid LazyText.Text

-- Instances for commutative monoids:
instance LeftDistributiveGCDMonoid ()
instance LeftDistributiveGCDMonoid (Product Natural)
instance LeftDistributiveGCDMonoid (Sum Natural)
instance LeftDistributiveGCDMonoid IntSet.IntSet
instance Ord a => LeftDistributiveGCDMonoid (Set.Set a)

-- Instances for monoid transformers:
instance LeftDistributiveGCDMonoid a => LeftDistributiveGCDMonoid (Identity a)
instance LeftDistributiveGCDMonoid a => LeftDistributiveGCDMonoid (Const a b)
instance RightDistributiveGCDMonoid a => LeftDistributiveGCDMonoid (Dual a)

--------------------------------------------------------------------------------
-- RightDistributiveGCDMonoid
--------------------------------------------------------------------------------

-- | Class of /right/ GCD monoids with /right/-distributivity.
--
-- In addition to the general 'RightGCDMonoid' laws, instances of this class
-- must also satisfy the following law:
--
-- @
-- 'commonSuffix' (a '<>' c) (b '<>' c) '==' 'commonSuffix' a b '<>' c
-- @
--
class RightGCDMonoid m => RightDistributiveGCDMonoid m

-- Instances for non-commutative monoids:
instance Eq a => RightDistributiveGCDMonoid [a]
instance Eq a => RightDistributiveGCDMonoid (Sequence.Seq a)
instance Eq a => RightDistributiveGCDMonoid (Vector.Vector a)
instance RightDistributiveGCDMonoid ByteString.ByteString
instance RightDistributiveGCDMonoid LazyByteString.ByteString
instance RightDistributiveGCDMonoid Text.Text
instance RightDistributiveGCDMonoid LazyText.Text

-- Instances for commutative monoids:
instance RightDistributiveGCDMonoid ()
instance RightDistributiveGCDMonoid (Product Natural)
instance RightDistributiveGCDMonoid (Sum Natural)
instance RightDistributiveGCDMonoid IntSet.IntSet
instance Ord a => RightDistributiveGCDMonoid (Set.Set a)

-- Instances for monoid transformers:
instance RightDistributiveGCDMonoid a => RightDistributiveGCDMonoid (Identity a)
instance RightDistributiveGCDMonoid a => RightDistributiveGCDMonoid (Const a b)
instance LeftDistributiveGCDMonoid a => RightDistributiveGCDMonoid (Dual a)
