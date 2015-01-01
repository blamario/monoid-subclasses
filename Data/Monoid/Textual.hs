{- 
    Copyright 2013 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'TextualMonoid' class and its most important instances for 'String' and 'Text'.
-- 

{-# LANGUAGE FlexibleInstances #-}

module Data.Monoid.Textual (
   TextualMonoid(..)
   )
where

import Prelude hiding (all, any, break, concatMap, dropWhile, foldl, foldl1, foldr, foldr1, map, scanl, scanl1, scanr, scanr1,
                       span, takeWhile)

import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable
import Data.Maybe (fromJust)
import Data.Either (rights)
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Text (Text)
import Data.Monoid (Monoid(mappend, mconcat, mempty))
import qualified Data.Sequence as Sequence
import qualified Data.Vector as Vector
import Data.String (IsString(fromString))
import Data.Int (Int64)

import Data.Monoid.Null (MonoidNull (null))
import Data.Monoid.Cancellative (LeftReductiveMonoid, LeftGCDMonoid)
import Data.Monoid.Factorial (FactorialMonoid)
import qualified Data.Monoid.Factorial as Factorial

-- | The 'TextualMonoid' class is an extension of 'FactorialMonoid' specialized for monoids that can contain
-- characters. Its methods are generally equivalent to their namesake functions from "Data.List" and "Data.Text", and
-- they satisfy the following laws:
-- 
-- > unfoldr splitCharacterPrefix . fromString == id
-- > splitCharacterPrefix . primePrefix == fmap (\(c, t)-> (c, mempty)) . splitCharacterPrefix
-- >
-- > map f . fromString == fromString . List.map f
-- > concatMap (fromString . f) . fromString == fromString . List.concatMap f
-- >
-- > foldl  ft fc a . fromString == List.foldl  fc a
-- > foldr  ft fc a . fromString == List.foldr  fc a
-- > foldl' ft fc a . fromString == List.foldl' fc a
-- >
-- > scanl f c . fromString == fromString . List.scanl f c
-- > scanr f c . fromString == fromString . List.scanr f c
-- > mapAccumL f a . fromString == fmap fromString . List.mapAccumL f a
-- > mapAccumL f a . fromString == fmap fromString . List.mapAccumL f a
-- >
-- > takeWhile pt pc . fromString == fromString . takeWhile pc
-- > dropWhile pt pc . fromString == fromString . dropWhile pc
-- >
-- > mconcat . intersperse (singleton c) . split (== c) == id
-- > find p . fromString == List.find p
-- > elem c . fromString == List.elem c
--
-- A 'TextualMonoid' may contain non-character data insterspersed between its characters. Every class method that
-- returns a modified 'TextualMonoid' instance generally preserves this non-character data. Methods like 'foldr' can
-- access both the non-character and character data and expect two arguments for the two purposes. For each of these
-- methods there is also a simplified version with underscore in name (like 'foldr_') that ignores the non-character
-- data.
--
-- All of the following expressions are identities:
--
-- > map id
-- > concatMap singleton
-- > foldl  (<>) (\a c-> a <> singleton c) mempty
-- > foldr  (<>) ((<>) . singleton) mempty
-- > foldl' (<>) (\a c-> a <> singleton c) mempty
-- > scanl1 (const id)
-- > scanr1 const
-- > uncurry (mapAccumL (,))
-- > uncurry (mapAccumR (,))
-- > takeWhile (const True) (const True)
-- > dropWhile (const False) (const False)
--
-- A minimal instance definition must implement 'splitCharacterPrefix'.

class (IsString t, LeftReductiveMonoid t, LeftGCDMonoid t, FactorialMonoid t) => TextualMonoid t where
   -- | Contructs a new data type instance Like 'fromString', but from a 'Text' input instead of 'String'.
   --
   -- > fromText == fromString . Text.unpack
   fromText :: Text -> t
   -- | Creates a prime monoid containing a single character.
   --
   -- > singleton c == fromString [c]
   singleton :: Char -> t
   -- | Specialized version of 'Factorial.splitPrimePrefix'. Every prime factor of a 'Textual' monoid must consist of a
   -- single character or no character at all.
   splitCharacterPrefix :: t -> Maybe (Char, t)
   -- | Extracts a single character that prefixes the monoid, if the monoid begins with a character. Otherwise returns
   -- 'Nothing'.
   --
   -- > characterPrefix == fmap fst . splitCharacterPrefix
   characterPrefix :: t -> Maybe Char
   -- | Equivalent to 'List.map' from "Data.List" with a @Char -> Char@ function. Preserves all non-character data.
   --
   -- > map f == concatMap (singleton . f)
   map :: (Char -> Char) -> t -> t
   -- | Equivalent to 'List.concatMap' from "Data.List" with a @Char -> String@ function. Preserves all non-character
   -- data.
   concatMap :: (Char -> t) -> t -> t
   -- | Equivalent to 'List.any' from "Data.List". Ignores all non-character data.
   any :: (Char -> Bool) -> t -> Bool
   -- | Equivalent to 'List.all' from "Data.List". Ignores all non-character data.
   all :: (Char -> Bool) -> t -> Bool

   -- | The first argument folds over the non-character prime factors, the second over characters. Otherwise equivalent
   -- to 'List.foldl' from "Data.List".
   foldl   :: (a -> t -> a) -> (a -> Char -> a) -> a -> t -> a
   -- | Strict version of 'foldl'.
   foldl'  :: (a -> t -> a) -> (a -> Char -> a) -> a -> t -> a
   -- | The first argument folds over the non-character prime factors, the second over characters. Otherwise equivalent
   -- to 'List.foldl\'' from "Data.List".
   foldr   :: (t -> a -> a) -> (Char -> a -> a) -> a -> t -> a

   -- | Equivalent to 'List.scanl' from "Data.List" when applied to a 'String', but preserves all non-character data.
   scanl :: (Char -> Char -> Char) -> Char -> t -> t
   -- | Equivalent to 'List.scanl1' from "Data.List" when applied to a 'String', but preserves all non-character data.
   --
   -- > scanl f c == scanl1 f . (singleton c <>)
   scanl1 :: (Char -> Char -> Char) -> t -> t
   -- | Equivalent to 'List.scanr' from "Data.List" when applied to a 'String', but preserves all non-character data.
   scanr :: (Char -> Char -> Char) -> Char -> t -> t
   -- | Equivalent to 'List.scanr1' from "Data.List" when applied to a 'String', but preserves all non-character data.
   --
   -- > scanr f c == scanr1 f . (<> singleton c)
   scanr1 :: (Char -> Char -> Char) -> t -> t
   -- | Equivalent to 'List.mapAccumL' from "Data.List" when applied to a 'String', but preserves all non-character
   -- data.
   mapAccumL :: (a -> Char -> (a, Char)) -> a -> t -> (a, t)
   -- | Equivalent to 'List.mapAccumR' from "Data.List" when applied to a 'String', but preserves all non-character
   -- data.
   mapAccumR :: (a -> Char -> (a, Char)) -> a -> t -> (a, t)

   -- | The first predicate tests the non-character data, the second one the characters. Otherwise equivalent to
   -- 'List.takeWhile' from "Data.List" when applied to a 'String'.
   takeWhile :: (t -> Bool) -> (Char -> Bool) -> t -> t
   -- | The first predicate tests the non-character data, the second one the characters. Otherwise equivalent to
   -- 'List.dropWhile' from "Data.List" when applied to a 'String'.
   dropWhile :: (t -> Bool) -> (Char -> Bool) -> t -> t
   -- | 'break pt pc' is equivalent to |span (not . pt) (not . pc)|.
   break :: (t -> Bool) -> (Char -> Bool) -> t -> (t, t)
   -- | 'span pt pc t' is equivalent to |(takeWhile pt pc t, dropWhile pt pc t)|.
   span :: (t -> Bool) -> (Char -> Bool) -> t -> (t, t)
   -- | A stateful variant of 'span', threading the result of the test function as long as it returns 'Just'.
   spanMaybe :: s -> (s -> t -> Maybe s) -> (s -> Char -> Maybe s) -> t -> (t, t, s)
   -- | Strict version of 'spanMaybe'.
   spanMaybe' :: s -> (s -> t -> Maybe s) -> (s -> Char -> Maybe s) -> t -> (t, t, s)
   -- | Splits the monoid into components delimited by character separators satisfying the given predicate. The
   -- characters satisfying the predicate are not a part of the result.
   --
   -- > split p == Factorial.split (maybe False p . characterPrefix)
   split :: (Char -> Bool) -> t -> [t]
   -- | Like 'List.find' from "Data.List" when applied to a 'String'. Ignores non-character data.
   find :: (Char -> Bool) -> t -> Maybe Char
   -- | Like 'List.elem' from "Data.List" when applied to a 'String'. Ignores non-character data.
   elem :: Char -> t -> Bool

   -- | > foldl_ = foldl const
   foldl_   :: (a -> Char -> a) -> a -> t -> a
   foldl_'  :: (a -> Char -> a) -> a -> t -> a
   foldr_   :: (Char -> a -> a) -> a -> t -> a
   -- | > takeWhile_ = takeWhile . const
   takeWhile_ :: Bool -> (Char -> Bool) -> t -> t
   -- | > dropWhile_ = dropWhile . const
   dropWhile_ :: Bool -> (Char -> Bool) -> t -> t
   -- | > break_ = break . const
   break_ :: Bool -> (Char -> Bool) -> t -> (t, t)
   -- | > span_ = span . const
   span_ :: Bool -> (Char -> Bool) -> t -> (t, t)
   -- | > spanMaybe_ s = spanMaybe s (const . Just)
   spanMaybe_ :: s -> (s -> Char -> Maybe s) -> t -> (t, t, s)
   spanMaybe_' :: s -> (s -> Char -> Maybe s) -> t -> (t, t, s)


   fromText = fromString . Text.unpack
   singleton = fromString . (:[])

   characterPrefix = fmap fst . splitCharacterPrefix

   map f = concatMap (singleton . f)
   concatMap f = foldr mappend (mappend . f) mempty
   all p = foldr (const id) ((&&) . p) True
   any p = foldr (const id) ((||) . p) False

   foldl ft fc = Factorial.foldl (\a prime-> maybe (ft a prime) (fc a) (characterPrefix prime))
   foldr ft fc = Factorial.foldr (\prime-> maybe (ft prime) fc (characterPrefix prime))
   foldl' ft fc = Factorial.foldl' (\a prime-> maybe (ft a prime) (fc a) (characterPrefix prime))
   foldl_ = foldl const
   foldr_ = foldr (const id)
   foldl_' = foldl' const

   scanl f c = mappend (singleton c) . fst . foldl foldlOther (foldlChars f) (mempty, c)
   scanl1 f t = case (Factorial.splitPrimePrefix t, splitCharacterPrefix t)
                of (Nothing, _) -> t
                   (Just (prefix, suffix), Nothing) -> mappend prefix (scanl1 f suffix)
                   (Just _, Just (c, suffix)) -> scanl f c suffix
   scanr f c = fst . foldr foldrOther (foldrChars f) (singleton c, c)
   scanr1 f = fst . foldr foldrOther fc (mempty, Nothing)
      where fc c (t, Nothing) = (mappend (singleton c) t, Just c)
            fc c1 (t, Just c2) = (mappend (singleton c') t, Just c')
               where c' = f c1 c2
   mapAccumL f a0 = foldl ft fc (a0, mempty)
      where ft (a, t1) t2 = (a, mappend t1 t2)
            fc (a, t) c = (a', mappend t (singleton c'))
               where (a', c') = f a c
   mapAccumR f a0 = foldr ft fc (a0, mempty)
      where ft t1 (a, t2) = (a, mappend t1 t2)
            fc c (a, t) = (a', mappend (singleton c') t)
               where (a', c') = f a c

   takeWhile pt pc = fst . span pt pc
   dropWhile pt pc = snd . span pt pc
   span pt pc = Factorial.span (\prime-> maybe (pt prime) pc (characterPrefix prime))
   break pt pc = Factorial.break (\prime-> maybe (pt prime) pc (characterPrefix prime))
   spanMaybe s0 ft fc t0 = spanAfter id s0 t0
      where spanAfter g s t = case Factorial.splitPrimePrefix t
                              of Just (prime, rest) | Just s' <- maybe (ft s prime) (fc s) (characterPrefix prime) ->
                                                        spanAfter (g . mappend prime) s' rest
                                                    | otherwise -> (g mempty, t, s)
                                 Nothing -> (t0, t, s)
   spanMaybe' s0 ft fc t0 = spanAfter id s0 t0
      where spanAfter g s t = seq s $
                              case Factorial.splitPrimePrefix t
                              of Just (prime, rest) | Just s' <- maybe (ft s prime) (fc s) (characterPrefix prime) ->
                                                        spanAfter (g . mappend prime) s' rest
                                                    | otherwise -> (g mempty, t, s)
                                 Nothing -> (t0, t, s)
   takeWhile_ = takeWhile . const
   dropWhile_ = dropWhile . const
   break_ = break . const
   span_ = span . const
   spanMaybe_ s = spanMaybe s (const . Just)
   spanMaybe_' s = spanMaybe' s (const . Just)

   split p m = prefix : splitRest
      where (prefix, rest) = break (const False) p m
            splitRest = case splitCharacterPrefix rest
                        of Nothing -> []
                           Just (_, tail) -> split p tail
   find p = foldr (const id) (\c r-> if p c then Just c else r) Nothing
   elem c = any (== c)

   {-# INLINE characterPrefix #-}
   {-# INLINE concatMap #-}
   {-# INLINE dropWhile #-}
   {-# INLINE fromText #-}
   {-# INLINE map #-}
   {-# INLINE mapAccumL #-}
   {-# INLINE mapAccumR #-}
   {-# INLINE scanl #-}
   {-# INLINE scanl1 #-}
   {-# INLINE scanr #-}
   {-# INLINE scanr1 #-}
   {-# INLINE singleton #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE split #-}
   {-# INLINE takeWhile #-}
   {-# INLINE foldl_ #-}
   {-# INLINE foldl_' #-}
   {-# INLINE foldr_ #-}
   {-# INLINE spanMaybe_ #-}
   {-# INLINE spanMaybe_' #-}
   {-# INLINE span_ #-}
   {-# INLINE break_ #-}
   {-# INLINE takeWhile_ #-}
   {-# INLINE dropWhile_ #-}

foldlChars f (t, c1) c2 = (mappend t (singleton c'), c')
   where c' = f c1 c2
foldlOther (t1, c) t2 = (mappend t1 t2, c)
foldrChars f c1 (t, c2) = (mappend (singleton c') t, c')
   where c' = f c1 c2
foldrOther t1 (t2, c) = (mappend t1 t2, c)

instance TextualMonoid String where
   fromText = Text.unpack
   singleton c = [c]
   splitCharacterPrefix (c:rest) = Just (c, rest)
   splitCharacterPrefix [] = Nothing
   characterPrefix (c:_) = Just c
   characterPrefix [] = Nothing
   map = List.map
   concatMap = List.concatMap
   any = List.any
   all = List.all

   foldl   = const List.foldl
   foldl'  = const List.foldl'
   foldr   = const List.foldr

   scanl = List.scanl
   scanl1 = List.scanl1
   scanr = List.scanr
   scanr1 = List.scanr1 
   mapAccumL = List.mapAccumL
   mapAccumR = List.mapAccumR

   takeWhile _ = List.takeWhile
   dropWhile _ = List.dropWhile
   break _ = List.break
   span _ = List.span
   spanMaybe s0 _ft fc l = (prefix' [], suffix' [], s')
      where (prefix', suffix', s', live') = List.foldl' g (id, id, s0, True) l
            g (prefix, suffix, s, live) c | live, Just s' <- fc s c = (prefix . (c:), id, s', True)
                                          | otherwise = (prefix, suffix . (c:), s, False)
   spanMaybe' s0 _ft fc l = (prefix' [], suffix' [], s')
      where (prefix', suffix', s', live') = List.foldl' g (id, id, s0, True) l
            g (prefix, suffix, s, live) c | live, Just s' <- fc s c = seq s' (prefix . (c:), id, s', True)
                                          | otherwise = (prefix, suffix . (c:), s, False)
   find = List.find
   elem = List.elem

   {-# INLINE all #-}
   {-# INLINE any #-}
   {-# INLINE break #-}
   {-# INLINE characterPrefix #-}
   {-# INLINE concatMap #-}
   {-# INLINE dropWhile #-}
   {-# INLINE elem #-}
   {-# INLINE find #-}
   {-# INLINE foldl   #-}
   {-# INLINE foldl'  #-}
   {-# INLINE foldr   #-}
   {-# INLINE fromText #-}
   {-# INLINE map #-}
   {-# INLINE mapAccumL #-}
   {-# INLINE mapAccumR #-}
   {-# INLINE scanl #-}
   {-# INLINE scanl1 #-}
   {-# INLINE scanr #-}
   {-# INLINE scanr1 #-}
   {-# INLINE singleton #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE split #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE takeWhile #-}

instance TextualMonoid Text where
   fromText = id
   singleton = Text.singleton
   splitCharacterPrefix = Text.uncons
   characterPrefix t = if Text.null t then Nothing else Just (Text.head t)
   map = Text.map
   concatMap = Text.concatMap
   any = Text.any
   all = Text.all

   foldl   = const Text.foldl
   foldl'  = const Text.foldl'
   foldr   = const Text.foldr

   scanl = Text.scanl
   scanl1 = Text.scanl1
   scanr = Text.scanr
   scanr1 = Text.scanr1 
   mapAccumL = Text.mapAccumL
   mapAccumR = Text.mapAccumR

   takeWhile _ = Text.takeWhile
   dropWhile _ = Text.dropWhile
   break _ = Text.break
   span _ = Text.span
   spanMaybe s0 _ft fc t = case Text.foldr g id t (0, s0)
                           of (i, s') | (prefix, suffix) <- Text.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- fc s c = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 _ft fc t = case Text.foldr g id t (0, s0)
                            of (i, s') | (prefix, suffix) <- Text.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- fc s c = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   split = Text.split
   find = Text.find

   {-# INLINE all #-}
   {-# INLINE any #-}
   {-# INLINE break #-}
   {-# INLINE characterPrefix #-}
   {-# INLINE concatMap #-}
   {-# INLINE dropWhile #-}
   {-# INLINE find #-}
   {-# INLINE foldl   #-}
   {-# INLINE foldl'  #-}
   {-# INLINE foldr   #-}
   {-# INLINE fromText #-}
   {-# INLINE map #-}
   {-# INLINE mapAccumL #-}
   {-# INLINE mapAccumR #-}
   {-# INLINE scanl #-}
   {-# INLINE scanl1 #-}
   {-# INLINE scanr #-}
   {-# INLINE scanr1 #-}
   {-# INLINE singleton #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE split #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE takeWhile #-}

instance TextualMonoid LazyText.Text where
   fromText = LazyText.fromStrict
   singleton = LazyText.singleton
   splitCharacterPrefix = LazyText.uncons
   characterPrefix t = if LazyText.null t then Nothing else Just (LazyText.head t)
   map = LazyText.map
   concatMap = LazyText.concatMap
   any = LazyText.any
   all = LazyText.all

   foldl   = const LazyText.foldl
   foldl'  = const LazyText.foldl'
   foldr   = const LazyText.foldr

   scanl = LazyText.scanl
   scanl1 = LazyText.scanl1
   scanr = LazyText.scanr
   scanr1 = LazyText.scanr1
   mapAccumL = LazyText.mapAccumL
   mapAccumR = LazyText.mapAccumR

   takeWhile _ = LazyText.takeWhile
   dropWhile _ = LazyText.dropWhile
   break _ = LazyText.break
   span _ = LazyText.span
   spanMaybe s0 _ft fc t = case LazyText.foldr g id t (0, s0)
                           of (i, s') | (prefix, suffix) <- LazyText.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- fc s c = let i' = succ i :: Int64 in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 _ft fc t = case LazyText.foldr g id t (0, s0)
                            of (i, s') | (prefix, suffix) <- LazyText.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- fc s c = let i' = succ i :: Int64 in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   split = LazyText.split
   find = LazyText.find
   {-# INLINE all #-}
   {-# INLINE any #-}
   {-# INLINE break #-}
   {-# INLINE characterPrefix #-}
   {-# INLINE concatMap #-}
   {-# INLINE dropWhile #-}
   {-# INLINE find #-}
   {-# INLINE foldl   #-}
   {-# INLINE foldl'  #-}
   {-# INLINE foldr   #-}
   {-# INLINE fromText #-}
   {-# INLINE map #-}
   {-# INLINE mapAccumL #-}
   {-# INLINE mapAccumR #-}
   {-# INLINE scanl #-}
   {-# INLINE scanl1 #-}
   {-# INLINE scanr #-}
   {-# INLINE scanr1 #-}
   {-# INLINE singleton #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE split #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE takeWhile #-}

instance IsString (Sequence.Seq Char) where
   fromString = Sequence.fromList

instance TextualMonoid (Sequence.Seq Char) where
   singleton = Sequence.singleton
   splitCharacterPrefix s = case Sequence.viewl s
                            of Sequence.EmptyL -> Nothing
                               c Sequence.:< rest -> Just (c, rest)
   characterPrefix s = case Sequence.viewl s
                       of Sequence.EmptyL -> Nothing
                          c Sequence.:< rest -> Just c
   map = Traversable.fmapDefault
   concatMap = Foldable.foldMap
   any = Foldable.any
   all = Foldable.all

   foldl   = const Foldable.foldl
   foldl'  = const Foldable.foldl'
   foldr   = const Foldable.foldr

   scanl = Sequence.scanl
   scanl1 f v | Sequence.null v = Sequence.empty
              | otherwise = Sequence.scanl1 f v
   scanr = Sequence.scanr
   scanr1 f v | Sequence.null v = Sequence.empty
              | otherwise = Sequence.scanr1 f v

   takeWhile _ = Sequence.takeWhileL
   dropWhile _ = Sequence.dropWhileL
   break _ = Sequence.breakl
   span _ = Sequence.spanl
   spanMaybe s0 _ft fc b = case Foldable.foldr g id b (0, s0)
                           of (i, s') | (prefix, suffix) <- Sequence.splitAt i b -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- fc s c = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 _ft fc b = case Foldable.foldr g id b (0, s0)
                            of (i, s') | (prefix, suffix) <- Sequence.splitAt i b -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- fc s c = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   find = Foldable.find
   elem = Foldable.elem

   {-# INLINE all #-}
   {-# INLINE any #-}
   {-# INLINE break #-}
   {-# INLINE characterPrefix #-}
   {-# INLINE concatMap #-}
   {-# INLINE dropWhile #-}
   {-# INLINE elem #-}
   {-# INLINE find #-}
   {-# INLINE foldl   #-}
   {-# INLINE foldl'  #-}
   {-# INLINE foldr   #-}
   {-# INLINE fromText #-}
   {-# INLINE map #-}
   {-# INLINE mapAccumL #-}
   {-# INLINE mapAccumR #-}
   {-# INLINE scanl #-}
   {-# INLINE scanl1 #-}
   {-# INLINE scanr #-}
   {-# INLINE scanr1 #-}
   {-# INLINE singleton #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE split #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE takeWhile #-}

instance IsString (Vector.Vector Char) where
   fromString = Vector.fromList

instance TextualMonoid (Vector.Vector Char) where
   singleton = Vector.singleton
   splitCharacterPrefix t = if Vector.null t then Nothing else Just (Vector.unsafeHead t, Vector.unsafeTail t)
   characterPrefix = (Vector.!? 0)
   map = Vector.map
   concatMap = Vector.concatMap
   any = Vector.any
   all = Vector.all

   foldl   = const Vector.foldl
   foldl'  = const Vector.foldl'
   foldr   = const Vector.foldr

   scanl = Vector.scanl
   scanl1 f v | Vector.null v = Vector.empty
              | otherwise = Vector.scanl1 f v
   scanr = Vector.scanr
   scanr1 f v | Vector.null v = Vector.empty
              | otherwise = Vector.scanr1 f v
   mapAccumL f a0 t = (a, Vector.reverse $ Vector.fromList l)
      where (a, l) = Vector.foldl fc (a0, []) t
            fc (a, l) c = (a', c':l)
               where (a', c') = f a c
   mapAccumR f a0 t = (a, Vector.fromList l)
      where (a, l) = Vector.foldr fc (a0, []) t
            fc c (a, l) = (a',  c':l)
               where (a', c') = f a c

   takeWhile _ = Vector.takeWhile
   dropWhile _ = Vector.dropWhile
   break _ = Vector.break
   span _ = Vector.span
   spanMaybe s0 _ft fc v = case Vector.ifoldr g Left v s0
                           of Left s' -> (v, Vector.empty, s')
                              Right (i, s') | (prefix, suffix) <- Vector.splitAt i v -> (prefix, suffix, s')
      where g i c cont s | Just s' <- fc s c = cont s'
                         | otherwise = Right (i, s)
   spanMaybe' s0 _ft fc v = case Vector.ifoldr' g Left v s0
                            of Left s' -> (v, Vector.empty, s')
                               Right (i, s') | (prefix, suffix) <- Vector.splitAt i v -> (prefix, suffix, s')
      where g i c cont s | Just s' <- fc s c = seq s' (cont s')
                         | otherwise = Right (i, s)
   find = Vector.find
   elem = Vector.elem

   {-# INLINE all #-}
   {-# INLINE any #-}
   {-# INLINE break #-}
   {-# INLINE characterPrefix #-}
   {-# INLINE concatMap #-}
   {-# INLINE dropWhile #-}
   {-# INLINE elem #-}
   {-# INLINE find #-}
   {-# INLINE foldl   #-}
   {-# INLINE foldl'  #-}
   {-# INLINE foldr   #-}
   {-# INLINE fromText #-}
   {-# INLINE map #-}
   {-# INLINE mapAccumL #-}
   {-# INLINE mapAccumR #-}
   {-# INLINE scanl #-}
   {-# INLINE scanl1 #-}
   {-# INLINE scanr #-}
   {-# INLINE scanr1 #-}
   {-# INLINE singleton #-}
   {-# INLINE span #-}
   {-# INLINE spanMaybe #-}
   {-# INLINE spanMaybe' #-}
   {-# INLINE split #-}
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE takeWhile #-}
