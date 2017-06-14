{- 
    Copyright 2017 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module contains orphan 'IsString' and 'TextualMonoid' instances of @Vector Char@.
-- 

{-# LANGUAGE Haskell2010, FlexibleInstances, Trustworthy #-}

module Data.Monoid.Instances.CharVector where

import Data.String (IsString(fromString))
import qualified Data.Vector as Vector

import Data.Monoid.Textual (TextualMonoid(..))

instance IsString (Vector.Vector Char) where
   fromString = Vector.fromList

instance TextualMonoid (Vector.Vector Char) where
   singleton = Vector.singleton
   splitCharacterPrefix t = if Vector.null t then Nothing else Just (Vector.unsafeHead t, Vector.unsafeTail t)
   characterPrefix = (Vector.!? 0)
   map = Vector.map
   concatMap = Vector.concatMap
   toString = const Vector.toList
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
   mapAccumL f a0 t = (a', Vector.reverse $ Vector.fromList l')
      where (a', l') = Vector.foldl fc (a0, []) t
            fc (a, l) c = (:l) <$> f a c
   mapAccumR f a0 t = (a', Vector.fromList l')
      where (a', l') = Vector.foldr fc (a0, []) t
            fc c (a, l) = (:l) <$> f a c

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
   {-# INLINE splitCharacterPrefix #-}
   {-# INLINE takeWhile #-}
