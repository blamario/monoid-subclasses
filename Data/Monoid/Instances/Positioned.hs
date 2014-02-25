{-
    Copyright 2014 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines two monoid transformer data types, 'OffsetPositioned' and 'LinePositioned'. Both data types add
-- a notion of the current position to their base monoid. In case of 'OffsetPositioned', the current position is a
-- simple integer offset from the beginning of the monoid, and it can be applied to any 'StableFactorialMonoid'. The
-- base monoid of 'LinePositioned' must be a 'TextualMonoid', but for the price it will keep track of the current line
-- and column numbers as well.
--

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.Positioned (
   OffsetPositioned, LinePositioned, extract, position, line, column, findIndex, findPosition
   )
where

import Prelude hiding (all, any, break, filter, foldl, foldl1, foldr, foldr1, map, concatMap,
                       length, null, reverse, scanl, scanr, scanl1, scanr1, span, splitAt)
import Control.Applicative (Applicative(..))
import Data.Functor ((<$>))
import qualified Data.List as List
import Data.String (IsString(..))
import Data.Sequence (Seq, filter, (<|), (|>), ViewL((:<)), ViewR((:>)))
import qualified Data.Sequence as Seq

import Data.Monoid (Monoid(..), (<>), Endo(..), First(..), Sum(..))
import Data.Monoid.Cancellative (LeftReductiveMonoid(..), RightReductiveMonoid(..), ReductiveMonoid(..),
                                 LeftGCDMonoid(..), RightGCDMonoid(..), GCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..), StableFactorialMonoid)
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial
import qualified Data.Monoid.Textual as Textual

class Positioned p where
   extract :: p a -> a
   position :: p a -> Int

data OffsetPositioned m = OffsetPositioned{offset :: !Int, 
                                           -- ^ the current offset
                                           extractOffset :: m}

data LinePositioned m = LinePositioned{fullOffset :: !Int, 
                                       -- | the current line
                                       line :: !Int, 
                                       lineStart :: !Int, 
                                       extractLines :: m}

-- | the current column
column :: LinePositioned m -> Int
column lp = position lp - lineStart lp

instance Functor OffsetPositioned where
   fmap f (OffsetPositioned p c) = OffsetPositioned p (f c)

instance Functor LinePositioned where
   fmap f (LinePositioned p l lp c) = LinePositioned p l lp (f c)

instance Applicative OffsetPositioned where
   pure = OffsetPositioned 0
   OffsetPositioned _ f <*> OffsetPositioned p c = OffsetPositioned p (f c)

instance Applicative LinePositioned where
   pure = LinePositioned 1 1 0
   LinePositioned _ _ _ f <*> LinePositioned p l lp c = LinePositioned p l lp (f c)

instance Positioned OffsetPositioned where
   extract = extractOffset
   position = offset

instance Positioned LinePositioned where
   extract = extractLines
   position = fullOffset

instance Eq m => Eq (OffsetPositioned m) where
   OffsetPositioned{extractOffset= a} == OffsetPositioned{extractOffset= b} = a == b

instance Eq m => Eq (LinePositioned m) where
   LinePositioned{extractLines= a} == LinePositioned{extractLines= b} = a == b

instance Ord m => Ord (OffsetPositioned m) where
   compare OffsetPositioned{extractOffset= a} OffsetPositioned{extractOffset= b} = compare a b

instance Ord m => Ord (LinePositioned m) where
   compare LinePositioned{extractLines= a} LinePositioned{extractLines= b} = compare a b

instance Show m => Show (OffsetPositioned m) where
   showsPrec prec (OffsetPositioned pos c) = shows pos . (": " ++) . showsPrec prec c

instance Show m => Show (LinePositioned m) where
   showsPrec prec (LinePositioned pos l lpos c) = 
      ("Line " ++) . shows l . (", column " ++) . shows (pos - lpos) . (": " ++) . showsPrec prec c

instance StableFactorialMonoid m => Monoid (OffsetPositioned m) where
   mempty = pure mempty
   mappend (OffsetPositioned p1 c1) (OffsetPositioned p2 c2) =
      OffsetPositioned (max p1 (p2 - length c1)) (mappend c1 c2)

instance (StableFactorialMonoid m, TextualMonoid m) => Monoid (LinePositioned m) where
   mempty = pure mempty
   mappend (LinePositioned p1 l1 lp1 c1) (LinePositioned p2 l2 lp2 c2) =
      let p2' = p2 - length c1
          l2' = l2 - lines
          (lines, _) = linesColumns c1
          c = mappend c1 c2
      in if p1 >= p2' || l1 > l2' || lp1 > lp2
         then LinePositioned p1 l1 lp1 c
         else LinePositioned p2' l2' (if lines == 0 then lp2 else lp1) c

instance (StableFactorialMonoid m, MonoidNull m) => MonoidNull (OffsetPositioned m) where
   null = null . extractOffset

instance (StableFactorialMonoid m, TextualMonoid m, MonoidNull m) => MonoidNull (LinePositioned m) where
   null = null . extractLines

instance (StableFactorialMonoid m, PositiveMonoid m) => PositiveMonoid (OffsetPositioned m)

instance (StableFactorialMonoid m, TextualMonoid m, PositiveMonoid m) => PositiveMonoid (LinePositioned m)

instance (StableFactorialMonoid m, LeftReductiveMonoid m) => LeftReductiveMonoid (OffsetPositioned m) where
   isPrefixOf (OffsetPositioned _ c1) (OffsetPositioned _ c2) = isPrefixOf c1 c2
   stripPrefix (OffsetPositioned _ c1) (OffsetPositioned p c2) = fmap (OffsetPositioned (p + length c1)) (stripPrefix c1 c2)

instance (StableFactorialMonoid m, TextualMonoid m, LeftReductiveMonoid m) => 
         LeftReductiveMonoid (LinePositioned m) where
   isPrefixOf a b = isPrefixOf (extractLines a) (extractLines b)
   stripPrefix LinePositioned{extractLines= c1} (LinePositioned p l lpos c2) =
      let (lines, columns) = linesColumns c1
          len = length c1
      in fmap (LinePositioned (p + len) (l + lines) (lpos + len - columns)) (stripPrefix c1 c2)

instance (StableFactorialMonoid m, LeftGCDMonoid m) => LeftGCDMonoid (OffsetPositioned m) where
   commonPrefix (OffsetPositioned p1 c1) (OffsetPositioned p2 c2) = OffsetPositioned (min p1 p2) (commonPrefix c1 c2)
   stripCommonPrefix (OffsetPositioned p1 c1) (OffsetPositioned p2 c2) = 
      (OffsetPositioned (min p1 p2) prefix, OffsetPositioned (p1 + l) c1', OffsetPositioned (p2 + l) c2')
      where (prefix, c1', c2') = stripCommonPrefix c1 c2
            l = length prefix

instance (StableFactorialMonoid m, TextualMonoid m, LeftGCDMonoid m) => LeftGCDMonoid (LinePositioned m) where
   commonPrefix (LinePositioned p1 l1 lp1 c1) (LinePositioned p2 l2 lp2 c2) =
      if p1 <= p2
      then LinePositioned p1 l1 lp1 (commonPrefix c1 c2)
      else LinePositioned p2 l2 lp2 (commonPrefix c1 c2)
   stripCommonPrefix (LinePositioned p1 l1 lp1 c1) (LinePositioned p2 l2 lp2 c2) =
      let (prefix, c1', c2') = stripCommonPrefix c1 c2
          (lines, columns) = linesColumns prefix
          len = length prefix
      in (if p1 <= p2 then LinePositioned p1 l1 lp1 prefix else LinePositioned p2 l2 lp2 prefix, 
          LinePositioned (p1 + len) (l1 + lines) (lp1 + len - columns) c1', 
          LinePositioned (p2 + len) (l2 + lines) (lp2 + len - columns) c2')

instance (StableFactorialMonoid m, RightReductiveMonoid m) => RightReductiveMonoid (OffsetPositioned m) where
   isSuffixOf (OffsetPositioned _ c1) (OffsetPositioned _ c2) = isSuffixOf c1 c2
   stripSuffix (OffsetPositioned _ c1) (OffsetPositioned p c2) = fmap (OffsetPositioned p) (stripSuffix c1 c2)

instance (StableFactorialMonoid m, TextualMonoid m, RightReductiveMonoid m) =>
         RightReductiveMonoid (LinePositioned m) where
   isSuffixOf LinePositioned{extractLines=c1} LinePositioned{extractLines=c2} = isSuffixOf c1 c2
   stripSuffix (LinePositioned p l lp c1) LinePositioned{extractLines=c2} = 
      fmap (LinePositioned p l lp) (stripSuffix c1 c2)

instance (StableFactorialMonoid m, RightGCDMonoid m) => RightGCDMonoid (OffsetPositioned m) where
   commonSuffix (OffsetPositioned p1 c1) (OffsetPositioned p2 c2) = 
      OffsetPositioned (min (p1 + length c1) (p2 + length c2) - length suffix) suffix
      where suffix = commonSuffix c1 c2
   stripCommonSuffix (OffsetPositioned p1 c1) (OffsetPositioned p2 c2) = 
      (OffsetPositioned p1 c1', OffsetPositioned p2 c2', 
       OffsetPositioned (min (p1 + length c1') (p2 + length c2')) suffix)
      where (c1', c2', suffix) = stripCommonSuffix c1 c2

instance (StableFactorialMonoid m, TextualMonoid m, RightGCDMonoid m) => RightGCDMonoid (LinePositioned m) where
   stripCommonSuffix (LinePositioned p1 l1 lp1 c1) (LinePositioned p2 l2 lp2 c2) =
      (LinePositioned p1 l1 lp1 c1', LinePositioned p2 l2 lp2 c2',
       if p1 < p2
       then LinePositioned (p1 + len1) (l1 + lines1) (lp1 + len1 - columns1) suffix
       else LinePositioned (p2 + len2) (l2 + lines2) (lp2 + len2 - columns2) suffix)
      where (c1', c2', suffix) = stripCommonSuffix c1 c2
            len1 = length c1'
            len2 = length c2'
            (lines1, columns1) = linesColumns c1'
            (lines2, columns2) = linesColumns c2'

instance StableFactorialMonoid m => FactorialMonoid (OffsetPositioned m) where
   factors (OffsetPositioned p c) = snd $ List.mapAccumL next p (factors c)
      where next p1 c1 = (succ p1, OffsetPositioned p1 c1)
   primePrefix (OffsetPositioned p c) = OffsetPositioned p (primePrefix c)
   splitPrimePrefix (OffsetPositioned p c) = fmap position (splitPrimePrefix c)
      where position (cp, cs) = (OffsetPositioned p cp, OffsetPositioned (succ p) cs)
   splitPrimeSuffix (OffsetPositioned p c) = fmap position (splitPrimeSuffix c)
      where position (cp, cs) = (OffsetPositioned p cp, OffsetPositioned (p + length cp) cs)
   foldl f a0 (OffsetPositioned p0 c0) = fst $ Factorial.foldl f' (a0, p0) c0
      where f' (a, p) c = (f a (OffsetPositioned p c), succ p)
   foldl' f a0 (OffsetPositioned p0 c0) = fst $ Factorial.foldl' f' (a0, p0) c0
      where f' (a, p) c = let a' = f a (OffsetPositioned p c) in seq a' (a', succ p)
   foldr f a0 (OffsetPositioned p0 c0) = Factorial.foldr f' (const a0) c0 p0
      where f' c cont p = f (OffsetPositioned p c) (cont $! succ p)
   length (OffsetPositioned _ c) = length c
   foldMap f (OffsetPositioned p c) = appEndo (Factorial.foldMap f' c) (const mempty) p
      where -- f' :: m -> Endo (Int -> m)
            f' prime = Endo (\cont pos-> f (OffsetPositioned pos prime) <> cont (succ pos))
   span f m = Factorial.splitAt (findIndex (not . f) m) m
   break f m = Factorial.splitAt (findIndex f m) m
   takeWhile f m = Factorial.take (findIndex (not . f) m) m
   dropWhile f m = Factorial.drop (findIndex (not . f) m) m
   splitAt n m@(OffsetPositioned p c) | n <= 0 = (mempty, m)
                                      | n >= length c = (m, mempty)
                                      | otherwise = (OffsetPositioned p prefix, OffsetPositioned (p + n) suffix)
      where (prefix, suffix) = splitAt n c
   drop n (OffsetPositioned p c) = OffsetPositioned (p + n) (Factorial.drop n c)
   take n (OffsetPositioned p c) = OffsetPositioned p (Factorial.take n c)
   reverse (OffsetPositioned p c) = OffsetPositioned p (Factorial.reverse c)

instance (StableFactorialMonoid m, TextualMonoid m) => FactorialMonoid (LinePositioned m) where
   factors (LinePositioned p0 l0 lp0 c) = snd $ List.mapAccumL next (p0, l0, lp0) (factors c)
      where next (p, l, lp) c1 | characterPrefix c1 == Just '\n' = ((succ p, succ l, p), LinePositioned p l lp c1)
                               | otherwise = ((succ p, l, lp), LinePositioned p l lp c1)
   primePrefix (LinePositioned p l lp c) = LinePositioned p l lp (primePrefix c)
   splitPrimePrefix (LinePositioned p l lp c) = fmap position (splitPrimePrefix c)
      where position (cp, cs) = (LinePositioned p l lp cp, 
                                 if characterPrefix cp == Just '\n'
                                 then LinePositioned (succ p) (succ l) p cs
                                 else LinePositioned (succ p) l lp cs)
   splitPrimeSuffix (LinePositioned p l lp c) = fmap position (splitPrimeSuffix c)
      where position (cp, cs) = (LinePositioned p l lp cp, LinePositioned (p + len) (l + lines) (lp + len - columns) cs)
               where len = length cp
                     (lines, columns) = linesColumns cp
   foldl f a0 (LinePositioned p0 l0 lp0 c0) = fst $ Factorial.foldl f' (a0, p0, l0, lp0) c0
      where f' (a, p, l, lp) c | characterPrefix c == Just '\n' = (f a (LinePositioned p l lp c), succ p, succ l, p)
                               | otherwise = (f a (LinePositioned p l lp c), succ p, l, lp)
            fst (a, _, _, _) = a
   foldl' f a0 (LinePositioned p0 l0 lp0 c0) = fst $ Factorial.foldl' f' (a0, p0, l0, lp0) c0
      where f' (a, p, l, lp) c = let a' = f a (LinePositioned p l lp c) 
                                 in seq a' (if characterPrefix c == Just '\n' 
                                            then (a', succ p, succ l, p)
                                            else (a', succ p, l, lp))
            fst (a, _, _, _) = a
   foldr f a0 (LinePositioned p0 l0 lp0 c0) = Factorial.foldr f' (const3 a0) c0 p0 l0 lp0
      where f' c cont p l lp
               | characterPrefix c == Just '\n' = f (LinePositioned p l lp c) $ ((cont $! succ p) $! succ l) p
               | otherwise = f (LinePositioned p l lp c) $ (cont $! succ p) l lp
   length = length . extractLines
   foldMap f (LinePositioned p l lp c) = appEndo (Factorial.foldMap f' c) (const mempty) p l lp
      where -- f' :: m -> Endo (Int -> Int -> Int -> m)
            f' prime = Endo (\cont p l lp-> f (LinePositioned p l lp prime) 
                                            <> if characterPrefix prime == Just '\n'
                                               then cont (succ p) (succ l) p
                                               else cont (succ p) l lp)
   
   span f m = Factorial.splitAt (findLineIndex (not . f) m) m
   break f m = Factorial.splitAt (findLineIndex f m) m
   takeWhile f m = Factorial.take (findLineIndex (not . f) m) m
   dropWhile f m = Factorial.drop (findLineIndex (not . f) m) m
   splitAt n m@(LinePositioned p l lp c) | n <= 0 = (mempty, m)
                                         | n >= length c = (m, mempty)
                                         | otherwise = (LinePositioned p l lp prefix, 
                                                        LinePositioned (p + n) (l + lines) (lp + n - columns) suffix)
      where (prefix, suffix) = splitAt n c
            (lines, columns) = linesColumns prefix
   take n (LinePositioned p l lp c) = LinePositioned p l lp (Factorial.take n c)
   reverse (LinePositioned p l lp c) = LinePositioned p l lp (Factorial.reverse c)

instance StableFactorialMonoid m => StableFactorialMonoid (OffsetPositioned m)

instance (StableFactorialMonoid m, TextualMonoid m) => StableFactorialMonoid (LinePositioned m)

instance IsString m => IsString (OffsetPositioned m) where
   fromString = pure . fromString

instance IsString m => IsString (LinePositioned m) where
   fromString = pure . fromString

instance (StableFactorialMonoid m, TextualMonoid m) => TextualMonoid (OffsetPositioned m) where
   splitCharacterPrefix (OffsetPositioned p c) = fmap (fmap $ OffsetPositioned $ succ p) (splitCharacterPrefix c)

   fromText = pure . fromText
   singleton = pure . singleton

   characterPrefix = characterPrefix . extractOffset

   map f (OffsetPositioned p c) = OffsetPositioned p (map f c)
   concatMap f (OffsetPositioned p c) = OffsetPositioned p (concatMap (extractOffset . f) c)
   all p = all p . extractOffset
   any p = any p . extractOffset

   foldl ft fc a0 (OffsetPositioned p0 c0) = fst $ Textual.foldl ft' fc' (a0, p0) c0
      where ft' (a, p) c = (ft a (OffsetPositioned p c), succ p)
            fc' (a, p) c = (fc a c, succ p)
   foldl' ft fc a0 (OffsetPositioned p0 c0) = fst $ Textual.foldl' ft' fc' (a0, p0) c0
      where ft' (a, p) c = let a' = ft a (OffsetPositioned p c) in seq a' (a', succ p)
            fc' (a, p) c = let a' = fc a c in seq a' (a', succ p)
   foldr ft fc a0 (OffsetPositioned p0 c0) = snd $ Textual.foldr ft' fc' (p0, a0) c0
      where ft' c (p, a) = (succ p, ft (OffsetPositioned p c) a)
            fc' c (p, a) = (succ p, fc c a)

   scanl f ch (OffsetPositioned p c) = OffsetPositioned p (Textual.scanl f ch c)
   scanl1 f (OffsetPositioned p c) = OffsetPositioned p (Textual.scanl1 f c)
   scanr f ch (OffsetPositioned p c) = OffsetPositioned p (Textual.scanr f ch c)
   scanr1 f (OffsetPositioned p c) = OffsetPositioned p (Textual.scanr1 f c)
   mapAccumL f a0 (OffsetPositioned p c) = fmap (OffsetPositioned p) (Textual.mapAccumL f a0 c)
   mapAccumR f a0 (OffsetPositioned p c) = fmap (OffsetPositioned p) (Textual.mapAccumR f a0 c)

   span pt pc (OffsetPositioned p c) = 
      case (splitCharacterPrefix cs, splitPrimePrefix cs)
      of (Nothing, Just (csp, css)) | pt (OffsetPositioned p' csp) ->
            let (OffsetPositioned _ cssp, ms) = Textual.span pt pc (OffsetPositioned (succ p') css)
            in (OffsetPositioned p (cp <> csp <> cssp), ms)
         _ -> (OffsetPositioned p cp, OffsetPositioned p' cs)
      where (cp, cs) = Textual.span (const False) pc c
            p' = p + length cp
   break pt pc (OffsetPositioned p c) =
      case (splitCharacterPrefix cs, splitPrimePrefix cs)
      of (Nothing, Just (csp, css)) | not (pt (OffsetPositioned p' csp)) ->
            let (OffsetPositioned _ cssp, ms) = Textual.break pt pc (OffsetPositioned (succ p') css)
            in (OffsetPositioned p (cp <> csp <> cssp), ms)
         _ -> (OffsetPositioned p cp, OffsetPositioned p' cs)
      where (cp, cs) = Textual.break (const True) pc c
            p' = p + length cp
   split f (OffsetPositioned p0 c0) = rewrap p0 (Textual.split f c0)
      where rewrap p [] = []
            rewrap p (c:rest) = OffsetPositioned p c : rewrap (p + length c) rest
   find p = find p . extractOffset

instance (StableFactorialMonoid m, TextualMonoid m) => TextualMonoid (LinePositioned m) where
   splitCharacterPrefix (LinePositioned p l lp c) = 
      case splitCharacterPrefix c
      of Nothing -> Nothing
         Just ('\n', rest) -> Just ('\n', LinePositioned (succ p) (succ l) p rest)
         Just (ch, rest) -> Just (ch, LinePositioned (succ p) l lp rest)

   fromText = pure . fromText
   singleton = pure . singleton

   characterPrefix = characterPrefix . extractLines

   map f (LinePositioned p l lp c) = LinePositioned p l lp (map f c)
   concatMap f (LinePositioned p l lp c) = LinePositioned p l lp (concatMap (extractLines . f) c)
   all p = all p . extractLines
   any p = any p . extractLines

   foldl ft fc a0 (LinePositioned p0 l0 lp0 c0) = fstOf4 $ Textual.foldl ft' fc' (a0, p0, l0, lp0) c0
      where ft' (a, p, l, lp) c = (ft a (LinePositioned p l lp c), succ p, l, lp)
            fc' (a, p, l, lp) '\n' = (fc a '\n', succ p, succ l, p)
            fc' (a, p, l, lp) c = (fc a c, succ p, l, lp)
            fstOf4 (a, _, _, _) = a
   foldl' ft fc a0 (LinePositioned p0 l0 lp0 c0) = fstOf4 $ Textual.foldl' ft' fc' (a0, p0, l0, lp0) c0
      where ft' (a, p, l, lp) c = let a' = ft a (LinePositioned p l lp c) 
                                      p' = succ p
                                  in a' `seq` p' `seq` (a', p', l, lp)
            fc' (a, p, l, lp) c = let a' = fc a c 
                                      p' = succ p
                                      l' = succ l
                                  in if c == '\n'
                                     then a' `seq` p' `seq` l' `seq` (a', p', l', p)
                                     else a' `seq` p' `seq` (a', p', l, lp)
            fstOf4 (a, _, _, _) = a
   foldr ft fc a0 (LinePositioned p0 l0 lp0 c0) = Textual.foldr ft' fc' (const3 a0) c0 p0 l0 lp0
      where ft' c cont p l lp = ft (LinePositioned p l lp c) $ (cont $! succ p) l lp
            fc' c cont p l lp
               | c == '\n' = fc c $ ((cont $! succ p) $! succ l) p
               | otherwise = fc c $ (cont $! succ p) l lp

   scanl f ch (LinePositioned p l lp c) = LinePositioned p l lp (Textual.scanl f ch c)
   scanl1 f (LinePositioned p l lp c) = LinePositioned p l lp (Textual.scanl1 f c)
   scanr f ch (LinePositioned p l lp c) = LinePositioned p l lp (Textual.scanr f ch c)
   scanr1 f (LinePositioned p l lp c) = LinePositioned p l lp (Textual.scanr1 f c)
   mapAccumL f a0 (LinePositioned p l lp c) = fmap (LinePositioned p l lp) (Textual.mapAccumL f a0 c)
   mapAccumR f a0 (LinePositioned p l lp c) = fmap (LinePositioned p l lp) (Textual.mapAccumR f a0 c)

   span pt pc (LinePositioned p l lp c) = 
      case (splitCharacterPrefix cs, splitPrimePrefix cs)
      of (Nothing, Just (csp, css)) | pt (LinePositioned p' l' lp' csp) ->
            let (LinePositioned{extractLines= cssp}, ms) = Textual.span pt pc (LinePositioned (succ p') l' lp' css)
            in (LinePositioned p l lp (cp <> csp <> cssp), ms)
         _ -> (LinePositioned p l lp cp, LinePositioned p' l' lp' cs)
      where (cp, cs) = Textual.span (const False) pc c
            p' = p + length cp
            l' = l + lines
            lp' = if lines == 0 then lp else p' - columns
            (lines, columns) = linesColumns cp
   break pt pc (LinePositioned p l lp c) =
      case (splitCharacterPrefix cs, splitPrimePrefix cs)
      of (Nothing, Just (csp, css)) | not (pt (LinePositioned p' l' lp' csp)) ->
            let (LinePositioned{extractLines= cssp}, ms) = Textual.break pt pc (LinePositioned (succ p') l' lp' css)
            in (LinePositioned p l lp (cp <> csp <> cssp), ms)
         _ -> (LinePositioned p l lp cp, LinePositioned p' l' lp' cs)
      where (cp, cs) = Textual.break (const True) pc c
            p' = p + length cp
            l' = l + lines
            lp' = if lines == 0 then lp else p' - columns
            (lines, columns) = linesColumns cp
   split f (LinePositioned p0 l0 lp0 c0) = rewrap p0 l0 lp0 (Textual.split f c0)
      where rewrap _ _ _ [] = []
            rewrap p l lp (c:rest) = LinePositioned p l lp c 
                                     : rewrap p' (l + lines) (if lines == 0 then lp else p' - columns) rest
               where p' = p + length c
                     (lines, columns) = linesColumns c
   find p = find p . extractLines

findIndex f m = findPosition f m - position m

findPosition :: FactorialMonoid m => (OffsetPositioned m -> Bool) -> OffsetPositioned m -> Int
findPosition f (OffsetPositioned p c) = appEndo (foldMap f' c) id p
   where -- f' :: m -> Endo ((Int -> Int) -> Int -> Int)
         f' prime = Endo (\cont pos-> if f (OffsetPositioned pos prime) then pos else cont (succ pos))

findLineIndex f m = findLinePosition f m - position m

findLinePosition :: TextualMonoid m => (LinePositioned m -> Bool) -> LinePositioned m -> Int
findLinePosition f (LinePositioned p l lp c) = Factorial.foldr f' const2 c p l lp
   where -- f' :: m -> (Int -> Int -> Int -> Int) -> Int -> Int -> Int -> Int
         f' t cont p l lp | f (LinePositioned p l lp t) = p 
                          | characterPrefix t == Just '\n' = cont (succ p) (succ l) p
                          | otherwise = cont (succ p) l lp
         const2 p _l _lp = p

linesColumns :: TextualMonoid m => m -> (Int, Int)
linesColumns t = Textual.foldl' (const . fmap succ) fc (0, 0) t
   where fc (l, c) '\n' = (succ l, 0)
         fc (l, c) _ = (l, succ c)

const3 a _p _l _lp = a
