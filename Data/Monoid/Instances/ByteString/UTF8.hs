{- 
    Copyright 2013-2015 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'ByteStringUTF8' newtype wrapper around 'ByteString', together with its 'TextualMonoid'
-- instance. The 'FactorialMonoid' instance of a wrapped 'ByteStringUTF8' value differs from the original 'ByteString':
-- the prime 'factors' of the original value are its bytes, and for the wrapped value the prime 'factors' are its valid
-- UTF8 byte sequences. The following example session demonstrates the relationship:
-- 
-- >> let utf8@(ByteStringUTF8 bs) = fromString "E=mc\xb2"
-- >> bs
-- >"E=mc\194\178"
-- >> factors bs
-- >["E","=","m","c","\194","\178"]
-- >> utf8
-- >"E=mc²"
-- >> factors utf8
-- >["E","=","m","c","²"]
--
-- The 'TextualMonoid' instance follows the same logic, but it also decodes all valid UTF8 sequences into
-- characters. Any invalid UTF8 byte sequence from the original 'ByteString' is preserved as a single prime factor:
--
-- >> let utf8'@(ByteStringUTF8 bs') = ByteStringUTF8 (Data.ByteString.map pred bs)
-- >> bs'
-- >"D<lb\193\177"
-- >> factors bs'
-- >["D","<","l","b","\193","\177"]
-- >> utf8'
-- >"D<lb\[193,177]"
-- >> factors utf8'
-- >["D","<","l","b","\[193,177]"]

{-# LANGUAGE Haskell2010 #-}

module Data.Monoid.Instances.ByteString.UTF8 (
   ByteStringUTF8(..), decode
   )
where

import Prelude hiding (any, drop, dropWhile, foldl, foldl1, foldMap, foldr, foldr1, scanl, scanr, scanl1, scanr1,
                       map, concatMap, break, span)

import Control.Exception (assert)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (chr, ord, isDigit, isPrint)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String (IsString(fromString))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Internal (w2c)
import Data.ByteString.Unsafe (unsafeDrop, unsafeHead, unsafeTail, unsafeTake, unsafeIndex)

import Data.Monoid (Monoid(mempty, mappend))
import Data.Monoid.Cancellative (LeftReductiveMonoid(..), LeftCancellativeMonoid, LeftGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(..), PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial (FactorialMonoid(..))
import qualified Data.Monoid.Textual as Textual (TextualMonoid(..))

newtype ByteStringUTF8 = ByteStringUTF8 ByteString deriving (Eq, Ord)

-- | Takes a raw 'ByteString' chunk and returns a pair of 'ByteStringUTF8' decoding the prefix of the chunk and the
-- remaining suffix that is either null or contains the incomplete last character of the chunk.
decode :: ByteString -> (ByteStringUTF8, ByteString)
decode bs
   | ByteString.null bs || l < 0x80 = (ByteStringUTF8 bs, mempty)
   | l >= 0xC0 = (ByteStringUTF8 (ByteString.init bs), ByteString.singleton l)
   | ByteString.null prefix = (mempty, bs)
   | otherwise =
      case toChar (ByteString.last prefix) suffix
      of Nothing -> (ByteStringUTF8 (ByteString.init prefix), drop (ByteString.length prefix - 1) bs)
         Just{} -> (ByteStringUTF8 bs, mempty)
   where (prefix, suffix) = ByteString.breakEnd byteStartsCharacter bs
         l = ByteString.last bs

instance Monoid ByteStringUTF8 where
   mempty = ByteStringUTF8 ByteString.empty
   {-# INLINE mempty #-}
   ByteStringUTF8 a `mappend` ByteStringUTF8 b = ByteStringUTF8 (a `mappend` b)
   {-# INLINE mappend #-}

instance MonoidNull ByteStringUTF8 where
   null (ByteStringUTF8 b) = ByteString.null b
   {-# INLINE null #-}

instance LeftReductiveMonoid ByteStringUTF8 where
   stripPrefix (ByteStringUTF8 a) (ByteStringUTF8 b) = fmap ByteStringUTF8 (stripPrefix a b)
   {-# INLINE stripPrefix #-}
   ByteStringUTF8 a `isPrefixOf` ByteStringUTF8 b = a `isPrefixOf` b
   {-# INLINE isPrefixOf #-}

instance LeftCancellativeMonoid ByteStringUTF8

instance LeftGCDMonoid ByteStringUTF8 where
   commonPrefix (ByteStringUTF8 a) (ByteStringUTF8 b) = ByteStringUTF8 (commonPrefix a b)
   {-# INLINE commonPrefix #-}
   stripCommonPrefix (ByteStringUTF8 a) (ByteStringUTF8 b) = wrapTriple (stripCommonPrefix a b)
   {-# INLINE stripCommonPrefix #-}

instance Show ByteStringUTF8 where
   showsPrec _ bs s0 = '"' : Textual.foldr showsBytes showsChar ('"' : s0) bs
      where showsBytes (ByteStringUTF8 b) s = '\\' : shows (ByteString.unpack b) s
            showsChar c s
              | isPrint c = c : s
              | h:_ <- s, isDigit h = "\\" ++ show (ord c) ++ "\\&" ++ s
              | otherwise = "\\" ++ show (ord c) ++ s

instance IsString ByteStringUTF8 where
   fromString = ByteStringUTF8 . Foldable.foldMap fromChar
   {-# INLINE fromString #-}

instance PositiveMonoid ByteStringUTF8

instance FactorialMonoid ByteStringUTF8 where
   splitPrimePrefix utf8@(ByteStringUTF8 bs)
      | ByteString.null bs = Nothing
      | unsafeHead bs < 0x80 = Just (wrapPair $ ByteString.splitAt 1 bs)
      | otherwise = case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
                    of Just i -> Just (wrapPair $ ByteString.splitAt (succ i) bs)
                       Nothing -> Just (utf8, ByteStringUTF8 $ ByteString.empty)
   {-# INLINABLE splitPrimePrefix #-}
   splitPrimeSuffix (ByteStringUTF8 bs)
      | ByteString.null bs = Nothing
      | ByteString.null prefix = Just (wrapPair splitBS)
      | not (ByteString.null suffix) && ByteString.last prefix < 0x80 = Just (wrapPair splitBS)
      | otherwise = Just (wrapPair $ ByteString.splitAt (pred $ ByteString.length prefix) bs)
      where splitBS@(prefix, suffix) = ByteString.breakEnd byteStartsCharacter bs
   {-# INLINABLE splitPrimeSuffix #-}
   primePrefix utf8@(ByteStringUTF8 bs)
      | ByteString.null bs = utf8
      | unsafeHead bs < 0x80 = ByteStringUTF8 (ByteString.take 1 bs)
      | otherwise = case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
                    of Just i -> ByteStringUTF8 (ByteString.take (succ i) bs)
                       Nothing -> utf8
   {-# INLINABLE primePrefix #-}
   factors (ByteStringUTF8 bs) = List.map ByteStringUTF8 $ ByteString.groupBy continued bs
      where continued a b = a >= 0x80 && b >= 0x80 && b < 0xC0
   {-# INLINABLE factors #-}
   length (ByteStringUTF8 bs) = fst (ByteString.foldl' count (0, False) bs)
      where count (n, high) byte | byte < 0x80 = (succ n, False)
                                 | byte < 0xC0 = (if high then n else succ n, True)
                                 | otherwise = (succ n, True)
   {-# INLINABLE length #-}
   foldl f a0 (ByteStringUTF8 bs) = List.foldl f' a0 (groupASCII bs)
      where f' a b | unsafeHead b < 0x80 = ByteString.foldl f'' a b
                   | otherwise = f a (ByteStringUTF8 b)
            f'' a w = f a (ByteStringUTF8 $ ByteString.singleton w)
   {-# INLINABLE foldl #-}
   foldl' f a0 (ByteStringUTF8 bs) = List.foldl' f' a0 (groupASCII bs)
      where f' a b | unsafeHead b < 0x80 = ByteString.foldl' f'' a b
                   | otherwise = f a (ByteStringUTF8 b)
            f'' a w = f a (ByteStringUTF8 $ ByteString.singleton w)
   {-# INLINABLE foldl' #-}
   foldr f a0 (ByteStringUTF8 bs) = List.foldr f' a0 (groupASCII bs)
      where f' b a | unsafeHead b < 0x80 = ByteString.foldr f'' a b
                   | otherwise = f (ByteStringUTF8 b) a
            f'' w a = f (ByteStringUTF8 $ ByteString.singleton w) a
   {-# INLINABLE foldr #-}
   splitAt n (ByteStringUTF8 bs) = wrapPair (ByteString.splitAt (charStartIndex n bs) bs)
   {-# INLINE splitAt #-}
   take n (ByteStringUTF8 bs) = ByteStringUTF8 (ByteString.take (charStartIndex n bs) bs)
   {-# INLINE take #-}
   drop n (ByteStringUTF8 bs) = ByteStringUTF8 (ByteString.drop (charStartIndex n bs) bs)
   {-# INLINE drop #-}
   dropWhile p (ByteStringUTF8 bs0) = dropASCII bs0
      where dropASCII bs =
               let suffix = ByteString.dropWhile (\w-> w < 0x80 && p (ByteStringUTF8 $ ByteString.singleton w)) bs
               in if ByteString.null suffix || unsafeHead suffix < 0x80
                  then ByteStringUTF8 suffix
                  else dropMultiByte suffix
            dropMultiByte bs =
               let utf8 = ByteStringUTF8 bs
               in case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
                  of Nothing -> if p utf8 then ByteStringUTF8 ByteString.empty else utf8
                     Just i -> let (hd, tl) = ByteString.splitAt (succ i) bs
                               in if p (ByteStringUTF8 hd)
                                  then dropASCII tl
                                  else utf8
   {-# INLINE dropWhile #-}
   takeWhile p utf8@(ByteStringUTF8 bs) =
      ByteStringUTF8 $ ByteString.take (ByteString.length bs - ByteString.length s) bs
      where (ByteStringUTF8 s) = Factorial.dropWhile p utf8
   {-# INLINE takeWhile #-}
   span p utf8@(ByteStringUTF8 bs) =
      (ByteStringUTF8 $ ByteString.take (ByteString.length bs - ByteString.length s) bs, suffix)
      where suffix@(ByteStringUTF8 s) = Factorial.dropWhile p utf8
   {-# INLINE span #-}
   break p = Factorial.span (not . p)
   {-# INLINE break #-}
   spanMaybe s0 f (ByteStringUTF8 bs0) = (ByteStringUTF8 $ ByteString.take (ByteString.length bs0 - ByteString.length dropped) bs0,
                                          ByteStringUTF8 dropped,
                                          s')
      where (dropped, s') = dropASCII s0 bs0
            dropASCII s bs =
               let suffix = ByteString.drop index bs
                   (index, s1) = ByteString.foldr f8 id bs (0, s)
                   f8 w cont (i, s2)
                     | w < 0x80, Just s3 <- f s2 (ByteStringUTF8 $ ByteString.singleton w) =
                         let i' = succ i :: Int in seq i' $ cont (i', s3)
                     | otherwise = (i, s2)
               in if ByteString.null suffix || unsafeHead suffix < 0x80
                  then (suffix, s1)
                  else dropMultiByte s1 suffix
            dropMultiByte s bs =
               case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
               of Nothing -> case f s (ByteStringUTF8 bs)
                             of Just s1 -> (ByteString.empty, s1)
                                Nothing -> (bs, s)
                  Just i -> let (hd, tl) = ByteString.splitAt (succ i) bs
                            in case f s (ByteStringUTF8 hd)
                               of Just s1 -> dropASCII s1 tl
                                  Nothing -> (bs, s)
   {-# INLINE spanMaybe #-}
   spanMaybe' s0 f (ByteStringUTF8 bs0) = (ByteStringUTF8 $ ByteString.take (ByteString.length bs0 - ByteString.length dropped) bs0,
                                           ByteStringUTF8 dropped,
                                           s')
      where (dropped, s') = dropASCII s0 bs0
            dropASCII s bs =
               let suffix = ByteString.drop index bs
                   (index, s1) = ByteString.foldr f8 id bs (0, s)
                   f8 w cont (i, s2)
                     | w < 0x80, Just s3 <- f s2 (ByteStringUTF8 $ ByteString.singleton w) =
                         let i' = succ i :: Int in seq i' $ seq s3 $ cont (i', s3)
                     | otherwise = (i, s)
               in if ByteString.null suffix || unsafeHead suffix < 0x80
                  then (suffix, s1)
                  else dropMultiByte s1 suffix
            dropMultiByte s bs =
               case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
               of Nothing -> case f s (ByteStringUTF8 bs)
                             of Just s1 -> seq s1 (ByteString.empty, s1)
                                Nothing -> (bs, s)
                  Just i -> let (hd, tl) = ByteString.splitAt (succ i) bs
                            in case f s (ByteStringUTF8 hd)
                               of Just s1 -> seq s1 (dropASCII s1 tl)
                                  Nothing -> (bs, s)
   {-# INLINE spanMaybe' #-}
   reverse (ByteStringUTF8 bs) =
      ByteStringUTF8 (ByteString.concat $ List.reverse $ List.map reverseASCII $ groupASCII bs)
      where reverseASCII b | unsafeHead b < 0x80 = ByteString.reverse b
                           | otherwise = b
   {-# INLINABLE reverse #-}

instance TextualMonoid ByteStringUTF8 where
   singleton = ByteStringUTF8 . fromChar
   {-# INLINE singleton #-}
   splitCharacterPrefix (ByteStringUTF8 bs) = ByteString.uncons bs >>= uncurry toChar
   {-# INLINE splitCharacterPrefix #-}
   foldl ft fc a0 (ByteStringUTF8 bs) = case ByteString.Char8.foldl f (a0, []) bs
                                        of (a, []) -> a
                                           (a, acc) -> multiByte a acc
      where f (a, []) c | c < '\x80' = (fc a c, [])
                        | otherwise = (a, [fromIntegral $ ord c])
            f (a, acc) c | c < '\x80' = (fc (multiByte a acc) c, [])
                         | c < '\xC0' = (a, fromIntegral (ord c) : acc)
                         | otherwise = (multiByte a acc, [fromIntegral $ ord c])
            multiByte a acc = reverseBytesToChar (ft a . ByteStringUTF8) (fc a) acc
   {-# INLINE foldl #-}
   foldl' ft fc a0 (ByteStringUTF8 bs) = case ByteString.Char8.foldl' f (a0, []) bs
                                         of (a, []) -> a
                                            (a, acc) -> multiByte a acc
      where f (a, []) c | c < '\x80' = (fc a c, [])
                        | otherwise = seq a (a, [fromIntegral $ ord c])
            f (a, acc) c | seq a c < '\x80' = let a' = multiByte a acc in seq a' (fc a' c, [])
                         | c < '\xC0' = (a, fromIntegral (ord c) : acc)
                         | otherwise = let a' = multiByte a acc in seq a' (a', [fromIntegral $ ord c])
            multiByte a acc = reverseBytesToChar (ft a . ByteStringUTF8) (fc a) acc
   {-# INLINE foldl' #-}
   foldr ft fc a0 (ByteStringUTF8 bs) = case ByteString.Char8.foldr f (a0, []) bs
                                        of (a, []) -> a
                                           (a, acc) -> multiByte a acc
      where f c (a, []) | c < '\x80' = (fc c a, [])
                        | c < '\xC0' = (a, [fromIntegral $ ord c])
                        | otherwise = (ft (ByteStringUTF8 $ ByteString.Char8.singleton c) a, [])
            f c (a, acc) | c < '\x80' = (fc c (ft (ByteStringUTF8 $ ByteString.pack acc) a), [])
                         | c < '\xC0' = (a, fromIntegral (ord c) : acc)
                         | otherwise = (multiByte a (fromIntegral (ord c) : acc), [])
            multiByte a acc = bytesToChar ((`ft` a) . ByteStringUTF8) (`fc` a) acc
   {-# INLINE foldr #-}
   dropWhile pb pc (ByteStringUTF8 bs) = ByteStringUTF8 $ dropASCII bs
      where dropASCII rest = case ByteString.Char8.findIndex (\c-> c > '\x7f' || not (pc c)) rest
                             of Nothing -> ByteString.empty
                                Just j -> let rest' = unsafeDrop j rest
                                          in if unsafeHead rest' > 0x7f
                                             then dropMultiByte rest'
                                             else rest'
            dropMultiByte rest = case splitCharacterPrefix (ByteStringUTF8 rest)
                                 of Just (c, ByteStringUTF8 rest') | pc c -> dropASCII rest'
                                    Nothing -> let j = succ (headIndex $ drop 1 rest)
                                               in if pb (ByteStringUTF8 $ ByteString.take j rest)
                                                  then dropASCII (unsafeDrop j rest)
                                                  else rest
                                    _ -> rest
   {-# INLINE dropWhile #-}
   takeWhile pb pc utf8@(ByteStringUTF8 bs) =
      ByteStringUTF8 $ unsafeTake (ByteString.length bs - ByteString.length suffix) bs
      where ByteStringUTF8 suffix = Textual.dropWhile pb pc utf8
   {-# INLINE takeWhile #-}
   span pb pc utf8@(ByteStringUTF8 bs) = (ByteStringUTF8 $ unsafeTake (ByteString.length bs - ByteString.length suffix') bs, suffix)
      where suffix@(ByteStringUTF8 suffix') = Textual.dropWhile pb pc utf8
   {-# INLINE span #-}
   break pb pc = Textual.span (not . pb) (not . pc)
   {-# INLINE break #-}
   spanMaybe s0 ft fc (ByteStringUTF8 bs)  =
      let inner i s
            | i < len =
                let w = unsafeIndex bs i
                in if w < 0x80
                   then case fc s (w2c w)
                        of Just s' -> inner (i + 1) s'
                           Nothing -> done i s
                   else case splitCharacterPrefix (ByteStringUTF8 $ unsafeDrop i bs)
                        of Just (c, ByteStringUTF8 rest) | Just s' <- fc s c -> inner (len - ByteString.length rest) s'
                           Nothing -> let j = succ (headIndex $ drop (i + 1) bs)
                                      in case ft s (ByteStringUTF8 $ ByteString.take j $ unsafeDrop i bs)
                                         of Just s' -> inner (i + j) s'
                                            Nothing -> done i s
                           _ -> done i s
            | otherwise = done i s
          done i s = i `seq` s `seq` (ByteStringUTF8 $ unsafeTake i bs, ByteStringUTF8 $ unsafeDrop i bs, s)
          len = ByteString.length bs
      in inner 0 s0
   {-# INLINE spanMaybe #-}
   spanMaybe' s0 ft fc (ByteStringUTF8 bs)  =
      let inner i s
            | i < len =
                s `seq`
                let w = unsafeIndex bs i
                in if w < 0x80
                   then case fc s (w2c w)
                        of Just s' -> inner (i + 1) s'
                           Nothing -> done i s
                   else case splitCharacterPrefix (ByteStringUTF8 $ unsafeDrop i bs)
                        of Just (c, ByteStringUTF8 rest) | Just s' <- fc s c -> inner (len - ByteString.length rest) s'
                           Nothing -> let j = succ (headIndex $ drop (i + 1) bs)
                                      in case ft s (ByteStringUTF8 $ ByteString.take j $ unsafeDrop i bs)
                                         of Just s' -> inner (i + j) s'
                                            Nothing -> done i s
                           _ -> done i s
            | otherwise = done i s
          done i s = i `seq` s `seq` (ByteStringUTF8 $ unsafeTake i bs, ByteStringUTF8 $ unsafeDrop i bs, s)
          len = ByteString.length bs
      in inner 0 s0
   {-# INLINE spanMaybe' #-}
   find p (ByteStringUTF8 bs0) = loop bs0
      where loop bs = case ByteString.Char8.findIndex (\c-> c >= '\x80' || p c) bs
                      of Nothing -> Nothing
                         Just i -> let x = unsafeIndex bs i
                                       bs' = unsafeDrop (i + 1) bs
                                   in if x < 0x80
                                      then Just (w2c x)
                                      else case toChar x bs'
                                           of Just (c, ByteStringUTF8 rest) | p c -> Just c
                                                                            | otherwise -> loop rest
                                              Nothing -> loop (ByteString.dropWhile (not . byteStartsCharacter) bs')
   {-# INLINE find #-}
   any p utf8 = isJust (find p utf8)
   {-# INLINE any #-}
   all p utf8 = isNothing (find (not . p) utf8)
   {-# INLINE all #-}
   elem c utf8@(ByteStringUTF8 bs)
     | c < '\x80' = ByteString.Char8.elem c bs
     | otherwise = any (== c) utf8
   {-# INLINE elem #-}

reverseBytesToChar :: (ByteString -> a) -> (Char -> a) -> [Word8] -> a
reverseBytesToChar ft fc [w] = if w < 0x80 then fc (w2c w) else ft (ByteString.singleton w)
reverseBytesToChar ft fc [b0, b1] =
  assert (0x80 <= b0 && b0 < 0xC0 && 0xC0 <= b1) $
  if 0xC2 <= b1 && b1 < 0xE0
  then fc (chr (shiftL (fromIntegral b1 .&. 0x1F) 6 .|. fromIntegral b0 .&. 0x3F))
  else ft (ByteString.pack [b1, b0])
reverseBytesToChar ft fc [b0, b1, b2] =
  assert (0x80 <= b0 && b0 < 0xC0 && 0x80 <= b1 && b1 < 0xC0 && 0xC0 <= b2) $
  if (0xE0 < b2 || 0xE0 == b2 && 0xA0 <= b1) && b2 < 0xF0
  then fc (chr (shiftL (fromIntegral b2 .&. 0xF) 12
                .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                .|. fromIntegral b0 .&. 0x3F))
  else ft (ByteString.pack [b2, b1, b0])
reverseBytesToChar ft fc [b0, b1, b2, b3] =
  assert (0x80 <= b0 && b0 < 0xC0 && 0x80 <= b1 && b1 < 0xC0 && 0x80 <= b2 && b2 < 0xC0 && 0xC0 <= b3) $
  if (0xF0 < b3 || 0xF0 == b3 && 0x90 <= b2) && b3 < 0xF4
  then fc (chr (shiftL (fromIntegral b3 .&. 0x7) 18
                .|. shiftL (fromIntegral b2 .&. 0x3F) 12
                .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                .|. fromIntegral b0 .&. 0x3F))
  else ft (ByteString.pack [b3, b2, b1, b0])
reverseBytesToChar ft _fc bytes = ft (ByteString.reverse $ ByteString.pack bytes)

bytesToChar :: (ByteString -> a) -> (Char -> a) -> [Word8] -> a
bytesToChar ft fc [w] = if w < 0x80 then fc (w2c w) else ft (ByteString.singleton w)
bytesToChar ft fc bytes@[b1, b0] =
  assert (0x80 <= b0 && b0 < 0xC0) $
  if 0xC2 <= b1 && b1 < 0xE0
  then fc (chr (shiftL (fromIntegral b1 .&. 0x1F) 6 .|. fromIntegral b0 .&. 0x3F))
  else ft (ByteString.pack bytes)
bytesToChar ft fc bytes@[b2, b1, b0] =
  assert (0x80 <= b0 && b0 < 0xC0 && 0x80 <= b1 && b1 < 0xC0) $
  if (0xE0 < b2 || 0xE0 == b2 && 0xA0 <= b1) && b2 < 0xF0
  then fc (chr (shiftL (fromIntegral b2 .&. 0xF) 12
                .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                .|. fromIntegral b0 .&. 0x3F))
  else ft (ByteString.pack bytes)
bytesToChar ft fc bytes@[b3, b2, b1, b0] =
  assert (0x80 <= b0 && b0 < 0xC0 && 0x80 <= b1 && b1 < 0xC0 && 0x80 <= b2 && b2 < 0xC0) $
  if (0xF0 < b3 || 0xF0 == b3 && 0x90 <= b2) && b3 < 0xF4
  then fc (chr (shiftL (fromIntegral b3 .&. 0x7) 18
                .|. shiftL (fromIntegral b2 .&. 0x3F) 12
                .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                .|. fromIntegral b0 .&. 0x3F))
  else ft (ByteString.pack bytes)
bytesToChar ft _fc bytes = ft (ByteString.pack bytes)

wrapPair :: (ByteString, ByteString) -> (ByteStringUTF8, ByteStringUTF8)
wrapPair (bs1, bs2) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2)
{-# INLINE wrapPair #-}

wrapTriple :: (ByteString, ByteString, ByteString) -> (ByteStringUTF8, ByteStringUTF8, ByteStringUTF8)
wrapTriple (bs1, bs2, bs3) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2, ByteStringUTF8 bs3)
{-# INLINE wrapTriple #-}

fromChar :: Char -> ByteString
fromChar c | c < '\x80'    = ByteString.Char8.singleton c
           | c < '\x800'   = ByteString.pack [0xC0 + fromIntegral (shiftR n 6),
                                              0x80 + fromIntegral (n .&. 0x3F)]
           | c < '\x10000' = ByteString.pack [0xE0 + fromIntegral (shiftR n 12),
                                              0x80 + fromIntegral (shiftR n 6 .&. 0x3F),
                                              0x80 + fromIntegral (n .&. 0x3F)]
           | n < 0x200000  = ByteString.pack [0xF0 + fromIntegral (shiftR n 18),
                                              0x80 + fromIntegral (shiftR n 12 .&. 0x3F),
                                              0x80 + fromIntegral (shiftR n 6 .&. 0x3F),
                                              0x80 + fromIntegral (n .&. 0x3F)]
           | otherwise  = error ("Data.Char.ord '" ++ (c : "' >=0x200000"))
   where n = ord c

toChar :: Word8 -> ByteString -> Maybe (Char, ByteStringUTF8)
toChar hd tl | hd < 0x80 = Just (w2c hd, ByteStringUTF8 tl)
             | hd < 0xC2 = Nothing
             | hd < 0xE0 = do (b0, t0) <- ByteString.uncons tl
                              if headIndex tl == 1
                                 then return (chr (shiftL (fromIntegral hd .&. 0x1F) 6
                                                   .|. fromIntegral b0 .&. 0x3F),
                                              ByteStringUTF8 t0)
                                 else Nothing
             | hd < 0xF0 = do (b1, t1) <- ByteString.uncons tl
                              (b0, t0) <- ByteString.uncons t1
                              if (hd > 0xE0 || b1 >= 0xA0) && headIndex tl == 2
                                 then return (chr (shiftL (fromIntegral hd .&. 0xF) 12
                                                   .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                                                   .|. fromIntegral b0 .&. 0x3F),
                                              ByteStringUTF8 t0)
                                 else Nothing
             | hd < 0xF4 = do (b2, t2) <- ByteString.uncons tl
                              (b1, t1) <- ByteString.uncons t2
                              (b0, t0) <- ByteString.uncons t1
                              if (hd > 0xF0 || b2 >= 0x90) && headIndex tl == 3
                                 then return (chr (shiftL (fromIntegral hd .&. 0x7) 18
                                                   .|. shiftL (fromIntegral b2 .&. 0x3F) 12
                                                   .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                                                   .|. fromIntegral b0 .&. 0x3F),
                                              ByteStringUTF8 t0)
                                 else Nothing
             | otherwise = Nothing

groupASCII :: ByteString -> [ByteString]
groupASCII = ByteString.groupBy continued
   where continued a b = (a < 0x80) == (b < 0x80) && b < 0xC0
{-# INLINE groupASCII #-}

headIndex :: ByteString -> Int
headIndex bs = fromMaybe (ByteString.length bs) $ ByteString.findIndex byteStartsCharacter bs
{-# INLINE headIndex #-}

byteStartsCharacter :: Word8 -> Bool
byteStartsCharacter b = b < 0x80 || b >= 0xC0
{-# INLINE byteStartsCharacter #-}

charStartIndex :: Int -> ByteString -> Int
charStartIndex n _ | n <= 0 = 0
charStartIndex n bs =
   case List.drop (pred n) (ByteString.findIndices byteStartsCharacter $ ByteString.drop 1 bs)
   of [] -> ByteString.length bs
      k:_ -> succ k
{-# INLINE charStartIndex #-}
