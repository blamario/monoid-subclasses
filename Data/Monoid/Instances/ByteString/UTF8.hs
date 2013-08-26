{- 
    Copyright 2013 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'ByteStringUTF8' newtype wrapper around 'ByteString', together with its 'TextualMonoid'
-- instance.
-- 

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Monoid.Instances.ByteString.UTF8 (
   ByteStringUTF8(..)
   )
where

import Prelude hiding (drop, dropWhile, foldl, foldl1, foldr, foldr1, scanl, scanr, scanl1, scanr1, map, concatMap, break, span)

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (chr, ord)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Unsafe (unsafeDrop, unsafeHead, unsafeTail, unsafeIndex)

import Data.Monoid (Monoid)
import Data.Monoid.Cancellative (LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid)
import Data.Monoid.Null (MonoidNull, PositiveMonoid)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial (FactorialMonoid(..))
import qualified Data.Monoid.Textual as Textual (TextualMonoid(..))

newtype ByteStringUTF8 = ByteStringUTF8 ByteString deriving (Eq, Monoid, MonoidNull,
                                                             LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid)

instance Show ByteStringUTF8 where
   show (ByteStringUTF8 bs) = show (ByteString.Char8.unpack bs)

instance IsString ByteStringUTF8 where
   fromString = ByteStringUTF8 . Foldable.foldMap fromChar

instance PositiveMonoid ByteStringUTF8

instance FactorialMonoid ByteStringUTF8 where
   splitPrimePrefix utf8@(ByteStringUTF8 bs)
      | ByteString.null bs = Nothing
      | unsafeHead bs < 0x80 = Just (wrapPair $ ByteString.splitAt 1 bs)
      | otherwise = case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
                    of Just i -> Just (wrapPair $ ByteString.splitAt (succ i) bs)
                       Nothing -> Just (utf8, ByteStringUTF8 $ ByteString.empty)
   factors (ByteStringUTF8 bs) = List.map ByteStringUTF8 $ ByteString.groupBy continued bs
      where continued a b = a >= 0x80 && b >= 0x80 && b < 0xC0
   length (ByteStringUTF8 bs) = fst (ByteString.foldl' count (0, False) bs)
      where count (n, high) byte | byte < 0x80 = (succ n, False)
                                 | byte < 0xC0 = (if high then n else succ n, True)
                                 | otherwise = (succ n, True)
   splitAt n (ByteStringUTF8 bs) = wrapPair (ByteString.splitAt (charStartIndex n bs) bs)
   take n (ByteStringUTF8 bs) = ByteStringUTF8 (ByteString.take (charStartIndex n bs) bs)
   drop n (ByteStringUTF8 bs) = ByteStringUTF8 (ByteString.drop (charStartIndex n bs) bs)
   dropWhile p (ByteStringUTF8 bs) = dropASCII bs
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
   takeWhile p utf8@(ByteStringUTF8 bs) =
      ByteStringUTF8 $ ByteString.take (ByteString.length bs - ByteString.length s) bs
      where suffix@(ByteStringUTF8 s) = Factorial.dropWhile p utf8
   span p utf8@(ByteStringUTF8 bs) =
      (ByteStringUTF8 $ ByteString.take (ByteString.length bs - ByteString.length s) bs, suffix)
      where suffix@(ByteStringUTF8 s) = Factorial.dropWhile p utf8
   break p = Factorial.span (not . p)

instance TextualMonoid ByteStringUTF8 where
   singleton = ByteStringUTF8 . fromChar
   splitCharacterPrefix (ByteStringUTF8 bs) =
      case ByteString.uncons bs
      of Nothing -> Nothing
         Just (hd, tl) | hd < 0x80 -> Just (chr $ fromIntegral hd, ByteStringUTF8 tl)
                       | hd < 0xC0 -> Nothing
                       | hd < 0xE0 ->
                          do (b0, t0) <- ByteString.uncons tl
                             if hd >= 0xC2 && headIndex tl == 1
                                then return (chr (shiftL (fromIntegral hd .&. 0x1F) 6
                                                  .|. fromIntegral b0 .&. 0x3F),
                                             ByteStringUTF8 t0)
                                else Nothing
                       | hd < 0xF0 ->
                          do (b1, t1) <- ByteString.uncons tl
                             (b0, t0) <- ByteString.uncons t1
                             if (hd > 0xE0 || b1 >= 0xA0) && headIndex tl == 2
                                then return (chr (shiftL (fromIntegral hd .&. 0xF) 12
                                                  .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                                                  .|. fromIntegral b0 .&. 0x3F),
                                             ByteStringUTF8 t0)
                                else Nothing
                       | hd < 0xF8 ->
                          do (b2, t2) <- ByteString.uncons tl
                             (b1, t1) <- ByteString.uncons t2
                             (b0, t0) <- ByteString.uncons t1
                             if (hd > 0xF0 || b1 >= 0x90) && hd < 0xF4 && headIndex tl == 3
                                then return (chr (shiftL (fromIntegral hd .&. 0x7) 18
                                                  .|. shiftL (fromIntegral b2 .&. 0x3F) 12
                                                  .|. shiftL (fromIntegral b1 .&. 0x3F) 6
                                                  .|. fromIntegral b0 .&. 0x3F),
                                             ByteStringUTF8 t0)
                                else Nothing
                       | otherwise -> Nothing
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
   takeWhile pb pc utf8@(ByteStringUTF8 bs) = ByteStringUTF8 $ ByteString.take (ByteString.length bs - ByteString.length suffix) bs
      where ByteStringUTF8 suffix = Textual.dropWhile pb pc utf8
   span pb pc utf8@(ByteStringUTF8 bs) = wrapPair $ ByteString.splitAt (ByteString.length bs - ByteString.length suffix) bs
      where ByteStringUTF8 suffix = Textual.dropWhile pb pc utf8
   break pb pc = Textual.span (not . pb) (not . pc)

wrapPair (bs1, bs2) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2)

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
   where n = ord c

headIndex bs = fromMaybe (ByteString.length bs) $ ByteString.findIndex byteStartsCharacter bs

byteStartsCharacter :: Word8 -> Bool
byteStartsCharacter b = b < 0x80 || b >= 0xC0

charStartIndex :: Int -> ByteString -> Int
charStartIndex n _ | n <= 0 = 0
charStartIndex n bs =
   case List.drop (pred n) (ByteString.findIndices byteStartsCharacter $ ByteString.drop 1 bs)
   of [] -> ByteString.length bs
      k:_ -> succ k
