{- 
    Copyright 2013 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'ByteStringUTF8' newtype wrapper around 'ByteString', together with its 'TextualMonoid'
-- instance.
-- 

module Data.Monoid.Instances.ByteString.UTF8 (
   ByteStringUTF8(..), decode
   )
where

import Prelude hiding (drop, dropWhile, foldl, foldl1, foldr, foldr1, scanl, scanr, scanl1, scanr1,
                       map, concatMap, break, span)

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

import Data.Monoid (Monoid(mempty, mappend))
import Data.Monoid.Cancellative (LeftReductiveMonoid(..), LeftCancellativeMonoid, LeftGCDMonoid(..))
import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)
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
   ByteStringUTF8 a `mappend` ByteStringUTF8 b = ByteStringUTF8 (a `mappend` b)

instance MonoidNull ByteStringUTF8 where
   null (ByteStringUTF8 b) = ByteString.null b

instance LeftReductiveMonoid ByteStringUTF8 where
   stripPrefix (ByteStringUTF8 a) (ByteStringUTF8 b) = fmap ByteStringUTF8 (stripPrefix a b)
   ByteStringUTF8 a `isPrefixOf` ByteStringUTF8 b = a `isPrefixOf` b

instance LeftCancellativeMonoid ByteStringUTF8

instance LeftGCDMonoid ByteStringUTF8 where
   commonPrefix (ByteStringUTF8 a) (ByteStringUTF8 b) = ByteStringUTF8 (commonPrefix a b)
   stripCommonPrefix (ByteStringUTF8 a) (ByteStringUTF8 b) = wrapTriple (stripCommonPrefix a b)

instance Show ByteStringUTF8 where
   showsPrec _ bs s = '"' : Textual.foldr showsBytes (:) ('"' : s) bs
      where showsBytes (ByteStringUTF8 b) s = '\\' : shows (ByteString.unpack b) s

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
   splitPrimeSuffix utf8@(ByteStringUTF8 bs)
      | ByteString.null bs = Nothing
      | ByteString.null prefix = Just (wrapPair split)
      | not (ByteString.null suffix) && ByteString.last prefix < 0x80 = Just (wrapPair split)
      | otherwise = Just (wrapPair $ ByteString.splitAt (pred $ ByteString.length prefix) bs)
      where split@(prefix, suffix) = ByteString.breakEnd byteStartsCharacter bs
   primePrefix utf8@(ByteStringUTF8 bs)
      | ByteString.null bs = utf8
      | unsafeHead bs < 0x80 = ByteStringUTF8 (ByteString.take 1 bs)
      | otherwise = case ByteString.findIndex byteStartsCharacter (unsafeTail bs)
                    of Just i -> ByteStringUTF8 (ByteString.take (succ i) bs)
                       Nothing -> utf8
   factors (ByteStringUTF8 bs) = List.map ByteStringUTF8 $ ByteString.groupBy continued bs
      where continued a b = a >= 0x80 && b >= 0x80 && b < 0xC0
   length (ByteStringUTF8 bs) = fst (ByteString.foldl' count (0, False) bs)
      where count (n, high) byte | byte < 0x80 = (succ n, False)
                                 | byte < 0xC0 = (if high then n else succ n, True)
                                 | otherwise = (succ n, True)
   foldl f a0 (ByteStringUTF8 bs) = List.foldl f' a0 (groupASCII bs)
      where f' a b | unsafeHead b < 0x80 = ByteString.foldl f'' a b
                   | otherwise = f a (ByteStringUTF8 b)
            f'' a w = f a (ByteStringUTF8 $ ByteString.singleton w)
   foldl' f a0 (ByteStringUTF8 bs) = List.foldl' f' a0 (groupASCII bs)
      where f' a b | unsafeHead b < 0x80 = ByteString.foldl' f'' a b
                   | otherwise = f a (ByteStringUTF8 b)
            f'' a w = f a (ByteStringUTF8 $ ByteString.singleton w)
   foldr f a0 (ByteStringUTF8 bs) = List.foldr f' a0 (groupASCII bs)
      where f' b a | unsafeHead b < 0x80 = ByteString.foldr f'' a b
                   | otherwise = f (ByteStringUTF8 b) a
            f'' w a = f (ByteStringUTF8 $ ByteString.singleton w) a
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
   reverse (ByteStringUTF8 bs) =
      ByteStringUTF8 (ByteString.concat $ List.reverse $ List.map reverseASCII $ groupASCII bs)
      where reverseASCII b | unsafeHead b < 0x80 = ByteString.reverse b
                           | otherwise = b

instance TextualMonoid ByteStringUTF8 where
   singleton = ByteStringUTF8 . fromChar
   splitCharacterPrefix (ByteStringUTF8 bs) = ByteString.uncons bs >>= uncurry toChar
   foldl ft fc a0 (ByteStringUTF8 bs) = List.foldl f a0 (groupASCII bs)
      where f a b = let hd = unsafeHead b
                    in if hd < 0x80
                       then ByteString.Char8.foldl fc a b
                       else maybe (ft a $ ByteStringUTF8 b) (fc a . fst) (toChar hd $ unsafeTail b)
   foldl' ft fc a0 (ByteStringUTF8 bs) = List.foldl' f a0 (groupASCII bs)
      where f a b = let hd = unsafeHead b
                    in if hd < 0x80
                       then ByteString.Char8.foldl' fc a b
                       else maybe (ft a $ ByteStringUTF8 b) (fc a . fst) (toChar hd $ unsafeTail b)
   foldr ft fc a0 (ByteStringUTF8 bs) = List.foldr f a0 (groupASCII bs)
      where f b a = let hd = unsafeHead b
                    in if hd < 0x80
                       then ByteString.Char8.foldr fc a b
                       else maybe (ft (ByteStringUTF8 b) a) (flip fc a . fst) (toChar hd $ unsafeTail b)
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
   takeWhile pb pc utf8@(ByteStringUTF8 bs) =
      ByteStringUTF8 $ ByteString.take (ByteString.length bs - ByteString.length suffix) bs
      where ByteStringUTF8 suffix = Textual.dropWhile pb pc utf8
   span pb pc utf8@(ByteStringUTF8 bs) =
      wrapPair $ ByteString.splitAt (ByteString.length bs - ByteString.length suffix) bs
      where ByteStringUTF8 suffix = Textual.dropWhile pb pc utf8
   break pb pc = Textual.span (not . pb) (not . pc)

wrapPair (bs1, bs2) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2)
wrapTriple (bs1, bs2, bs3) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2, ByteStringUTF8 bs3)

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

toChar :: Word8 -> ByteString -> Maybe (Char, ByteStringUTF8)
toChar hd tl | hd < 0x80 = Just (chr $ fromIntegral hd, ByteStringUTF8 tl)
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

headIndex bs = fromMaybe (ByteString.length bs) $ ByteString.findIndex byteStartsCharacter bs

byteStartsCharacter :: Word8 -> Bool
byteStartsCharacter b = b < 0x80 || b >= 0xC0

charStartIndex :: Int -> ByteString -> Int
charStartIndex n _ | n <= 0 = 0
charStartIndex n bs =
   case List.drop (pred n) (ByteString.findIndices byteStartsCharacter $ ByteString.drop 1 bs)
   of [] -> ByteString.length bs
      k:_ -> succ k
