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

import Prelude hiding (foldl, foldl1, foldr, foldr1, scanl, scanr, scanl1, scanr1, map, concatMap, break, span)

import Data.String (IsString(fromString))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Unsafe (unsafeDrop, unsafeIndex)
import qualified Data.ByteString.UTF8 as UTF8

import Data.Monoid (Monoid)
import Data.Monoid.Cancellative (LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid)
import Data.Monoid.Null (MonoidNull)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))
import qualified Data.Monoid.Factorial as Factorial (FactorialMonoid(..))
import qualified Data.Monoid.Textual as Textual (TextualMonoid(..))

newtype ByteStringUTF8 = ByteStringUTF8 ByteString deriving (Eq, Monoid, MonoidNull,
                                                             LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid)

instance Show ByteStringUTF8 where
   show (ByteStringUTF8 bs) = show (UTF8.toString bs)

instance IsString ByteStringUTF8 where
   fromString = ByteStringUTF8 . UTF8.fromString

instance FactorialMonoid ByteStringUTF8 where
   splitPrimePrefix (ByteStringUTF8 bs) = 
      do (_, n) <- UTF8.decode bs
         let (bytes, rest) = ByteString.splitAt n bs
         return (ByteStringUTF8 bytes, ByteStringUTF8 rest)
   splitAt n (ByteStringUTF8 bs) = wrapPair (UTF8.splitAt n bs)
   take n (ByteStringUTF8 bs) = ByteStringUTF8 (UTF8.take n bs)
   drop n (ByteStringUTF8 bs) = ByteStringUTF8 (UTF8.drop n bs)
   length (ByteStringUTF8 bs) = UTF8.length bs
   span p (ByteStringUTF8 bs) = wrapPair (loop 0)
      where limit = ByteString.length bs
            loop i = if i < limit
                     then let w = unsafeIndex bs i
                          in if w < 0x80
                             then if p (ByteStringUTF8 $ ByteString.singleton w)
                                  then loop (succ i)
                                  else ByteString.splitAt i bs
                             else let cs = ByteString.drop i bs
                                  in case UTF8.decode cs
                                     of Just (_,n) | p (ByteStringUTF8 $ ByteString.take n cs)
                                                     -> loop (i+n)
                                        _ -> ByteString.splitAt i bs
                     else (bs, ByteString.empty)
   break p = Factorial.span (not . p)
   takeWhile p = fst . Factorial.span p
   dropWhile p = snd . Factorial.span p

instance TextualMonoid ByteStringUTF8 where
   splitCharacterPrefix (ByteStringUTF8 bs) = do (c, rest) <- UTF8.uncons bs
                                                 if c == UTF8.replacement_char
                                                    then Nothing
                                                    else return (c, ByteStringUTF8 rest)
   span pb pc (ByteStringUTF8 bs) = wrapPair (spanASCII 0 bs)
      where spanASCII i rest = case ByteString.Char8.findIndex (\c-> c > '\x7f' || not (pc c)) rest
                               of Nothing -> (bs, ByteString.empty)
                                  Just j -> if unsafeIndex rest j > 0x7f
                                            then spanMultiByte (i + j) (unsafeDrop j rest)
                                            else ByteString.splitAt (i + j) bs
            spanMultiByte i rest = case UTF8.decode rest
                                   of Just (c,n) | if c == UTF8.replacement_char
                                                      then pb (ByteStringUTF8 $ ByteString.take n rest)
                                                      else pc c
                                                   -> spanASCII (i+n) (unsafeDrop n rest)
                                      _ -> ByteString.splitAt i bs
   break pb pc = Textual.span (not . pb) (not . pc)
   takeWhile pb pc = fst . Textual.span pb pc
   dropWhile pb pc = snd . Textual.span pb pc

wrapPair (bs1, bs2) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2)
