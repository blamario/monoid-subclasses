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
import qualified Data.ByteString.UTF8 as UTF8

import Data.Monoid (Monoid)
import Data.Monoid.Cancellative (LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid)
import Data.Monoid.Null (MonoidNull)
import Data.Monoid.Factorial (FactorialMonoid(..))
import Data.Monoid.Textual (TextualMonoid(..))

newtype ByteStringUTF8 = ByteStringUTF8 ByteString deriving (Eq, Show, Monoid, MonoidNull,
                                                             LeftReductiveMonoid, LeftCancellativeMonoid, LeftGCDMonoid)

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

instance TextualMonoid ByteStringUTF8 where
   splitCharacterPrefix (ByteStringUTF8 bs) = do (c, rest) <- UTF8.uncons bs
                                                 if c == UTF8.replacement_char
                                                    then Nothing
                                                    else return (c, ByteStringUTF8 rest)


wrapPair (bs1, bs2) = (ByteStringUTF8 bs1, ByteStringUTF8 bs2)
