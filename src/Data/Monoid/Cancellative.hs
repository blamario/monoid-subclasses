{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'Monoid' => 'CommutativeMonoid' => 'ReductiveMonoid' => 'CancellativeMonoid' constraint
-- synonym hierarchy.
--
-- Since most practical monoids in Haskell are not commutative, the last two of these synonyms have two symmetric
-- superclasses each:
-- 
-- * 'LeftReductiveMonoid'
-- 
-- * 'LeftCancellativeMonoid'
-- 
-- * 'RightReductiveMonoid'
-- 
-- * 'RightCancellativeMonoid'
--
-- This module and its constraint synonyms are provided for compatibility with the older versions of the
-- @monoid-sublasses@ library. Starting with version 1.0, the classes from the "Data.Semigroup.Cancellative" module
-- are recommended instead.

{-# LANGUAGE Haskell2010, ConstraintKinds, FlexibleInstances #-}

module Data.Monoid.Cancellative {- from 1.1 DEPRECATED "Use \"Data.Semigroup.Cancellative\" and \"Data.Monoid.GCD\" instead" -} (
   module Data.Semigroup.Cancellative,
   module Data.Monoid.GCD,
   -- * Symmetric, commutative monoid classes
   CommutativeMonoid, ReductiveMonoid, CancellativeMonoid,
   -- * Asymmetric monoid classes
   LeftReductiveMonoid, RightReductiveMonoid,
   LeftCancellativeMonoid, RightCancellativeMonoid
   )
where

import Data.Monoid (Monoid)

import Data.Semigroup.Cancellative
import Data.Monoid.GCD

{- from 1.1-}
{- DEPRECATED CommutativeMonoid "Use Data.Semigroup.Cancellative.Commutative instead." -}
{- DEPRECATED ReductiveMonoid "Use Data.Semigroup.Cancellative.Reductive instead." -}
{- DEPRECATED LeftReductiveMonoid "Use Data.Semigroup.Cancellative.LeftReductive instead." -}
{- DEPRECATED RightReductiveMonoid "Use Data.Semigroup.Cancellative.RightReductive instead." -}
{- DEPRECATED CancellativeMonoid "Use Data.Semigroup.Cancellative.Cancellative instead." -}
{- DEPRECATED LeftCancellativeMonoid "Use Data.Semigroup.Cancellative.LeftCancellative instead." -}
{- DEPRECATED RightCancellativeMonoid "Use Data.Semigroup.Cancellative.RightCancellative instead." -}
type CommutativeMonoid m = (Monoid m, Commutative m)
type ReductiveMonoid m = (Monoid m, Reductive m)
type LeftReductiveMonoid m = (Monoid m, LeftReductive m)
type RightReductiveMonoid m = (Monoid m, RightReductive m)
type CancellativeMonoid m = (Monoid m, Cancellative m)
type LeftCancellativeMonoid m = (Monoid m, LeftCancellative m)
type RightCancellativeMonoid m = (Monoid m, RightCancellative m)
