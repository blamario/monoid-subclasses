{- 
    Copyright 2013-2019 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'GCDMonoid' and 'Monus' subclasses of the 'Monoid' class.
--
-- The 'GCDMonoid' subclass adds the 'gcd' operation which takes two monoidal arguments and finds their greatest
-- common divisor, or (more generally) the greatest monoid that can be extracted with the '</>' operation from both.
--
-- The two above classes are for Abelian, /i.e./, commutative monoids. Since most practical monoids in Haskell are not
-- Abelian, there are also three symmetric superclasses:
-- 
-- * 'LeftGCDMonoid'
-- 
-- * 'RightGCDMonoid'
-- 
-- * 'OverlappingGCDMonoid'

{-# LANGUAGE Haskell2010, ConstraintKinds, FlexibleInstances, Trustworthy #-}

module Data.Monoid.Cancellative {-# DEPRECATED "Use Data.Semigroup.Cancellative and Data.Monoid.GCD instead" #-} (
   module Data.Semigroup.Cancellative,
   -- * Symmetric, commutative monoid classes
   CommutativeMonoid, ReductiveMonoid, CancellativeMonoid, GCDMonoid(..),
   -- * Asymmetric monoid classes
   LeftReductiveMonoid, RightReductiveMonoid,
   LeftCancellativeMonoid, RightCancellativeMonoid,
   LeftGCDMonoid(..), RightGCDMonoid(..)
   )
where

import qualified Prelude

import Data.Monoid (Monoid)

import Data.Semigroup.Cancellative
import Data.Monoid.GCD

{-# DEPRECATED CommutativeMonoid "Use Data.Semigroup.Cancellative.Commutative instead." #-}
{-# DEPRECATED ReductiveMonoid "Use Data.Semigroup.Cancellative.Reductive instead." #-}
{-# DEPRECATED LeftReductiveMonoid "Use Data.Semigroup.Cancellative.LeftReductive instead." #-}
{-# DEPRECATED RightReductiveMonoid "Use Data.Semigroup.Cancellative.RightReductive instead." #-}
{-# DEPRECATED CancellativeMonoid "Use Data.Semigroup.Cancellative.Cancellative instead." #-}
{-# DEPRECATED LeftCancellativeMonoid "Use Data.Semigroup.Cancellative.LeftCancellative instead." #-}
{-# DEPRECATED RightCancellativeMonoid "Use Data.Semigroup.Cancellative.RightCancellative instead." #-}
type CommutativeMonoid m = (Monoid m, Commutative m)
type ReductiveMonoid m = (Monoid m, Reductive m)
type LeftReductiveMonoid m = (Monoid m, LeftReductive m)
type RightReductiveMonoid m = (Monoid m, RightReductive m)
type CancellativeMonoid m = (Monoid m, Cancellative m)
type LeftCancellativeMonoid m = (Monoid m, LeftCancellative m)
type RightCancellativeMonoid m = (Monoid m, RightCancellative m)
