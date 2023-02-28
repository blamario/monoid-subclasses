{-# LANGUAGE Haskell2010, FlexibleInstances #-}

-- | This module defines the 'LCMMonoid' subclass of the 'Monoid' class.
--
-- The 'LCMMonoid' subclass adds the 'lcm' operation, which takes two monoidal
-- arguments and finds their /least common multiple/, or (more generally) the
-- least monoid from which either argument can be subtracted with the '</>'
-- operation.
--
-- The 'LCMMonoid' class is for Abelian, /i.e./, 'Commutative' monoids.
--
module Data.Monoid.LCM (
    LCMMonoid (..)
    )
where

import Prelude hiding (lcm, max)
import qualified Prelude

import Data.IntSet (IntSet)
import Data.Monoid (Dual (..), Product (..), Sum (..))
import Data.Monoid.GCD (GCDMonoid)
import Data.Set (Set)
import Numeric.Natural (Natural)
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set

-- These imports are marked as redundant, but are actually required by haddock:
import Data.Maybe (isJust)
import Data.Semigroup.Cancellative (Reductive ((</>)))
import Data.Semigroup.Commutative (Commutative)

-- | Class of Abelian monoids that allow the /least common multiple/ to be
--   found for any two given values.
--
-- Operations must satisfy the following laws:
--
-- __/Reductivity/__
--
-- @
-- 'isJust' ('lcm' a b '</>' a)
-- @
-- @
-- 'isJust' ('lcm' a b '</>' b)
-- @
--
-- __/Uniqueness/__
--
-- @
-- 'all' 'isJust'
--     [ \   \   c '</>' a
--     , \   \   c '</>' b
--     , 'lcm' a b '</>' c
--     ]
-- ==>
--     ('lcm' a b '==' c)
-- @
--
-- __/Idempotence/__
--
-- @
-- 'lcm' a a '==' a
-- @
--
-- __/Identity/__
--
-- @
-- 'lcm' 'mempty' a '==' a
-- @
-- @
-- 'lcm' a 'mempty' '==' a
-- @
--
-- __/Commutativity/__
--
-- @
-- 'lcm' a b '==' 'lcm' b a
-- @
--
-- __/Associativity/__
--
-- @
-- 'lcm' ('lcm' a b) c '==' 'lcm' a ('lcm' b c)
-- @
--
-- __/Absorption/__
--
-- @
-- 'lcm' a ('gcd' a b) '==' a
-- @
-- @
-- 'gcd' a ('lcm' a b) '==' a
-- @
--
-- __/Distributivity/__
--
-- @
-- 'lcm' (a '<>' b) (a '<>' c) '==' a '<>' 'lcm' b c
-- @
-- @
-- 'lcm' (a '<>' c) (b '<>' c) '==' 'lcm' a b '<>' c
-- @
-- @
-- 'lcm' a ('gcd' b c) '==' 'gcd' ('lcm' a b) ('lcm' a c)
-- @
-- @
-- 'gcd' a ('lcm' b c) '==' 'lcm' ('gcd' a b) ('gcd' a c)
-- @
--
class GCDMonoid m => LCMMonoid m where
    lcm :: m -> m -> m

instance LCMMonoid () where
    lcm () () = ()

instance LCMMonoid a => LCMMonoid (Dual a) where
    lcm (Dual a) (Dual b) = Dual (lcm a b)

instance LCMMonoid (Product Natural) where
    lcm (Product a) (Product b) = Product (Prelude.lcm a b)

instance LCMMonoid (Sum Natural) where
    lcm (Sum a) (Sum b) = Sum (Prelude.max a b)

instance Ord a => LCMMonoid (Set a) where
    lcm = Set.union

instance LCMMonoid IntSet where
    lcm = IntSet.union

instance (LCMMonoid a, LCMMonoid b) => LCMMonoid (a, b) where
    lcm (a0, a1) (b0, b1) =
        (lcm a0 b0, lcm a1 b1)

instance (LCMMonoid a, LCMMonoid b, LCMMonoid c) => LCMMonoid (a, b, c) where
    lcm (a0, a1, a2) (b0, b1, b2) =
        (lcm a0 b0, lcm a1 b1, lcm a2 b2)

instance (LCMMonoid a, LCMMonoid b, LCMMonoid c, LCMMonoid d) =>
    LCMMonoid (a, b, c, d)
  where
    lcm (a0, a1, a2, a3) (b0, b1, b2, b3) =
        (lcm a0 b0, lcm a1 b1, lcm a2 b2, lcm a3 b3)
