Version 1.2
---------------
* Depending on new `commutative-semigroups` package
* Modified the `instance OverlappingMonoid/Monus Map/IntMap` instances to conform with the class laws
* Bumped the `vector` dependency upper bounds

Version 1.1.4
---------------
* Canonicalized all `mappend` definitions
* Added `deriving (Data, Typeable)` to all data types

Version 1.1.3
---------------
* Support for text-2.0 by Bodigrim

Version 1.1.2
---------------
* CI tests
* Fallback implementation of `stripCommonSuffix @Text` for GHCjs by Jack Kelly
* Fixed documentation bug #31, Factorial laws too strong

Version 1.1.1
---------------
* Fixed compilation with GHC 8.0.2
* `Positioned` doesn't use a column for zero-width characters any more

Version 1.1
---------------
* Added a new TextualMonoid method `toText` for performance
* Fixed the calculations of `column` of `LinePositioned`
* Changed the `column` of `LinePositioned` to be one-based
* `LinePositioned` now treats FF, CR, and TAB characters as
  special, in accordance with Haskell language specification.

Version 1.0.1
---------------
* Incremented the lower bound of the base dependency
* Fixed Haddock links

Version 1.0
---------------
* Fixed issue #24, unlawful LeftReductiveMonoid instance for Map
* Tightened the laws of GCD classes, dropped instances for Sum and Product
* Introduced the `Monus` class
* Introduced the `OverlappingGCDMonoid` class
* Added the instances of type `Sum Natural` and `Product Natural`
* Using the language extensions `FlexibleInstances` and `OverlappingInstances`
* Removed the linear complexity requirement
* Added and documented less efficient instances
* Moved various GCD classes into the new module `Data.Monoid.GCD`
* Added module `Data.Semigroup.Cancellative` with `Semigroup` subclasses
* Added module `Data.Semigroup.Factorial` with `Semigroup` subclasses
* Deprecated several `Monoid` subclasses and made them constraint synonyms instead:
  * `type CommutativeMonoid m = (Monoid m, Commutative m)`
  * `type ReductiveMonoid m = (Monoid m, Reductive m)`
  * `type LeftReductiveMonoid m = (Monoid m, LeftReductive m)`
  * `type RightReductiveMonoid m = (Monoid m, RightReductive m)`
  * `type CancellativeMonoid m = (Monoid m, Cancellative m)`
  * `type LeftCancellativeMonoid m = (Monoid m, LeftCancellative m)`
  * `type RightCancellativeMonoid m = (Monoid m, RightCancellative m)`

Version 0.4.6.1
---------------
Bumped the containers dependency upper bounds

Version 0.4.6
---------------
Added the Semigroup instances to fix the compilation errors with base-4.11

Version 0.4.4
---------------
Fixed boundary condition bugs in ByteStringUTF8 uncovered by a new version of QuickCheck

Version 0.4.3.2
---------------
Fixed compilation errors with GHC 7.8.4 and older

Version 0.4.3.1
---------------
Bumped the vector dependency upper bounds

Version 0.4.3
---------------
* Added instances for 3- and 4-tuples
* Re-implemented Concat as an own data type, dropping Seq

Version 0.4.2.1
---------------
* Fixed compilation problems with GHC 8 and containers-0.5.7
* Fixed compilation problems with GHC 8 and containers-0.5.7
* Merge pull request #10 from mgiles: minor typo in FactorialMonoid laws

Version 0.4.2
---------------
* Fixed a bug in splitAt implementation for ByteStringUTF8
* Merge pull request #9 from phadej: use newest quickcheck-instances
* Removed the overzealous assertions from ByteStringUTF8

Version 0.4.1.2
---------------
Removing accidental reference to Instances.Markup module

Version 0.4.1.1
---------------
* Bumped the vector dependency upper bounds
* Removed GHC-prof-options from the cabal file

Version 0.4.1
---------------
* Changed the Prelude imports to enable compilation with GHC 7.4
* Added INLINE pragmas
* Added the toString method to TextualMonoid class
* Importing Text.Show.Functions to avoid overlapping instances
* Eliminated the redundant import warnings from GHC 7.10.1

Version 0.4.0.4
---------------
* Added -Wall GHC option and eliminated almost all the warnings
* Fixed a bug in the Textual instance of ByteStringUTF8

Version 0.4.0.3
---------------
* Excluding the imports of foldMap from Prelude

Version 0.4.0.2
---------------
* Added more tests and fixed a bug in Stateful
* Fixed a bug in Positioned.span_
* Optimized the Stateful data type

Version 0.3.6.2
---------------
* Added a bunch of pragmas

Version 0.3.6
---------------
* Deprecated all the inject functions
* Registered the new Stateful module

Version 0.3.4.1
---------------
Accommodating the text-1.0 release

* Introduced the function ByteStringUTF8.decode
* Removed the utf-string dependency
* Replaced the utf-string import by a more efficient UTF-8 encoding

Version 0.3.1
---------------
* Added the Data.Monoid.Instances.Concat module and tests
* Added the PositiveMonoid class
* Added the StableFactorialMonoid subclass of FactorialMonoid
* Added more instances for ()

Version 0.3
---------------
Added the CommutativeMonoid class at the root of the Cancellative classes

Version 0.2
---------------
* Added TextualMonoid instances for Seq Char and Vector Char
* Renamed the FactorialMonoid method map to foldMap in keeping with Foldable

Version 0.1.2
---------------
Optimizations of the default Factorial methods and of the ButeStringUTF8 instances

Version 0.1
---------------
Initial release
