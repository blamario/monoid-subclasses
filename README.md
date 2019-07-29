monoid-subclasses
=================

### Subclasses of Monoid with a solid theoretical foundation and practical purposes ###

The monoid-subclasses package has been released [on Hackage](http://hackage.haskell.org/package/monoid-subclasses). The package defines several classes that are richer than [monoids](http://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Monoid) but less demanding than [groups](http://hackage.haskell.org/package/groups/docs/Data-Group.html):
  * [ReductiveMonoid](http://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Cancellative.html#t:ReductiveMonoid) provides the operator `</>` which acts as a partial inverse of the `<>` operator, _i.e._, `Monoid.mappend`.
  * [CancellativeMonoid](http://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Cancellative.html#t:CancellativeMonoid) is a subclass of `ReductiveMonoid` that provides additional guarantees about the `</>` operation result:

        (a <> b) </> a == Just b
        (a <> b) </> b == Just a

    Every group (<em>i.e.</em>, every `Monoid a` with the operation `inverse :: a -> a`) is a cancellative monoid where `a </> b = Just (a <> inverse b)` but not every `CancellativeMonoid` is a group.
  * [GCDMonoid](http://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Cancellative.html#t:GCDMonoid) is a subclass of `ReductiveMonoid` that provides the `gcd` operation for getting the greatest common denominator for two given monoid values.
  * [MonoidNull](http://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Null.html) class provides the Boolean `null` operation that checks if the argument monoid is `mempty`.
  * [FactorialMonoid](http://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Factorial.html) class represents monoids that can be split up into irreducible factors.

That's the theoretical point of view. From the practical point of view, the main purpose of the _monoid-subclasses_ package is similar to that of [ListLike](http://hackage.haskell.org/package/ListLike/docs/Data-ListLike.html) - to provide unifying abstractions for various monoidal data types in Haskell, primarily [String](http://hackage.haskell.org/package/base/docs/Data-String.html#t:String), [ByteString](http://hackage.haskell.org/package/bytestring/docs/Data-ByteString.html#t:ByteString), and [Text](http://hackage.haskell.org/package/text). All three types are already instances of the [Monoid](http://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Monoid) class. While that abstraction is useful for building sequences of data, it doesn't help with deconstructing them.

That being said, there are two major differences in the goals of _ListLike_ and _monoid-subclasses_:
  * _ListLike_ strives to reproduce the standard [Data.List](http://hackage.haskell.org/package/base/docs/Data-List.html) interface, whereas _monoid-subclasses_ builds from deeper theoretical foundations; and
  * The _monoid-subclasses_ implementation uses standard Haskell 2010, with the exception of two minor extensions which can be worked around if necessary.

The [incremental-parser](http://hackage.haskell.org/package/incremental-parser) package provides one example of use of _monoid-subclasses_. Another example is [picoparsec](https://bitbucket.org/blamario/picoparsec), a fork of [attoparsec](http://hackage.haskell.org/package/attoparsec).

A more thorough description of the library can be found in the Haskell Symposium 2013 paper [Adding Structure to Monoids
](https://github.com/blamario/monoid-subclasses/wiki/Files/HaskellSymposium2013.pdf)

