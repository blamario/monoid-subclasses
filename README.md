monoid-subclasses
=================

### Subclasses of Monoid with a solid theoretical foundation and practical purposes ###

The monoid-subclasses package has been released [on Hackage](http://hackage.haskell.org/package/monoid-subclasses). The package defines several classes that are richer than [monoids](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Monoid.html#t:Monoid) but less demanding than [groups](http://hackage.haskell.org/packages/archive/groups/0.1.0.1/doc/html/Data-Group.html)

That's the theoretical point of view. From the practical point of view, the main purpose of the monoid-subclasses package is similar to that of [ListLike](http://hackage.haskell.org/packages/archive/ListLike/latest/doc/html/Data-ListLike.html) - to provide unifying abstractions for various monoidal data types in Haskell, primarily `String`, `ByteString`, and `Text`. All three types are already instances of the `Monoid` class. That abstraction is useful for building sequences of data, but it cannot help with deconstructing them.

That being said, there are two major differences in the goals of `ListLike` and `monoid-subclasses`:
  * `ListLike` strives to reproduce the standard [Data.List](http://hackage.haskell.org/packages/archive/base/4.6.0.0/doc/html/Data-List.html) interface, while `monoid-subclasses` builds from deeper theoretical foundations; and
  * `monoid-subclasses` is standard Haskell 2010, with the exception of two minor extensions which can be worked around if necessary.
