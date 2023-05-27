[hackage-flatparse]: https://hackage.haskell.org/package/flatparse
[hackage-megaparsec]: https://hackage.haskell.org/package/megaparsec

# senserial
senserial is a small library providing reusable generics for (binary) parsers
and serializers. No need to muddle through boilerplate generics that look the
same as everyone else's; just provide a few definitions and senserial can give
you powerful generic instances.

## Why?
It is 2023. There are a number of competing parsing and serialization Haskell
libraries, and some notable high-performance binary serialization libraries.
These are often fairly experimental. Maybe you want some generics to benchmark
some real-world use case against popular libraries like binary and cereal. But
maybe generics aren't provided. Shucks.

That's a shame, because a pure generic binary parser or serializer doesn't have
much work to do:

  * traverse the generic sum-of-products tree of the given type left to right
  * defer to the appropriate type class for base cases

Sum types necessitate a little more work. Otherwise, most generic binary parsers
and serializers look fairly comparable to each other. Why are we rewriting this
stuff over and over again?

senserial provides *reusable generics* which have holes in for your favourite
parsers and serializers. Fill out a few definitions to receive a fresh new
generic instance for your own library, without all the boilerplate.

## Really?
Kind of. In reality, this library can only handle cases where no configuration
is needed other than what is provided in the data type itself. senserial
provides the generic traversal, and you can't alter that. Plus, the only
often rewritten and straightforward traversal I can think of is sequential field
concatenation. So though the code isn't limited to bytestrings and binary
serialization formats, you will have trouble using it for anything else, because
anything else will probably require a very different traversal (e.g. JSON
serialization).

In short, the primary use of this library is to pull out the common generics
patterns from binary serialization libraries for easy reuse.

## Notes
### Orphan instances
This library is designed to work with and around existing libraries and type
classes. Unless you add a senserial instance to your parser/serializer
definition site, you will be dealing with orphan instances. That's life, Jim.

## License
Provided under the MIT license. See `LICENSE` for license text.
