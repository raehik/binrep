# To-dos
## Think about
  * say it's for defining byte-oriented binary representations, particularly
    file formats (where byte-oriented is the norm). not well suited for
    bit-oriented formats such as some wire formats/network data representations
    * you can still encode bit-oriented data, but it has to be packaged as
      byte-aligned, because you cannot get/put less than a byte at a time.
      so for bit fields you'll need bitwise shifts. e.g. IPv4 packet header
    * there's a cool bit-oriented (= compact!) data representation out there
      with a Haskell focus called Flat: https://hackage.haskell.org/package/flat
      should be (?) bad for efficiency and tough for complexity, but cool idea

## Simple
  * properly test the default `AsByteString 'C` sum tag handler (!)

## Faster serialization
According to
[strict `ByteString` builder benchmarks](https://github.com/haskell-perf/strict-bytestring-builders)
from Nikita Volkov (also see
[mason's benchmarks](https://github.com/fumieval/mason#performance)),
cereal is one of the slower options for serializing.

  * fumieval's [mason](https://github.com/fumieval/mason) library seems like a
    good bet. Well maintained by an extremely clever guy, apparently wins most
    measurements.
  * There's also Nikita Volkov's
    [bytestring-strict-builder](https://hackage.haskell.org/package/bytestring-strict-builder),
    which is slightly faster for some measurements, but seems a bit
    inconsistent.

## Generate (human-readable) schema from type
I think I do this by writing yet another typeclass, filling it out for my
primitives, then writing yet another generic typeclass. But I'd like to make
this really powerful. Send an `a_generic_info -> b` function along or something,
so we can provide a ton of different schema formats for free. e.g write to plain
pretty string, write to a JSON doc, write to some description language...

Actually, I think you send a bunch of functions along. So that's why people use
a typeclass instead, for this "no middle man" thing. Should be fun.

## More primitives
  * Varint
    * `newtype Varint sign end = Varint { unVarint :: VarintRep sign }`
    * apparently `Varint s 'LE` would be LEB128, `Varint s 'BE` would be VLQ
    * https://developers.google.com/protocol-buffers/docs/encoding , Mason
    * byte-oriented, so each byte is 0-127 instead of 0-255.
    * protobuf uses little endian, twos complement
    * Wikipedia suggests we could support signed & unsigned:
      https://en.wikipedia.org/wiki/LEB128
  * ZigZag
    * Maps signed to unsigned
    * `newtype ZigZag size end = ZigZag { unZigZag :: I 'S size end }`
    * https://developers.google.com/protocol-buffers/docs/encoding
    * https://hackage.haskell.org/package/zigzag-0.0.1.0/docs/Data-Word-Zigzag.html

## Provide "deriving via" generic derivers
Seems to be all the craze lately. See generic-random:
https://hackage.haskell.org/package/generic-random-1.5.0.1/docs/Generic-Random-DerivingVia.html

This would let me do something rather interesting -- instead of use newtypes all
over and forcing the user to endlessly wrap and unwrap, I could perhaps let them
use the regular old type, but derive a specialized instance through a newtype.

Doesn't work with `WithRefine`, since the user needs to handle those themselves
(that's the point). But neat idea.

Ah - also means no user-supplied functions, because no type-level function
passing. See fumieval's
[deriving-aeson](https://hackage.haskell.org/package/deriving-aeson).
