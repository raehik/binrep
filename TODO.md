# To-dos
## Intermediate types should use the bytestring builder type, not a bytestring
Saves on allocations if you're just gonna serialize. Specifically, the text
modules probably want this. Not sure how to do it exactly to retain maximum
soundness.

This might be a big change. I need to check aeson's design more thoroughly, it'd
look like the `Encoding` stuff.

2023-02-19: No, actually, I think this is a Text-unique thing, and they just
need some extra definitions.

## Serialize with bytezap
All change!

## ~Special static parser like peeky-blinders~
How about

```haskell
newtype Peek a = Addr# -> State# RealWorld -> (# State# RealWorld, a, Addr# #)
```

The problem is that it's just a shitty flatparse, where all you can do is march
on forward. No errors other than the length check at the start (which is
separate, handled by `CBLen`). Which is sad, because the fancy parsing is a
feature of binrep. Sure, I could replace the low-level parsers with this
peeky-blinders-style parser, but then I lose the error handling.

Changing this ever so slightly to support more features simply ends up
approximating flatparse, which is stupid. No, I'm settled: flatparse-only, baby.

## Octet instead of byte?
Is it better to refer to octets instead of bytes? An octet is always 8 bits,
while a byte is kind of "not necessarily".

## Safety against unknown inputs
Consider implementing "practical maxes" for various types.

  * https://github.com/multiformats/unsigned-varint does this
  * So do Haskell's protobufs:
    https://hackage.haskell.org/package/protocol-buffers-2.4.17/docs/src/Text.ProtocolBuffers.Get.html#decode7unrolled

May help prevent unexpected OOMs?

## Structured parse errors like strongweak
It means lots of work wrapping flatparse and making my own various combinators,
but otherwise everything's already there.

## More primitives
  * Varint
    * base done, but doesn't support everything
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

## Generate (human-readable) schema from type
I think I do this by writing yet another typeclass, filling it out for my
primitives, then writing yet another generic typeclass. But I'd like to make
this really powerful. Send an `a_generic_info -> b` function along or something,
so we can provide a ton of different schema formats for free. e.g write to plain
pretty string, write to a JSON doc, write to some description language...

Actually, I think you send a bunch of functions along. So that's why people use
a typeclass instead, for this "no middle man" thing. Should be fun.

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
