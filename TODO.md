# binrep to-dos
## Write more primitives
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

## Define types for some common file formats
* How about a shared type for xz

## Test new parser errors
Where we keep track of the offset. It's very weird and probably bad :(

## Octet instead of byte?
Is it better to refer to octets instead of bytes? An octet is always 8 bits,
while a byte is kind of "not necessarily".

## Safety against unknown inputs
Consider implementing "practical maxes" for various types.

  * https://github.com/multiformats/unsigned-varint does this
  * So do Haskell's protobufs:
    https://hackage.haskell.org/package/protocol-buffers-2.4.17/docs/src/Text.ProtocolBuffers.Get.html#decode7unrolled

May help prevent unexpected OOMs?

## Generate (human-readable) schema from type
I think I do this by writing yet another typeclass, filling it out for my
primitives, then writing yet another generic typeclass. But I'd like to make
this really powerful. Send an `a_generic_info -> b` function along or something,
so we can provide a ton of different schema formats for free. e.g write to plain
pretty string, write to a JSON doc, write to some description language...

Actually, I think you send a bunch of functions along. So that's why people use
a typeclass instead, for this "no middle man" thing. Should be fun.
