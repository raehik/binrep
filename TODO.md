# To-dos
## Challenges with unbuffered serialization
### Floats to decimal ASCII
Serializing floats to ASCII efficiently is hard. Most of the recent fast
algorithms `malloc` ~25 bytes at the start then tell you how long it actually
was. I see little in the way of "figure out for cheap how many characters long
this float will be once serialized".

mason wins with its builtin grisu3. I think beating that is Hard. bytestring
actually uses ryu internally! But via a Haskell implementation.
[ryu](https://github.com/ulfjack/ryu/blob/master/ryu/d2s.c) has some stuff.
[Dragonbox](https://github.com/jk-jeon/dragonbox) is better, but it's C++. Most
likely I am on my own here. Perhaps I can figure out some shortcuts in grisu3 to
calculate serialized length without doing *all* the work. If I can avoid
`malloc`ing, it would be a winner.

Perhaps I simply pray to SPJ for good caching behaviour. Running the inner
serializer in `blen` *should* mean it can be reused for `put`, and providing
they occur close to each other (which they do in `runPut`), I could see it
happening.

### Ints to decimal ASCII
This should be OK I think? I wonder how fast we can go, though. Cool C++ lib at
[jeaiii/itoa](https://github.com/jeaiii/itoa). Probably a waste of time, use
bytestring's prims along with a fast `BLen` check (I wrote one before I think).

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

## Octet instead of byte?
Is it better to refer to octets instead of bytes? An octet is always 8 bits,
while a byte is kind of "not necessarily".

## Safety against unknown inputs
Consider implementing "practical maxes" for various types.

  * https://github.com/multiformats/unsigned-varint does this
  * So do Haskell's protobufs:
    https://hackage.haskell.org/package/protocol-buffers-2.4.17/docs/src/Text.ProtocolBuffers.Get.html#decode7unrolled

May help prevent unexpected OOMs?

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
