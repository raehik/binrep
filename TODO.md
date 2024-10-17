# binrep to-dos
## Separate from strongweak
I don't use strongweak here, only write a few useful instances.
I could have a separate package for orphan instances instead.
This would make me much happier about:

* the `WeakenN` instances, which are the only "through" newtype currently
  permitted in binrep -- these would be over there
* the `ByteOrdered` orphans -- they would be over there

But I feel comfortable with the `Magic` weakening being here. And strongweak is
a small package. But still...

Yes, I think I should do this. No, I don't think it's very interesting or fun.

## Discrepancy in generic sum interface
In `Get`, I require a `Getter pt`. But in `Put`/`BLen`, you have to pass the
`Putter`/`BLen` manually. I'm not sure which one is correct. Probably `Get`.

## Move `Binrep.Type.Text`
It's fine, but it's not binrep (module imports appear separate).

## Clarify `Thin` stuff
I use the `ByteString` instance internally sometimes (probably), and in those
cases I want "thin" parsing since I'm consuming it right there and then. I need
to clarify the precise setup here.

## Assert fast magic handling is safe on either endian
I think we do endian-neutral operations, but do confirm.

## Extra primitive types, instances
* more list likes for `Prefix.Count`, maybe other places
  * `vector` for one. maybe others...?
* new primitive: Varint
  * base done, but doesn't support everything
  * `newtype Varint sign end = Varint { unVarint :: VarintRep sign }`
  * apparently `Varint s 'LE` would be LEB128, `Varint s 'BE` would be VLQ
  * https://developers.google.com/protocol-buffers/docs/encoding , Mason
  * byte-oriented, so each byte is 0-127 instead of 0-255.
  * protobuf uses little endian, twos complement
  * Wikipedia suggests we could support signed & unsigned:
    https://en.wikipedia.org/wiki/LEB128
* new primitive: ZigZag
  * maps signed to unsigned
  * `newtype ZigZag size end = ZigZag { unZigZag :: I 'S size end }`
  * https://developers.google.com/protocol-buffers/docs/encoding
  * https://hackage.haskell.org/package/zigzag-0.0.1.0/docs/Data-Word-Zigzag.html
* new primitive: ASCII numbers (for GNU tar)
  * urgh this one sucks, think I only got part way on flatparse

## Test new parser errors
Where we keep track of the offset. It's very weird and probably bad :(

## Intermediate types should use the bytestring builder type, not a bytestring
Saves on allocations if you're just gonna serialize. Specifically, the text
modules probably want this. Not sure how to do it exactly to retain maximum
soundness.

This might be a big change. I need to check aeson's design more thoroughly, it'd
look like the `Encoding` stuff.

2023-02-19: No, actually, I think this is a Text-unique thing, and they just
need some extra definitions.

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
