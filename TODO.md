# binrep to-dos
## Clarify `Thin` stuff
I use the `ByteString` instance internally sometimes (probably), and in those
cases I want "thin" parsing since I'm consuming it right there and then. I need
to clarify the precise setup here.

## `And` ordering
```haskell
newtype Compose f g a = Compose { getCompose :: f (g a)}
```

Should I flip `And`? I guess it depends how one thinks of applying two
predicates to a type. Is it `And l r` (l -> r), or `And f g` (=`f(g)`)?

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

## Support "known maximum length" serialization
For some types, we don't know how long their binary representation will be
before serializing-- but we _do_ know their maximum possible length. The prime
example here is serializing floats into their ASCII representation. For these,
we have no choice but to deny a binrep instance, instead requesting that the
user first transform to a simpler type (here, `Double -> ByteString`).
That's sensible for types where their serialized form can vary greatly
in length, but floats are small and, assuming random input, tend to hit close to
the maximum possible length. So it feels like a bit of a waste.

We could have a type-level switch that tracks whether we've ever encountered a
max-length type. If we have, you have to use the `createUpTo` family. If not,
you can use the `create` family (where we know precisely the size).

I note that `createUpTo`, uh, isn't really any less efficient than `create`.
It's essentially identical. So as long as your low-level builder can easily
determine so-far serialized length, which bytezap can, it's free!
(Unsure if mason can, but I bet so.)

This is highly relevant to the question "can we use binrep/bytezap to
efficiently serialize JSON?". Numbers are one thing, the other is text, which
requires escaping. If we could figure out a fast size counter, we probably could
do it. Perhaps we could figure out a fast, semi-accurate size counter, which
only figures out the maximum (but does it faster).

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
