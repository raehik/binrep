[gh-strongweak]: https://github.com/raehik/strongweak
[gh-flatparse]:  https://github.com/AndrasKovacs/flatparse
[gh-mason]:      https://github.com/fumieval/mason
[gh-refined]:    https://github.com/nikita-volkov/refined

# binrep
binrep is a library for **precisely modelling binary schemas** and working with
them effectively and efficiently in Haskell. Here's why it's useful:

  * **Explicit:** Binary representation primitives such as C-style bytestrings
    (null-terminated), sized explicit-endian machine integers, and null-padded
    data enable defining Haskell data types with the binary schema "baked in".
  * **Low boilerplate:** Generic parsers and serializers further reduce boilerplate for
    straightforward schemas. (See [Generic binary
    representation](#generic-binary-representation) for details.)
  * **Easy validation:** Goes hand in hand with my [strongweak][gh-strongweak]
    library to allow working with unwrapped data internally, and enforcing all
    the binary representation invariants before serializing - no extra
    definitions required.
  * **Performant:** Parsing and serialization is low-level and *extremely fast*,
    using [flatparse][gh-flatparse] and [mason][gh-mason] respectively.

## Philosophy
### Modelling, not serializing
binrep is good at modelling binary data formats. It is not a plain
"serialization" library, where the actual binary representation is hidden from
the user (intentionally, with good reason). The binary and cereal libraries are
great choices for that. They are interested in defining efficient binary codecs
for Haskell data. However, their codec typeclasses *hide representation
decisions* from the user. In cereal,

  * machine integers are encoded with
    [big endian](https://hackage.haskell.org/package/cereal-0.5.8.2/docs/src/Data.Serialize.html#line-182)
  * bytestrings are written with an
    [8-byte length prefix](https://hackage.haskell.org/package/cereal-0.5.8.2/docs/src/Data.Serialize.html#line-498)

These are fine decisions. But they aren't accurate to the types. Endianness is
an implementation decision.

binrep refuses to work with a machine integer unless it knows the endianness.
Bytestrings are split into C-style (null-terminated) and Pascal-style
(length-prefixed). This enforces careful consideration for the binary data being
modelled.

### Validation without boilerplate
A C-style bytestring must not contain any `0x00` null bytes. A Pascal-style
bytestring must be short enough to be able to encode its length in the length
prefix machine integer. But checking such invariants is tedious work. Am I
really going to wrap everything in a bunch of newtypes and force users to call a
bunch of checker functions every time?

Yes and no. Yes, binrep uses newtypes extensively, though most are type synonyms
over the `Refined` newtype from Nikita Volkov's wonderful [refined][gh-refined]
library. No, binrep doesn't want you to wrangle with these day-to-day. One
solution is to define a simplified "weak" type, and convert between it and the
binary-safe "strong" type. My [strongweak][gh-strongweak] library provides
supporting definitions for this pattern, and generic derivers which will work
with binrep's binary representation primitives.

### Performant primitives
Parsing uses András Kovács' [flatparse][gh-flatparse] library. Serializing is
via Fumiaki Kinoshita's [mason][gh-mason] library. These are about as fast as
you can get in 2022.

We only define serializers for validated types, meaning we can potentially skip
safety checks, that other serializers would do. Except we still do them, but
validation is an explicitly required step before serialization.

*This might change if we start to support weirder binary representations,
specifically offset-based data.*

## Generic binary representation
binrep's generic deriving makes very few decisions:

  * Constructors are encoded by sequentially encoding every enclosed field.
    * Empty constructors thus serialize to 0 bytes.
  * Sum types are encoded via a tag obtained from the constructor names.
    * It's the same approach as aeson, with a bit more flexibility: see below.

Sum types (data types with multiple constructors) are handled by first encoding
a "tag field", the value of which then indicates which constructor to use. You
must provide a function to convert from a constructor name to a (unique) tag.
You could encode them as a null-terminated ASCII bytestring (this is the
default), or as a single byte. To ease this, you may consider putting the tag
value in constructor names:

```haskell
data BinarySumType = B1 | B2

getConstructorTag :: String -> Word8
getConstructorTag = read . drop 1

-- >>> getConstructorTag "B1"
-- 1

-- Or use our generic helper, which takes hex values:
--
-- >>> cSumTagHex @Word8 (drop . 1) "BFF"
-- 255
```

## Similar projects
### Kaitai Struct
[Kaitai Struct](https://kaitai.io/) is a wonderful declarative parser generator
project. They bolt an expression language and a whole lot of binary cleverness
on top of a nice YAML schema. It comes with an IDE, a visualizer, and you can
compile schemas down to parsers for various different languages (no Haskell...).

Design principles like their fancy absolute offset handling and language
neutrality have stunted serialization support. Though it's more like they have
such powerful parsing that they can parse formats that can't be edited and
re-serialized naively, like archives with file indexes. For proper handling, one
should store a file table, and serialization generates the index. So in reverse,
you would want to combine them. But it's a bit program-y. In binrep, you are in
a programming language, so it's less of a problem... but I'm not sure if we can
be very efficient at absolute offset stuff.

Realistically, Kaitai Struct is the best decision for fast iteration on
reversing unknown data. binrep is useful for loading data straight into Haskell
for further processing, especially converting between simpler formats.

### Wuffs
[Wuffs](https://github.com/google/wuffs) is a crazy exploration into safe
low-level code via strong typing. You have to annotate every possibly dangerous
statement with a proof of safety. It's a tedious, explicit, very safe and very
fast imperative language for defining parsers and serializers.

Wuffs is more a codec engineer's tool than a reverse engineer's one. binrep
isn't really interested in speed, and being a Haskell library we get to focus on
defining types and their composition in a declarative & functional manner. As
such, we get to define more useful things quicker using binrep. Though we share
many core ideas, such as refinement types.

Check out Wuffs if you need to write a bunch of codecs and they really, really
need to be both fast and safe. The trade-off is, of course, your time.
