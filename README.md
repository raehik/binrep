# binrep
tl;dr aeson for binary

binrep is a library for defining **precise binary representations** of Haskell
data types. We define a set of binary representation primitives such as
endianness, machine integers and null-padding, then provide generic derivers for
efficiently (maybe) parsing & serializing types made out of these primitives.

See [Generic binary representation](#generic-binary-representation) for details
on what decisions the generic deriver makes.

## Representation, not serialization
The binary and cereal libraries are **binary serialization** libraries.
They are interested in defining efficient binary en/decoders for Haskell data.
cereal in particular is wonderful, and forms the basis of this library. However,
their typeclasses *hide representation decisions* from the user:

  * machine integers are encoded with
    [big endian](https://hackage.haskell.org/package/cereal-0.5.8.2/docs/src/Data.Serialize.html#line-182)
  * bytestrings are written with an
    [8-byte length prefix](https://hackage.haskell.org/package/cereal-0.5.8.2/docs/src/Data.Serialize.html#line-498)

These are all fine decisions. But they restrict the typeclasses to working with
other cereal users. binrep is mainly interested in *precision* and *safety*.

### Precision
Rather than bytestrings, it prefers to talk about C-style (null-terminated)
bytestrings and Pascal-style (length-prefixed) bytestrings. It doesn't like to
talk about machine integers at all, unless you have an explicit endianness on
the table.

These force verbosity and proper consideration for the target format. And by
shifting such decisions to the type level, we can write code that flips between
different representations with type safety guarantees: for example, a type
parametrized by its endianness.

### Safety
A C-style bytestring must not contain any `0x00` null bytes. A Pascal-style
bytestring must be short enough to be able to encode its length in the length
prefix machine integer. As we all know, checking such invariants is tedious. Are
you really going to wrap everything in a bunch of newtypes and force users to
call a bunch of checker functions?

Yes and no. binrep builds on top of Nikita Volkov's wonderful
[refined](https://hackage.haskell.org/package/refined) library, which lets us
write such predicated types via type synonyms rather than newtype overload. You
*can't get* a C-style bytestring without testing the above predicate.

binrep types still necessitate a lot of wrapping. And if you want to transform a
large type with lots of refined types inside, you either have to do lots of
extremely tedious work, or convert between a "safe" and "unsafe" format. That's
out of binrep's hands, but check out
[refined-extra](https://github.com/raehik/refined-extra), which provides generic
derivers for refining and unrefining a given data type. (You'll need my
[refined](https://github.com/raehik/refined) fork too.)

### OK performance
We use cereal for parsing (probably quite slow) and mason for serializing
(blazingly fast). We only define serializers for validated types, meaning we can
skip safety checks - though they are of course still done, just before
serialization.

*This might change if we start to support weirder binary representations,
specifically offset-based data.*

### Similar projects
#### Kaitai Struct
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

#### Wuffs
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

## Generic binary representation
binrep's generic deriving makes very few decisions:

  * Constructors are encoded by sequentially encoding every enclosed field.
    * Thus, empty constructors serialize to 0 bytes.
  * Sum types are encoded via a tag obtained from the constructor names.
    * It's the same approach as aeson, with a bit more flexibility: see below.

Due to sum type handling (and so that classes can stay independent from their
generic derivers), you must provide a deriver config.

### Generic sum type encoding
Data types with multiple constructors are handled by first encoding a "tag
field", the value of which then indicates which constructor to use. Here's the
trick -- you provide the type to use for the tag. You'll probably want to stick
with machine integers, but you may choose the size and endianness (well, you
have to).

You must also provide a function to convert from constructor strings to your
tag. We encourage the aeson approach of encoding tags in constructor names:

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

This function must map valid each valid constructor name to a unique tag value.
If it doesn't, you should hopefully get cool and fun errors.
