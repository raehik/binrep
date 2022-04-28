# binrep
tl;dr aeson for binary

binrep defines a set of binary representation primitives such as endianness,
machine integers and null-padding via various `newtype` wrappers, and provides
generic derivers for parsing & serializing types made out of these primitives.

## Why not binary or cereal?
The binary and cereal libraries are **binary serialization** libraries.
They are interested in defining efficient binary en/decoders for Haskell data.
However, their typeclasses make term-level decisions about representation. For
example, machine integers are encoded with big endian.

binrep encourages you to describe the **binary representation** directly in
types, and has its own typeclasses with instances based around that. Thus,
binrep will not encode a `Word64` unless you either provide the endianness to
use at runtime, or encode its endianness via some `newtype` wrapper.

## Generic binary representation
binrep's generic deriving makes very few decisions:

  * Empty constructors are empty - 0 bytes.
  * Fields are encoded sequentially in the order they appear in the data type.
  * Sum types are special, similar to aeson: see below.

### Generic sum type encoding
Sum types (data types with multiple constructors) are handled by first encoding
a "tag field", the value of which then indicates which constructor to use.
Here's the trick -- you provide the type to use for the tag. You'll probably
want to stick with machine integers, but you may choose the size and endianness
(well, you have to).

You must also provide a function to convert from constructor strings to your
tag. We encourage the aeson approach of encoding tags in constructor names:

```haskell
data BinarySumType = B1 | B2

getConstructorTag :: String -> Word8
getConstructorTag = read . drop 1

-- >>> getConstructorTag "B1"
-- 1
```
