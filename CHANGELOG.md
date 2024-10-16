## 1.1.0 (unreleased)
* fix ByteOrdered orphan strongweak instances to follow strongweak pattern --
  for original behaviour, use `WeakenN 2 (ByteOrdered end a)`
* update rerefined, strongweak

## 1.0.0 (2024-10-03)
* fix `NullTerminate` check being inverted (OOPS LOL)
* fix `Get [a]` instance (list backwards xd)
* add type-level constructor parsing for generics (!!)
* rewrite `AsciiNat`
* switch from refined to rerefined (my refined rewrite)
* add missing `BLen (GenericallyNonSum a)` instance
* other various cleanup

## 0.8.0 (2024-04-13)
* add missing `And` predicate combinators instances (`PutC`, `GetC`)
* add `Type.Derived.NullTermPadded` (type synonym over `And`)
* add `Generically` instances for `PutC` and `GetC`, where only non-sums are
  permitted
* add `GenericallyNonSum` newtype wrapper
* `Magic (a :: Symbol)` now supports UTF-8 symbols instead of just ASCII. All
  work is still done on the type-level.

## 0.7.0 (2024-04-10)
* provide "C struct" parser (from bytezap)
* fill out some missing C struct instances
* speed up magic parsing (sped up serializing in v0.6.0)
* add special binrep instances for `And` predicate combinator which re-associate
  to wrap the left predicate in the right
  * this gives a clean solution to the null-padded null-terminated bytestring,
    and appears to be generally sound! felt great to discover
* add Generically instances for C struct parser/serializers
  * can't for regular parser/serializer because of sum/non-sum choice

## 0.6.0 (2024-04-05)
* many updates to parsing/serializing internals, including generics
* provide "C struct" serializer

## 0.5.0 (2023-08-17)
  * support GHC 9.2 - 9.6
  * extract generic serializing & parsing into separate library. yes, I wrote
    generic generics. what are you going to do about it
  * allow using different libraries for parsing and serializing (since I can't
    decide)
  * count-prefixed types use `Refined1`, currently in my refined fork
  * refactor `Binrep.Type.Text`: users can now add extend to add their own
    encodings

## 0.3.1 (2022-08-28)
  * fix `Get [a]` instance

## 0.3.0 (2022-08-27)
  * useful parsing errors in `Get`
    * e.g. if parsing fails at "any Word64", emits "ran out, needed 8 bytes"
    * generic deriver places tons of data type info in highly structured errors
  * move `CBLen` into `BLen` as an associated type family
  * clean up magics (another open type family to associated)
  * add initial varint definitions at `Binrep.Type.Varint`

## 0.2.0 (2022-07-07)
Multiple rewrites (unable to push to Hackage for a while due to dependencies).

  * BinaryCodec split into Get and Put
  * fast serializing via fumieval's mason
  * fast parsing via András Kovács' flatparse
  * integration with my strongweak library
  * generics
  * tests
  * CBLen for constant length types
  * plenty more

## 0.1.0 (2022-04-22)
Initial release.

  * extracted from gtvm-hs
