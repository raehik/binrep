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
