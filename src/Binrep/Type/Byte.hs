{- | Safe, if silly, byte representation for use at the type level.

'Word8' is a special type that GHC doesn't (and I think can't) promote to the
type level. We only have 'Natural's, which are unbounded. So we define a safe,
promotable representation, to allow us to prove well-sizedness at compile time.
Then we provide a bunch of type families and reifying typeclasses to enable
going between "similar" kinds ('Natural') and types ('Word8', 'B.ByteString')
respectively.

Type-level functionality is stored in 'Binrep.Type.Byte.TypeLevel' because the
definitions are even sillier than the ones here.

Do not use this on the term level. That would be _extremely_ silly.
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.Byte where

import Data.Word ( Word8 )
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as B.Builder
import Data.ByteString.Builder qualified as B ( Builder )

-- | A single byte, represented via a big ol' sum type.
--
-- Try adding two of these bad boys together! ...Actually, please don't write a
-- bounds-checked sum operator for this.
data Byte
  = B00 | B01 | B02 | B03 | B04 | B05 | B06 | B07
  | B08 | B09 | B0A | B0B | B0C | B0D | B0E | B0F
  | B10 | B11 | B12 | B13 | B14 | B15 | B16 | B17
  | B18 | B19 | B1A | B1B | B1C | B1D | B1E | B1F
  | B20 | B21 | B22 | B23 | B24 | B25 | B26 | B27
  | B28 | B29 | B2A | B2B | B2C | B2D | B2E | B2F
  | B30 | B31 | B32 | B33 | B34 | B35 | B36 | B37
  | B38 | B39 | B3A | B3B | B3C | B3D | B3E | B3F
  | B40 | B41 | B42 | B43 | B44 | B45 | B46 | B47
  | B48 | B49 | B4A | B4B | B4C | B4D | B4E | B4F
  | B50 | B51 | B52 | B53 | B54 | B55 | B56 | B57
  | B58 | B59 | B5A | B5B | B5C | B5D | B5E | B5F
  | B60 | B61 | B62 | B63 | B64 | B65 | B66 | B67
  | B68 | B69 | B6A | B6B | B6C | B6D | B6E | B6F
  | B70 | B71 | B72 | B73 | B74 | B75 | B76 | B77
  | B78 | B79 | B7A | B7B | B7C | B7D | B7E | B7F
  | B80 | B81 | B82 | B83 | B84 | B85 | B86 | B87
  | B88 | B89 | B8A | B8B | B8C | B8D | B8E | B8F
  | B90 | B91 | B92 | B93 | B94 | B95 | B96 | B97
  | B98 | B99 | B9A | B9B | B9C | B9D | B9E | B9F
  | BA0 | BA1 | BA2 | BA3 | BA4 | BA5 | BA6 | BA7
  | BA8 | BA9 | BAA | BAB | BAC | BAD | BAE | BAF
  | BB0 | BB1 | BB2 | BB3 | BB4 | BB5 | BB6 | BB7
  | BB8 | BB9 | BBA | BBB | BBC | BBD | BBE | BBF
  | BC0 | BC1 | BC2 | BC3 | BC4 | BC5 | BC6 | BC7
  | BC8 | BC9 | BCA | BCB | BCC | BCD | BCE | BCF
  | BD0 | BD1 | BD2 | BD3 | BD4 | BD5 | BD6 | BD7
  | BD8 | BD9 | BDA | BDB | BDC | BDD | BDE | BDF
  | BE0 | BE1 | BE2 | BE3 | BE4 | BE5 | BE6 | BE7
  | BE8 | BE9 | BEA | BEB | BEC | BED | BEE | BEF
  | BF0 | BF1 | BF2 | BF3 | BF4 | BF5 | BF6 | BF7
  | BF8 | BF9 | BFA | BFB | BFC | BFD | BFE | BFF
    -- no derive >:( no use term-level >:((

-- | Reify type-level 'Byte's to their value in a 'Word8'.
class ByteVal (b :: Byte) where
    -- | Reify the type-level 'Byte' to its corresponding value in a 'Word8'.
    byteVal :: Word8

instance ByteVal 'B00 where byteVal = 0x00
instance ByteVal 'B01 where byteVal = 0x01
instance ByteVal 'B02 where byteVal = 0x02
instance ByteVal 'B03 where byteVal = 0x03
instance ByteVal 'B04 where byteVal = 0x04
instance ByteVal 'B05 where byteVal = 0x05
instance ByteVal 'B06 where byteVal = 0x06
instance ByteVal 'B07 where byteVal = 0x07
instance ByteVal 'B08 where byteVal = 0x08
instance ByteVal 'B09 where byteVal = 0x09
instance ByteVal 'B0A where byteVal = 0x0A
instance ByteVal 'B0B where byteVal = 0x0B
instance ByteVal 'B0C where byteVal = 0x0C
instance ByteVal 'B0D where byteVal = 0x0D
instance ByteVal 'B0E where byteVal = 0x0E
instance ByteVal 'B0F where byteVal = 0x0F
instance ByteVal 'B10 where byteVal = 0x10
instance ByteVal 'B11 where byteVal = 0x11
instance ByteVal 'B12 where byteVal = 0x12
instance ByteVal 'B13 where byteVal = 0x13
instance ByteVal 'B14 where byteVal = 0x14
instance ByteVal 'B15 where byteVal = 0x15
instance ByteVal 'B16 where byteVal = 0x16
instance ByteVal 'B17 where byteVal = 0x17
instance ByteVal 'B18 where byteVal = 0x18
instance ByteVal 'B19 where byteVal = 0x19
instance ByteVal 'B1A where byteVal = 0x1A
instance ByteVal 'B1B where byteVal = 0x1B
instance ByteVal 'B1C where byteVal = 0x1C
instance ByteVal 'B1D where byteVal = 0x1D
instance ByteVal 'B1E where byteVal = 0x1E
instance ByteVal 'B1F where byteVal = 0x1F
instance ByteVal 'B20 where byteVal = 0x20
instance ByteVal 'B21 where byteVal = 0x21
instance ByteVal 'B22 where byteVal = 0x22
instance ByteVal 'B23 where byteVal = 0x23
instance ByteVal 'B24 where byteVal = 0x24
instance ByteVal 'B25 where byteVal = 0x25
instance ByteVal 'B26 where byteVal = 0x26
instance ByteVal 'B27 where byteVal = 0x27
instance ByteVal 'B28 where byteVal = 0x28
instance ByteVal 'B29 where byteVal = 0x29
instance ByteVal 'B2A where byteVal = 0x2A
instance ByteVal 'B2B where byteVal = 0x2B
instance ByteVal 'B2C where byteVal = 0x2C
instance ByteVal 'B2D where byteVal = 0x2D
instance ByteVal 'B2E where byteVal = 0x2E
instance ByteVal 'B2F where byteVal = 0x2F
instance ByteVal 'B30 where byteVal = 0x30
instance ByteVal 'B31 where byteVal = 0x31
instance ByteVal 'B32 where byteVal = 0x32
instance ByteVal 'B33 where byteVal = 0x33
instance ByteVal 'B34 where byteVal = 0x34
instance ByteVal 'B35 where byteVal = 0x35
instance ByteVal 'B36 where byteVal = 0x36
instance ByteVal 'B37 where byteVal = 0x37
instance ByteVal 'B38 where byteVal = 0x38
instance ByteVal 'B39 where byteVal = 0x39
instance ByteVal 'B3A where byteVal = 0x3A
instance ByteVal 'B3B where byteVal = 0x3B
instance ByteVal 'B3C where byteVal = 0x3C
instance ByteVal 'B3D where byteVal = 0x3D
instance ByteVal 'B3E where byteVal = 0x3E
instance ByteVal 'B3F where byteVal = 0x3F
instance ByteVal 'B40 where byteVal = 0x40
instance ByteVal 'B41 where byteVal = 0x41
instance ByteVal 'B42 where byteVal = 0x42
instance ByteVal 'B43 where byteVal = 0x43
instance ByteVal 'B44 where byteVal = 0x44
instance ByteVal 'B45 where byteVal = 0x45
instance ByteVal 'B46 where byteVal = 0x46
instance ByteVal 'B47 where byteVal = 0x47
instance ByteVal 'B48 where byteVal = 0x48
instance ByteVal 'B49 where byteVal = 0x49
instance ByteVal 'B4A where byteVal = 0x4A
instance ByteVal 'B4B where byteVal = 0x4B
instance ByteVal 'B4C where byteVal = 0x4C
instance ByteVal 'B4D where byteVal = 0x4D
instance ByteVal 'B4E where byteVal = 0x4E
instance ByteVal 'B4F where byteVal = 0x4F
instance ByteVal 'B50 where byteVal = 0x50
instance ByteVal 'B51 where byteVal = 0x51
instance ByteVal 'B52 where byteVal = 0x52
instance ByteVal 'B53 where byteVal = 0x53
instance ByteVal 'B54 where byteVal = 0x54
instance ByteVal 'B55 where byteVal = 0x55
instance ByteVal 'B56 where byteVal = 0x56
instance ByteVal 'B57 where byteVal = 0x57
instance ByteVal 'B58 where byteVal = 0x58
instance ByteVal 'B59 where byteVal = 0x59
instance ByteVal 'B5A where byteVal = 0x5A
instance ByteVal 'B5B where byteVal = 0x5B
instance ByteVal 'B5C where byteVal = 0x5C
instance ByteVal 'B5D where byteVal = 0x5D
instance ByteVal 'B5E where byteVal = 0x5E
instance ByteVal 'B5F where byteVal = 0x5F
instance ByteVal 'B60 where byteVal = 0x60
instance ByteVal 'B61 where byteVal = 0x61
instance ByteVal 'B62 where byteVal = 0x62
instance ByteVal 'B63 where byteVal = 0x63
instance ByteVal 'B64 where byteVal = 0x64
instance ByteVal 'B65 where byteVal = 0x65
instance ByteVal 'B66 where byteVal = 0x66
instance ByteVal 'B67 where byteVal = 0x67
instance ByteVal 'B68 where byteVal = 0x68
instance ByteVal 'B69 where byteVal = 0x69
instance ByteVal 'B6A where byteVal = 0x6A
instance ByteVal 'B6B where byteVal = 0x6B
instance ByteVal 'B6C where byteVal = 0x6C
instance ByteVal 'B6D where byteVal = 0x6D
instance ByteVal 'B6E where byteVal = 0x6E
instance ByteVal 'B6F where byteVal = 0x6F
instance ByteVal 'B70 where byteVal = 0x70
instance ByteVal 'B71 where byteVal = 0x71
instance ByteVal 'B72 where byteVal = 0x72
instance ByteVal 'B73 where byteVal = 0x73
instance ByteVal 'B74 where byteVal = 0x74
instance ByteVal 'B75 where byteVal = 0x75
instance ByteVal 'B76 where byteVal = 0x76
instance ByteVal 'B77 where byteVal = 0x77
instance ByteVal 'B78 where byteVal = 0x78
instance ByteVal 'B79 where byteVal = 0x79
instance ByteVal 'B7A where byteVal = 0x7A
instance ByteVal 'B7B where byteVal = 0x7B
instance ByteVal 'B7C where byteVal = 0x7C
instance ByteVal 'B7D where byteVal = 0x7D
instance ByteVal 'B7E where byteVal = 0x7E
instance ByteVal 'B7F where byteVal = 0x7F
instance ByteVal 'B80 where byteVal = 0x80
instance ByteVal 'B81 where byteVal = 0x81
instance ByteVal 'B82 where byteVal = 0x82
instance ByteVal 'B83 where byteVal = 0x83
instance ByteVal 'B84 where byteVal = 0x84
instance ByteVal 'B85 where byteVal = 0x85
instance ByteVal 'B86 where byteVal = 0x86
instance ByteVal 'B87 where byteVal = 0x87
instance ByteVal 'B88 where byteVal = 0x88
instance ByteVal 'B89 where byteVal = 0x89
instance ByteVal 'B8A where byteVal = 0x8A
instance ByteVal 'B8B where byteVal = 0x8B
instance ByteVal 'B8C where byteVal = 0x8C
instance ByteVal 'B8D where byteVal = 0x8D
instance ByteVal 'B8E where byteVal = 0x8E
instance ByteVal 'B8F where byteVal = 0x8F
instance ByteVal 'B90 where byteVal = 0x90
instance ByteVal 'B91 where byteVal = 0x91
instance ByteVal 'B92 where byteVal = 0x92
instance ByteVal 'B93 where byteVal = 0x93
instance ByteVal 'B94 where byteVal = 0x94
instance ByteVal 'B95 where byteVal = 0x95
instance ByteVal 'B96 where byteVal = 0x96
instance ByteVal 'B97 where byteVal = 0x97
instance ByteVal 'B98 where byteVal = 0x98
instance ByteVal 'B99 where byteVal = 0x99
instance ByteVal 'B9A where byteVal = 0x9A
instance ByteVal 'B9B where byteVal = 0x9B
instance ByteVal 'B9C where byteVal = 0x9C
instance ByteVal 'B9D where byteVal = 0x9D
instance ByteVal 'B9E where byteVal = 0x9E
instance ByteVal 'B9F where byteVal = 0x9F
instance ByteVal 'BA0 where byteVal = 0xA0
instance ByteVal 'BA1 where byteVal = 0xA1
instance ByteVal 'BA2 where byteVal = 0xA2
instance ByteVal 'BA3 where byteVal = 0xA3
instance ByteVal 'BA4 where byteVal = 0xA4
instance ByteVal 'BA5 where byteVal = 0xA5
instance ByteVal 'BA6 where byteVal = 0xA6
instance ByteVal 'BA7 where byteVal = 0xA7
instance ByteVal 'BA8 where byteVal = 0xA8
instance ByteVal 'BA9 where byteVal = 0xA9
instance ByteVal 'BAA where byteVal = 0xAA
instance ByteVal 'BAB where byteVal = 0xAB
instance ByteVal 'BAC where byteVal = 0xAC
instance ByteVal 'BAD where byteVal = 0xAD
instance ByteVal 'BAE where byteVal = 0xAE
instance ByteVal 'BAF where byteVal = 0xAF
instance ByteVal 'BB0 where byteVal = 0xB0
instance ByteVal 'BB1 where byteVal = 0xB1
instance ByteVal 'BB2 where byteVal = 0xB2
instance ByteVal 'BB3 where byteVal = 0xB3
instance ByteVal 'BB4 where byteVal = 0xB4
instance ByteVal 'BB5 where byteVal = 0xB5
instance ByteVal 'BB6 where byteVal = 0xB6
instance ByteVal 'BB7 where byteVal = 0xB7
instance ByteVal 'BB8 where byteVal = 0xB8
instance ByteVal 'BB9 where byteVal = 0xB9
instance ByteVal 'BBA where byteVal = 0xBA
instance ByteVal 'BBB where byteVal = 0xBB
instance ByteVal 'BBC where byteVal = 0xBC
instance ByteVal 'BBD where byteVal = 0xBD
instance ByteVal 'BBE where byteVal = 0xBE
instance ByteVal 'BBF where byteVal = 0xBF
instance ByteVal 'BC0 where byteVal = 0xC0
instance ByteVal 'BC1 where byteVal = 0xC1
instance ByteVal 'BC2 where byteVal = 0xC2
instance ByteVal 'BC3 where byteVal = 0xC3
instance ByteVal 'BC4 where byteVal = 0xC4
instance ByteVal 'BC5 where byteVal = 0xC5
instance ByteVal 'BC6 where byteVal = 0xC6
instance ByteVal 'BC7 where byteVal = 0xC7
instance ByteVal 'BC8 where byteVal = 0xC8
instance ByteVal 'BC9 where byteVal = 0xC9
instance ByteVal 'BCA where byteVal = 0xCA
instance ByteVal 'BCB where byteVal = 0xCB
instance ByteVal 'BCC where byteVal = 0xCC
instance ByteVal 'BCD where byteVal = 0xCD
instance ByteVal 'BCE where byteVal = 0xCE
instance ByteVal 'BCF where byteVal = 0xCF
instance ByteVal 'BD0 where byteVal = 0xD0
instance ByteVal 'BD1 where byteVal = 0xD1
instance ByteVal 'BD2 where byteVal = 0xD2
instance ByteVal 'BD3 where byteVal = 0xD3
instance ByteVal 'BD4 where byteVal = 0xD4
instance ByteVal 'BD5 where byteVal = 0xD5
instance ByteVal 'BD6 where byteVal = 0xD6
instance ByteVal 'BD7 where byteVal = 0xD7
instance ByteVal 'BD8 where byteVal = 0xD8
instance ByteVal 'BD9 where byteVal = 0xD9
instance ByteVal 'BDA where byteVal = 0xDA
instance ByteVal 'BDB where byteVal = 0xDB
instance ByteVal 'BDC where byteVal = 0xDC
instance ByteVal 'BDD where byteVal = 0xDD
instance ByteVal 'BDE where byteVal = 0xDE
instance ByteVal 'BDF where byteVal = 0xDF
instance ByteVal 'BE0 where byteVal = 0xE0
instance ByteVal 'BE1 where byteVal = 0xE1
instance ByteVal 'BE2 where byteVal = 0xE2
instance ByteVal 'BE3 where byteVal = 0xE3
instance ByteVal 'BE4 where byteVal = 0xE4
instance ByteVal 'BE5 where byteVal = 0xE5
instance ByteVal 'BE6 where byteVal = 0xE6
instance ByteVal 'BE7 where byteVal = 0xE7
instance ByteVal 'BE8 where byteVal = 0xE8
instance ByteVal 'BE9 where byteVal = 0xE9
instance ByteVal 'BEA where byteVal = 0xEA
instance ByteVal 'BEB where byteVal = 0xEB
instance ByteVal 'BEC where byteVal = 0xEC
instance ByteVal 'BED where byteVal = 0xED
instance ByteVal 'BEE where byteVal = 0xEE
instance ByteVal 'BEF where byteVal = 0xEF
instance ByteVal 'BF0 where byteVal = 0xF0
instance ByteVal 'BF1 where byteVal = 0xF1
instance ByteVal 'BF2 where byteVal = 0xF2
instance ByteVal 'BF3 where byteVal = 0xF3
instance ByteVal 'BF4 where byteVal = 0xF4
instance ByteVal 'BF5 where byteVal = 0xF5
instance ByteVal 'BF6 where byteVal = 0xF6
instance ByteVal 'BF7 where byteVal = 0xF7
instance ByteVal 'BF8 where byteVal = 0xF8
instance ByteVal 'BF9 where byteVal = 0xF9
instance ByteVal 'BFA where byteVal = 0xFA
instance ByteVal 'BFB where byteVal = 0xFB
instance ByteVal 'BFC where byteVal = 0xFC
instance ByteVal 'BFD where byteVal = 0xFD
instance ByteVal 'BFE where byteVal = 0xFE
instance ByteVal 'BFF where byteVal = 0xFF

-- | Reify a list of type-level 'Byte's to a 'B.Builder'.
--
-- This gives us not-awful efficiency. We could be much more efficient e.g.
-- reify list length, malloc a @byte*@ with it, fill one by one. But that's
-- really hard to define. what you pay for with type safety :(
class ByteVals (bs :: [Byte]) where byteVals' :: B.Builder
instance ByteVals '[] where byteVals' = mempty
instance (ByteVal b, ByteVals bs) => ByteVals (b ': bs) where
    byteVals' = B.Builder.word8 (byteVal @b) <> byteVals' @bs

byteVals :: forall bs. ByteVals bs => B.ByteString
byteVals = B.toStrict $ B.Builder.toLazyByteString $ byteVals' @bs
