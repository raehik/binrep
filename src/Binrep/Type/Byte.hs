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

{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}

module Binrep.Type.Byte where

import Bytezap qualified as BZ
import Bytezap.Poke.Int qualified as BZ
import GHC.TypeNats
import GHC.Word

-- | Efficiently reify a list of type-level 'Natural' bytes to to a bytestring
--   builder.
--
-- Attempting to reify a 'Natural' larger than 255 results in a type error.
--
-- This is about as far as one should go for pointless performance here, I
-- should think.
class ReifyBytes (ns :: [Natural]) where reifyBytes :: BZ.Poke
instance ReifyBytes '[] where
    {-# INLINE reifyBytes #-}
    reifyBytes = mempty
instance (ByteVal n, ReifyBytes ns) => ReifyBytes (n ': ns) where
    {-# INLINE reifyBytes #-}
    reifyBytes = BZ.w8 (byteVal @n) <> reifyBytes @ns

class ByteVal (n :: Natural) where byteVal :: Word8

instance ByteVal 0x00 where
    {-# INLINE byteVal #-}
    byteVal = 0x00
instance ByteVal 0x01 where
    {-# INLINE byteVal #-}
    byteVal = 0x01
instance ByteVal 0x02 where
    {-# INLINE byteVal #-}
    byteVal = 0x02
instance ByteVal 0x03 where
    {-# INLINE byteVal #-}
    byteVal = 0x03
instance ByteVal 0x04 where
    {-# INLINE byteVal #-}
    byteVal = 0x04
instance ByteVal 0x05 where
    {-# INLINE byteVal #-}
    byteVal = 0x05
instance ByteVal 0x06 where
    {-# INLINE byteVal #-}
    byteVal = 0x06
instance ByteVal 0x07 where
    {-# INLINE byteVal #-}
    byteVal = 0x07
instance ByteVal 0x08 where
    {-# INLINE byteVal #-}
    byteVal = 0x08
instance ByteVal 0x09 where
    {-# INLINE byteVal #-}
    byteVal = 0x09
instance ByteVal 0x0a where
    {-# INLINE byteVal #-}
    byteVal = 0x0a
instance ByteVal 0x0b where
    {-# INLINE byteVal #-}
    byteVal = 0x0b
instance ByteVal 0x0c where
    {-# INLINE byteVal #-}
    byteVal = 0x0c
instance ByteVal 0x0d where
    {-# INLINE byteVal #-}
    byteVal = 0x0d
instance ByteVal 0x0e where
    {-# INLINE byteVal #-}
    byteVal = 0x0e
instance ByteVal 0x0f where
    {-# INLINE byteVal #-}
    byteVal = 0x0f
instance ByteVal 0x10 where
    {-# INLINE byteVal #-}
    byteVal = 0x10
instance ByteVal 0x11 where
    {-# INLINE byteVal #-}
    byteVal = 0x11
instance ByteVal 0x12 where
    {-# INLINE byteVal #-}
    byteVal = 0x12
instance ByteVal 0x13 where
    {-# INLINE byteVal #-}
    byteVal = 0x13
instance ByteVal 0x14 where
    {-# INLINE byteVal #-}
    byteVal = 0x14
instance ByteVal 0x15 where
    {-# INLINE byteVal #-}
    byteVal = 0x15
instance ByteVal 0x16 where
    {-# INLINE byteVal #-}
    byteVal = 0x16
instance ByteVal 0x17 where
    {-# INLINE byteVal #-}
    byteVal = 0x17
instance ByteVal 0x18 where
    {-# INLINE byteVal #-}
    byteVal = 0x18
instance ByteVal 0x19 where
    {-# INLINE byteVal #-}
    byteVal = 0x19
instance ByteVal 0x1a where
    {-# INLINE byteVal #-}
    byteVal = 0x1a
instance ByteVal 0x1b where
    {-# INLINE byteVal #-}
    byteVal = 0x1b
instance ByteVal 0x1c where
    {-# INLINE byteVal #-}
    byteVal = 0x1c
instance ByteVal 0x1d where
    {-# INLINE byteVal #-}
    byteVal = 0x1d
instance ByteVal 0x1e where
    {-# INLINE byteVal #-}
    byteVal = 0x1e
instance ByteVal 0x1f where
    {-# INLINE byteVal #-}
    byteVal = 0x1f
instance ByteVal 0x20 where
    {-# INLINE byteVal #-}
    byteVal = 0x20
instance ByteVal 0x21 where
    {-# INLINE byteVal #-}
    byteVal = 0x21
instance ByteVal 0x22 where
    {-# INLINE byteVal #-}
    byteVal = 0x22
instance ByteVal 0x23 where
    {-# INLINE byteVal #-}
    byteVal = 0x23
instance ByteVal 0x24 where
    {-# INLINE byteVal #-}
    byteVal = 0x24
instance ByteVal 0x25 where
    {-# INLINE byteVal #-}
    byteVal = 0x25
instance ByteVal 0x26 where
    {-# INLINE byteVal #-}
    byteVal = 0x26
instance ByteVal 0x27 where
    {-# INLINE byteVal #-}
    byteVal = 0x27
instance ByteVal 0x28 where
    {-# INLINE byteVal #-}
    byteVal = 0x28
instance ByteVal 0x29 where
    {-# INLINE byteVal #-}
    byteVal = 0x29
instance ByteVal 0x2a where
    {-# INLINE byteVal #-}
    byteVal = 0x2a
instance ByteVal 0x2b where
    {-# INLINE byteVal #-}
    byteVal = 0x2b
instance ByteVal 0x2c where
    {-# INLINE byteVal #-}
    byteVal = 0x2c
instance ByteVal 0x2d where
    {-# INLINE byteVal #-}
    byteVal = 0x2d
instance ByteVal 0x2e where
    {-# INLINE byteVal #-}
    byteVal = 0x2e
instance ByteVal 0x2f where
    {-# INLINE byteVal #-}
    byteVal = 0x2f
instance ByteVal 0x30 where
    {-# INLINE byteVal #-}
    byteVal = 0x30
instance ByteVal 0x31 where
    {-# INLINE byteVal #-}
    byteVal = 0x31
instance ByteVal 0x32 where
    {-# INLINE byteVal #-}
    byteVal = 0x32
instance ByteVal 0x33 where
    {-# INLINE byteVal #-}
    byteVal = 0x33
instance ByteVal 0x34 where
    {-# INLINE byteVal #-}
    byteVal = 0x34
instance ByteVal 0x35 where
    {-# INLINE byteVal #-}
    byteVal = 0x35
instance ByteVal 0x36 where
    {-# INLINE byteVal #-}
    byteVal = 0x36
instance ByteVal 0x37 where
    {-# INLINE byteVal #-}
    byteVal = 0x37
instance ByteVal 0x38 where
    {-# INLINE byteVal #-}
    byteVal = 0x38
instance ByteVal 0x39 where
    {-# INLINE byteVal #-}
    byteVal = 0x39
instance ByteVal 0x3a where
    {-# INLINE byteVal #-}
    byteVal = 0x3a
instance ByteVal 0x3b where
    {-# INLINE byteVal #-}
    byteVal = 0x3b
instance ByteVal 0x3c where
    {-# INLINE byteVal #-}
    byteVal = 0x3c
instance ByteVal 0x3d where
    {-# INLINE byteVal #-}
    byteVal = 0x3d
instance ByteVal 0x3e where
    {-# INLINE byteVal #-}
    byteVal = 0x3e
instance ByteVal 0x3f where
    {-# INLINE byteVal #-}
    byteVal = 0x3f
instance ByteVal 0x40 where
    {-# INLINE byteVal #-}
    byteVal = 0x40
instance ByteVal 0x41 where
    {-# INLINE byteVal #-}
    byteVal = 0x41
instance ByteVal 0x42 where
    {-# INLINE byteVal #-}
    byteVal = 0x42
instance ByteVal 0x43 where
    {-# INLINE byteVal #-}
    byteVal = 0x43
instance ByteVal 0x44 where
    {-# INLINE byteVal #-}
    byteVal = 0x44
instance ByteVal 0x45 where
    {-# INLINE byteVal #-}
    byteVal = 0x45
instance ByteVal 0x46 where
    {-# INLINE byteVal #-}
    byteVal = 0x46
instance ByteVal 0x47 where
    {-# INLINE byteVal #-}
    byteVal = 0x47
instance ByteVal 0x48 where
    {-# INLINE byteVal #-}
    byteVal = 0x48
instance ByteVal 0x49 where
    {-# INLINE byteVal #-}
    byteVal = 0x49
instance ByteVal 0x4a where
    {-# INLINE byteVal #-}
    byteVal = 0x4a
instance ByteVal 0x4b where
    {-# INLINE byteVal #-}
    byteVal = 0x4b
instance ByteVal 0x4c where
    {-# INLINE byteVal #-}
    byteVal = 0x4c
instance ByteVal 0x4d where
    {-# INLINE byteVal #-}
    byteVal = 0x4d
instance ByteVal 0x4e where
    {-# INLINE byteVal #-}
    byteVal = 0x4e
instance ByteVal 0x4f where
    {-# INLINE byteVal #-}
    byteVal = 0x4f
instance ByteVal 0x50 where
    {-# INLINE byteVal #-}
    byteVal = 0x50
instance ByteVal 0x51 where
    {-# INLINE byteVal #-}
    byteVal = 0x51
instance ByteVal 0x52 where
    {-# INLINE byteVal #-}
    byteVal = 0x52
instance ByteVal 0x53 where
    {-# INLINE byteVal #-}
    byteVal = 0x53
instance ByteVal 0x54 where
    {-# INLINE byteVal #-}
    byteVal = 0x54
instance ByteVal 0x55 where
    {-# INLINE byteVal #-}
    byteVal = 0x55
instance ByteVal 0x56 where
    {-# INLINE byteVal #-}
    byteVal = 0x56
instance ByteVal 0x57 where
    {-# INLINE byteVal #-}
    byteVal = 0x57
instance ByteVal 0x58 where
    {-# INLINE byteVal #-}
    byteVal = 0x58
instance ByteVal 0x59 where
    {-# INLINE byteVal #-}
    byteVal = 0x59
instance ByteVal 0x5a where
    {-# INLINE byteVal #-}
    byteVal = 0x5a
instance ByteVal 0x5b where
    {-# INLINE byteVal #-}
    byteVal = 0x5b
instance ByteVal 0x5c where
    {-# INLINE byteVal #-}
    byteVal = 0x5c
instance ByteVal 0x5d where
    {-# INLINE byteVal #-}
    byteVal = 0x5d
instance ByteVal 0x5e where
    {-# INLINE byteVal #-}
    byteVal = 0x5e
instance ByteVal 0x5f where
    {-# INLINE byteVal #-}
    byteVal = 0x5f
instance ByteVal 0x60 where
    {-# INLINE byteVal #-}
    byteVal = 0x60
instance ByteVal 0x61 where
    {-# INLINE byteVal #-}
    byteVal = 0x61
instance ByteVal 0x62 where
    {-# INLINE byteVal #-}
    byteVal = 0x62
instance ByteVal 0x63 where
    {-# INLINE byteVal #-}
    byteVal = 0x63
instance ByteVal 0x64 where
    {-# INLINE byteVal #-}
    byteVal = 0x64
instance ByteVal 0x65 where
    {-# INLINE byteVal #-}
    byteVal = 0x65
instance ByteVal 0x66 where
    {-# INLINE byteVal #-}
    byteVal = 0x66
instance ByteVal 0x67 where
    {-# INLINE byteVal #-}
    byteVal = 0x67
instance ByteVal 0x68 where
    {-# INLINE byteVal #-}
    byteVal = 0x68
instance ByteVal 0x69 where
    {-# INLINE byteVal #-}
    byteVal = 0x69
instance ByteVal 0x6a where
    {-# INLINE byteVal #-}
    byteVal = 0x6a
instance ByteVal 0x6b where
    {-# INLINE byteVal #-}
    byteVal = 0x6b
instance ByteVal 0x6c where
    {-# INLINE byteVal #-}
    byteVal = 0x6c
instance ByteVal 0x6d where
    {-# INLINE byteVal #-}
    byteVal = 0x6d
instance ByteVal 0x6e where
    {-# INLINE byteVal #-}
    byteVal = 0x6e
instance ByteVal 0x6f where
    {-# INLINE byteVal #-}
    byteVal = 0x6f
instance ByteVal 0x70 where
    {-# INLINE byteVal #-}
    byteVal = 0x70
instance ByteVal 0x71 where
    {-# INLINE byteVal #-}
    byteVal = 0x71
instance ByteVal 0x72 where
    {-# INLINE byteVal #-}
    byteVal = 0x72
instance ByteVal 0x73 where
    {-# INLINE byteVal #-}
    byteVal = 0x73
instance ByteVal 0x74 where
    {-# INLINE byteVal #-}
    byteVal = 0x74
instance ByteVal 0x75 where
    {-# INLINE byteVal #-}
    byteVal = 0x75
instance ByteVal 0x76 where
    {-# INLINE byteVal #-}
    byteVal = 0x76
instance ByteVal 0x77 where
    {-# INLINE byteVal #-}
    byteVal = 0x77
instance ByteVal 0x78 where
    {-# INLINE byteVal #-}
    byteVal = 0x78
instance ByteVal 0x79 where
    {-# INLINE byteVal #-}
    byteVal = 0x79
instance ByteVal 0x7a where
    {-# INLINE byteVal #-}
    byteVal = 0x7a
instance ByteVal 0x7b where
    {-# INLINE byteVal #-}
    byteVal = 0x7b
instance ByteVal 0x7c where
    {-# INLINE byteVal #-}
    byteVal = 0x7c
instance ByteVal 0x7d where
    {-# INLINE byteVal #-}
    byteVal = 0x7d
instance ByteVal 0x7e where
    {-# INLINE byteVal #-}
    byteVal = 0x7e
instance ByteVal 0x7f where
    {-# INLINE byteVal #-}
    byteVal = 0x7f
instance ByteVal 0x80 where
    {-# INLINE byteVal #-}
    byteVal = 0x80
instance ByteVal 0x81 where
    {-# INLINE byteVal #-}
    byteVal = 0x81
instance ByteVal 0x82 where
    {-# INLINE byteVal #-}
    byteVal = 0x82
instance ByteVal 0x83 where
    {-# INLINE byteVal #-}
    byteVal = 0x83
instance ByteVal 0x84 where
    {-# INLINE byteVal #-}
    byteVal = 0x84
instance ByteVal 0x85 where
    {-# INLINE byteVal #-}
    byteVal = 0x85
instance ByteVal 0x86 where
    {-# INLINE byteVal #-}
    byteVal = 0x86
instance ByteVal 0x87 where
    {-# INLINE byteVal #-}
    byteVal = 0x87
instance ByteVal 0x88 where
    {-# INLINE byteVal #-}
    byteVal = 0x88
instance ByteVal 0x89 where
    {-# INLINE byteVal #-}
    byteVal = 0x89
instance ByteVal 0x8a where
    {-# INLINE byteVal #-}
    byteVal = 0x8a
instance ByteVal 0x8b where
    {-# INLINE byteVal #-}
    byteVal = 0x8b
instance ByteVal 0x8c where
    {-# INLINE byteVal #-}
    byteVal = 0x8c
instance ByteVal 0x8d where
    {-# INLINE byteVal #-}
    byteVal = 0x8d
instance ByteVal 0x8e where
    {-# INLINE byteVal #-}
    byteVal = 0x8e
instance ByteVal 0x8f where
    {-# INLINE byteVal #-}
    byteVal = 0x8f
instance ByteVal 0x90 where
    {-# INLINE byteVal #-}
    byteVal = 0x90
instance ByteVal 0x91 where
    {-# INLINE byteVal #-}
    byteVal = 0x91
instance ByteVal 0x92 where
    {-# INLINE byteVal #-}
    byteVal = 0x92
instance ByteVal 0x93 where
    {-# INLINE byteVal #-}
    byteVal = 0x93
instance ByteVal 0x94 where
    {-# INLINE byteVal #-}
    byteVal = 0x94
instance ByteVal 0x95 where
    {-# INLINE byteVal #-}
    byteVal = 0x95
instance ByteVal 0x96 where
    {-# INLINE byteVal #-}
    byteVal = 0x96
instance ByteVal 0x97 where
    {-# INLINE byteVal #-}
    byteVal = 0x97
instance ByteVal 0x98 where
    {-# INLINE byteVal #-}
    byteVal = 0x98
instance ByteVal 0x99 where
    {-# INLINE byteVal #-}
    byteVal = 0x99
instance ByteVal 0x9a where
    {-# INLINE byteVal #-}
    byteVal = 0x9a
instance ByteVal 0x9b where
    {-# INLINE byteVal #-}
    byteVal = 0x9b
instance ByteVal 0x9c where
    {-# INLINE byteVal #-}
    byteVal = 0x9c
instance ByteVal 0x9d where
    {-# INLINE byteVal #-}
    byteVal = 0x9d
instance ByteVal 0x9e where
    {-# INLINE byteVal #-}
    byteVal = 0x9e
instance ByteVal 0x9f where
    {-# INLINE byteVal #-}
    byteVal = 0x9f
instance ByteVal 0xa0 where
    {-# INLINE byteVal #-}
    byteVal = 0xa0
instance ByteVal 0xa1 where
    {-# INLINE byteVal #-}
    byteVal = 0xa1
instance ByteVal 0xa2 where
    {-# INLINE byteVal #-}
    byteVal = 0xa2
instance ByteVal 0xa3 where
    {-# INLINE byteVal #-}
    byteVal = 0xa3
instance ByteVal 0xa4 where
    {-# INLINE byteVal #-}
    byteVal = 0xa4
instance ByteVal 0xa5 where
    {-# INLINE byteVal #-}
    byteVal = 0xa5
instance ByteVal 0xa6 where
    {-# INLINE byteVal #-}
    byteVal = 0xa6
instance ByteVal 0xa7 where
    {-# INLINE byteVal #-}
    byteVal = 0xa7
instance ByteVal 0xa8 where
    {-# INLINE byteVal #-}
    byteVal = 0xa8
instance ByteVal 0xa9 where
    {-# INLINE byteVal #-}
    byteVal = 0xa9
instance ByteVal 0xaa where
    {-# INLINE byteVal #-}
    byteVal = 0xaa
instance ByteVal 0xab where
    {-# INLINE byteVal #-}
    byteVal = 0xab
instance ByteVal 0xac where
    {-# INLINE byteVal #-}
    byteVal = 0xac
instance ByteVal 0xad where
    {-# INLINE byteVal #-}
    byteVal = 0xad
instance ByteVal 0xae where
    {-# INLINE byteVal #-}
    byteVal = 0xae
instance ByteVal 0xaf where
    {-# INLINE byteVal #-}
    byteVal = 0xaf
instance ByteVal 0xb0 where
    {-# INLINE byteVal #-}
    byteVal = 0xb0
instance ByteVal 0xb1 where
    {-# INLINE byteVal #-}
    byteVal = 0xb1
instance ByteVal 0xb2 where
    {-# INLINE byteVal #-}
    byteVal = 0xb2
instance ByteVal 0xb3 where
    {-# INLINE byteVal #-}
    byteVal = 0xb3
instance ByteVal 0xb4 where
    {-# INLINE byteVal #-}
    byteVal = 0xb4
instance ByteVal 0xb5 where
    {-# INLINE byteVal #-}
    byteVal = 0xb5
instance ByteVal 0xb6 where
    {-# INLINE byteVal #-}
    byteVal = 0xb6
instance ByteVal 0xb7 where
    {-# INLINE byteVal #-}
    byteVal = 0xb7
instance ByteVal 0xb8 where
    {-# INLINE byteVal #-}
    byteVal = 0xb8
instance ByteVal 0xb9 where
    {-# INLINE byteVal #-}
    byteVal = 0xb9
instance ByteVal 0xba where
    {-# INLINE byteVal #-}
    byteVal = 0xba
instance ByteVal 0xbb where
    {-# INLINE byteVal #-}
    byteVal = 0xbb
instance ByteVal 0xbc where
    {-# INLINE byteVal #-}
    byteVal = 0xbc
instance ByteVal 0xbd where
    {-# INLINE byteVal #-}
    byteVal = 0xbd
instance ByteVal 0xbe where
    {-# INLINE byteVal #-}
    byteVal = 0xbe
instance ByteVal 0xbf where
    {-# INLINE byteVal #-}
    byteVal = 0xbf
instance ByteVal 0xc0 where
    {-# INLINE byteVal #-}
    byteVal = 0xc0
instance ByteVal 0xc1 where
    {-# INLINE byteVal #-}
    byteVal = 0xc1
instance ByteVal 0xc2 where
    {-# INLINE byteVal #-}
    byteVal = 0xc2
instance ByteVal 0xc3 where
    {-# INLINE byteVal #-}
    byteVal = 0xc3
instance ByteVal 0xc4 where
    {-# INLINE byteVal #-}
    byteVal = 0xc4
instance ByteVal 0xc5 where
    {-# INLINE byteVal #-}
    byteVal = 0xc5
instance ByteVal 0xc6 where
    {-# INLINE byteVal #-}
    byteVal = 0xc6
instance ByteVal 0xc7 where
    {-# INLINE byteVal #-}
    byteVal = 0xc7
instance ByteVal 0xc8 where
    {-# INLINE byteVal #-}
    byteVal = 0xc8
instance ByteVal 0xc9 where
    {-# INLINE byteVal #-}
    byteVal = 0xc9
instance ByteVal 0xca where
    {-# INLINE byteVal #-}
    byteVal = 0xca
instance ByteVal 0xcb where
    {-# INLINE byteVal #-}
    byteVal = 0xcb
instance ByteVal 0xcc where
    {-# INLINE byteVal #-}
    byteVal = 0xcc
instance ByteVal 0xcd where
    {-# INLINE byteVal #-}
    byteVal = 0xcd
instance ByteVal 0xce where
    {-# INLINE byteVal #-}
    byteVal = 0xce
instance ByteVal 0xcf where
    {-# INLINE byteVal #-}
    byteVal = 0xcf
instance ByteVal 0xd0 where
    {-# INLINE byteVal #-}
    byteVal = 0xd0
instance ByteVal 0xd1 where
    {-# INLINE byteVal #-}
    byteVal = 0xd1
instance ByteVal 0xd2 where
    {-# INLINE byteVal #-}
    byteVal = 0xd2
instance ByteVal 0xd3 where
    {-# INLINE byteVal #-}
    byteVal = 0xd3
instance ByteVal 0xd4 where
    {-# INLINE byteVal #-}
    byteVal = 0xd4
instance ByteVal 0xd5 where
    {-# INLINE byteVal #-}
    byteVal = 0xd5
instance ByteVal 0xd6 where
    {-# INLINE byteVal #-}
    byteVal = 0xd6
instance ByteVal 0xd7 where
    {-# INLINE byteVal #-}
    byteVal = 0xd7
instance ByteVal 0xd8 where
    {-# INLINE byteVal #-}
    byteVal = 0xd8
instance ByteVal 0xd9 where
    {-# INLINE byteVal #-}
    byteVal = 0xd9
instance ByteVal 0xda where
    {-# INLINE byteVal #-}
    byteVal = 0xda
instance ByteVal 0xdb where
    {-# INLINE byteVal #-}
    byteVal = 0xdb
instance ByteVal 0xdc where
    {-# INLINE byteVal #-}
    byteVal = 0xdc
instance ByteVal 0xdd where
    {-# INLINE byteVal #-}
    byteVal = 0xdd
instance ByteVal 0xde where
    {-# INLINE byteVal #-}
    byteVal = 0xde
instance ByteVal 0xdf where
    {-# INLINE byteVal #-}
    byteVal = 0xdf
instance ByteVal 0xe0 where
    {-# INLINE byteVal #-}
    byteVal = 0xe0
instance ByteVal 0xe1 where
    {-# INLINE byteVal #-}
    byteVal = 0xe1
instance ByteVal 0xe2 where
    {-# INLINE byteVal #-}
    byteVal = 0xe2
instance ByteVal 0xe3 where
    {-# INLINE byteVal #-}
    byteVal = 0xe3
instance ByteVal 0xe4 where
    {-# INLINE byteVal #-}
    byteVal = 0xe4
instance ByteVal 0xe5 where
    {-# INLINE byteVal #-}
    byteVal = 0xe5
instance ByteVal 0xe6 where
    {-# INLINE byteVal #-}
    byteVal = 0xe6
instance ByteVal 0xe7 where
    {-# INLINE byteVal #-}
    byteVal = 0xe7
instance ByteVal 0xe8 where
    {-# INLINE byteVal #-}
    byteVal = 0xe8
instance ByteVal 0xe9 where
    {-# INLINE byteVal #-}
    byteVal = 0xe9
instance ByteVal 0xea where
    {-# INLINE byteVal #-}
    byteVal = 0xea
instance ByteVal 0xeb where
    {-# INLINE byteVal #-}
    byteVal = 0xeb
instance ByteVal 0xec where
    {-# INLINE byteVal #-}
    byteVal = 0xec
instance ByteVal 0xed where
    {-# INLINE byteVal #-}
    byteVal = 0xed
instance ByteVal 0xee where
    {-# INLINE byteVal #-}
    byteVal = 0xee
instance ByteVal 0xef where
    {-# INLINE byteVal #-}
    byteVal = 0xef
instance ByteVal 0xf0 where
    {-# INLINE byteVal #-}
    byteVal = 0xf0
instance ByteVal 0xf1 where
    {-# INLINE byteVal #-}
    byteVal = 0xf1
instance ByteVal 0xf2 where
    {-# INLINE byteVal #-}
    byteVal = 0xf2
instance ByteVal 0xf3 where
    {-# INLINE byteVal #-}
    byteVal = 0xf3
instance ByteVal 0xf4 where
    {-# INLINE byteVal #-}
    byteVal = 0xf4
instance ByteVal 0xf5 where
    {-# INLINE byteVal #-}
    byteVal = 0xf5
instance ByteVal 0xf6 where
    {-# INLINE byteVal #-}
    byteVal = 0xf6
instance ByteVal 0xf7 where
    {-# INLINE byteVal #-}
    byteVal = 0xf7
instance ByteVal 0xf8 where
    {-# INLINE byteVal #-}
    byteVal = 0xf8
instance ByteVal 0xf9 where
    {-# INLINE byteVal #-}
    byteVal = 0xf9
instance ByteVal 0xfa where
    {-# INLINE byteVal #-}
    byteVal = 0xfa
instance ByteVal 0xfb where
    {-# INLINE byteVal #-}
    byteVal = 0xfb
instance ByteVal 0xfc where
    {-# INLINE byteVal #-}
    byteVal = 0xfc
instance ByteVal 0xfd where
    {-# INLINE byteVal #-}
    byteVal = 0xfd
instance ByteVal 0xfe where
    {-# INLINE byteVal #-}
    byteVal = 0xfe
instance ByteVal 0xff where
    {-# INLINE byteVal #-}
    byteVal = 0xff
