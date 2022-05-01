{- | Type-level definitions for 'Byte's.

Lol
-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Type.Byte.TypeLevel where

import Binrep.Type.Byte
import GHC.TypeNats
import GHC.TypeLits ( TypeError, ErrorMessage(Text) )
import GHC.Exts ( proxy#, Proxy# )

type family NatToByte (n :: Natural) :: Maybe Byte where
    NatToByte 0x00 = 'Just 'B00
    NatToByte 0x01 = 'Just 'B01
    NatToByte 0x02 = 'Just 'B02
    NatToByte 0x03 = 'Just 'B03
    NatToByte 0x04 = 'Just 'B04
    NatToByte 0x05 = 'Just 'B05
    NatToByte 0x06 = 'Just 'B06
    NatToByte 0x07 = 'Just 'B07
    NatToByte 0x08 = 'Just 'B08
    NatToByte 0x09 = 'Just 'B09
    NatToByte 0x0A = 'Just 'B0A
    NatToByte 0x0B = 'Just 'B0B
    NatToByte 0x0C = 'Just 'B0C
    NatToByte 0x0D = 'Just 'B0D
    NatToByte 0x0E = 'Just 'B0E
    NatToByte 0x0F = 'Just 'B0F
    NatToByte 0x10 = 'Just 'B10
    NatToByte 0x11 = 'Just 'B11
    NatToByte 0x12 = 'Just 'B12
    NatToByte 0x13 = 'Just 'B13
    NatToByte 0x14 = 'Just 'B14
    NatToByte 0x15 = 'Just 'B15
    NatToByte 0x16 = 'Just 'B16
    NatToByte 0x17 = 'Just 'B17
    NatToByte 0x18 = 'Just 'B18
    NatToByte 0x19 = 'Just 'B19
    NatToByte 0x1A = 'Just 'B1A
    NatToByte 0x1B = 'Just 'B1B
    NatToByte 0x1C = 'Just 'B1C
    NatToByte 0x1D = 'Just 'B1D
    NatToByte 0x1E = 'Just 'B1E
    NatToByte 0x1F = 'Just 'B1F
    NatToByte 0x20 = 'Just 'B20
    NatToByte 0x21 = 'Just 'B21
    NatToByte 0x22 = 'Just 'B22
    NatToByte 0x23 = 'Just 'B23
    NatToByte 0x24 = 'Just 'B24
    NatToByte 0x25 = 'Just 'B25
    NatToByte 0x26 = 'Just 'B26
    NatToByte 0x27 = 'Just 'B27
    NatToByte 0x28 = 'Just 'B28
    NatToByte 0x29 = 'Just 'B29
    NatToByte 0x2A = 'Just 'B2A
    NatToByte 0x2B = 'Just 'B2B
    NatToByte 0x2C = 'Just 'B2C
    NatToByte 0x2D = 'Just 'B2D
    NatToByte 0x2E = 'Just 'B2E
    NatToByte 0x2F = 'Just 'B2F
    NatToByte 0x30 = 'Just 'B30
    NatToByte 0x31 = 'Just 'B31
    NatToByte 0x32 = 'Just 'B32
    NatToByte 0x33 = 'Just 'B33
    NatToByte 0x34 = 'Just 'B34
    NatToByte 0x35 = 'Just 'B35
    NatToByte 0x36 = 'Just 'B36
    NatToByte 0x37 = 'Just 'B37
    NatToByte 0x38 = 'Just 'B38
    NatToByte 0x39 = 'Just 'B39
    NatToByte 0x3A = 'Just 'B3A
    NatToByte 0x3B = 'Just 'B3B
    NatToByte 0x3C = 'Just 'B3C
    NatToByte 0x3D = 'Just 'B3D
    NatToByte 0x3E = 'Just 'B3E
    NatToByte 0x3F = 'Just 'B3F
    NatToByte 0x40 = 'Just 'B40
    NatToByte 0x41 = 'Just 'B41
    NatToByte 0x42 = 'Just 'B42
    NatToByte 0x43 = 'Just 'B43
    NatToByte 0x44 = 'Just 'B44
    NatToByte 0x45 = 'Just 'B45
    NatToByte 0x46 = 'Just 'B46
    NatToByte 0x47 = 'Just 'B47
    NatToByte 0x48 = 'Just 'B48
    NatToByte 0x49 = 'Just 'B49
    NatToByte 0x4A = 'Just 'B4A
    NatToByte 0x4B = 'Just 'B4B
    NatToByte 0x4C = 'Just 'B4C
    NatToByte 0x4D = 'Just 'B4D
    NatToByte 0x4E = 'Just 'B4E
    NatToByte 0x4F = 'Just 'B4F
    NatToByte 0x50 = 'Just 'B50
    NatToByte 0x51 = 'Just 'B51
    NatToByte 0x52 = 'Just 'B52
    NatToByte 0x53 = 'Just 'B53
    NatToByte 0x54 = 'Just 'B54
    NatToByte 0x55 = 'Just 'B55
    NatToByte 0x56 = 'Just 'B56
    NatToByte 0x57 = 'Just 'B57
    NatToByte 0x58 = 'Just 'B58
    NatToByte 0x59 = 'Just 'B59
    NatToByte 0x5A = 'Just 'B5A
    NatToByte 0x5B = 'Just 'B5B
    NatToByte 0x5C = 'Just 'B5C
    NatToByte 0x5D = 'Just 'B5D
    NatToByte 0x5E = 'Just 'B5E
    NatToByte 0x5F = 'Just 'B5F
    NatToByte 0x60 = 'Just 'B60
    NatToByte 0x61 = 'Just 'B61
    NatToByte 0x62 = 'Just 'B62
    NatToByte 0x63 = 'Just 'B63
    NatToByte 0x64 = 'Just 'B64
    NatToByte 0x65 = 'Just 'B65
    NatToByte 0x66 = 'Just 'B66
    NatToByte 0x67 = 'Just 'B67
    NatToByte 0x68 = 'Just 'B68
    NatToByte 0x69 = 'Just 'B69
    NatToByte 0x6A = 'Just 'B6A
    NatToByte 0x6B = 'Just 'B6B
    NatToByte 0x6C = 'Just 'B6C
    NatToByte 0x6D = 'Just 'B6D
    NatToByte 0x6E = 'Just 'B6E
    NatToByte 0x6F = 'Just 'B6F
    NatToByte 0x70 = 'Just 'B70
    NatToByte 0x71 = 'Just 'B71
    NatToByte 0x72 = 'Just 'B72
    NatToByte 0x73 = 'Just 'B73
    NatToByte 0x74 = 'Just 'B74
    NatToByte 0x75 = 'Just 'B75
    NatToByte 0x76 = 'Just 'B76
    NatToByte 0x77 = 'Just 'B77
    NatToByte 0x78 = 'Just 'B78
    NatToByte 0x79 = 'Just 'B79
    NatToByte 0x7A = 'Just 'B7A
    NatToByte 0x7B = 'Just 'B7B
    NatToByte 0x7C = 'Just 'B7C
    NatToByte 0x7D = 'Just 'B7D
    NatToByte 0x7E = 'Just 'B7E
    NatToByte 0x7F = 'Just 'B7F
    NatToByte 0x80 = 'Just 'B80
    NatToByte 0x81 = 'Just 'B81
    NatToByte 0x82 = 'Just 'B82
    NatToByte 0x83 = 'Just 'B83
    NatToByte 0x84 = 'Just 'B84
    NatToByte 0x85 = 'Just 'B85
    NatToByte 0x86 = 'Just 'B86
    NatToByte 0x87 = 'Just 'B87
    NatToByte 0x88 = 'Just 'B88
    NatToByte 0x89 = 'Just 'B89
    NatToByte 0x8A = 'Just 'B8A
    NatToByte 0x8B = 'Just 'B8B
    NatToByte 0x8C = 'Just 'B8C
    NatToByte 0x8D = 'Just 'B8D
    NatToByte 0x8E = 'Just 'B8E
    NatToByte 0x8F = 'Just 'B8F
    NatToByte 0x90 = 'Just 'B90
    NatToByte 0x91 = 'Just 'B91
    NatToByte 0x92 = 'Just 'B92
    NatToByte 0x93 = 'Just 'B93
    NatToByte 0x94 = 'Just 'B94
    NatToByte 0x95 = 'Just 'B95
    NatToByte 0x96 = 'Just 'B96
    NatToByte 0x97 = 'Just 'B97
    NatToByte 0x98 = 'Just 'B98
    NatToByte 0x99 = 'Just 'B99
    NatToByte 0x9A = 'Just 'B9A
    NatToByte 0x9B = 'Just 'B9B
    NatToByte 0x9C = 'Just 'B9C
    NatToByte 0x9D = 'Just 'B9D
    NatToByte 0x9E = 'Just 'B9E
    NatToByte 0x9F = 'Just 'B9F
    NatToByte 0xA0 = 'Just 'BA0
    NatToByte 0xA1 = 'Just 'BA1
    NatToByte 0xA2 = 'Just 'BA2
    NatToByte 0xA3 = 'Just 'BA3
    NatToByte 0xA4 = 'Just 'BA4
    NatToByte 0xA5 = 'Just 'BA5
    NatToByte 0xA6 = 'Just 'BA6
    NatToByte 0xA7 = 'Just 'BA7
    NatToByte 0xA8 = 'Just 'BA8
    NatToByte 0xA9 = 'Just 'BA9
    NatToByte 0xAA = 'Just 'BAA
    NatToByte 0xAB = 'Just 'BAB
    NatToByte 0xAC = 'Just 'BAC
    NatToByte 0xAD = 'Just 'BAD
    NatToByte 0xAE = 'Just 'BAE
    NatToByte 0xAF = 'Just 'BAF
    NatToByte 0xB0 = 'Just 'BB0
    NatToByte 0xB1 = 'Just 'BB1
    NatToByte 0xB2 = 'Just 'BB2
    NatToByte 0xB3 = 'Just 'BB3
    NatToByte 0xB4 = 'Just 'BB4
    NatToByte 0xB5 = 'Just 'BB5
    NatToByte 0xB6 = 'Just 'BB6
    NatToByte 0xB7 = 'Just 'BB7
    NatToByte 0xB8 = 'Just 'BB8
    NatToByte 0xB9 = 'Just 'BB9
    NatToByte 0xBA = 'Just 'BBA
    NatToByte 0xBB = 'Just 'BBB
    NatToByte 0xBC = 'Just 'BBC
    NatToByte 0xBD = 'Just 'BBD
    NatToByte 0xBE = 'Just 'BBE
    NatToByte 0xBF = 'Just 'BBF
    NatToByte 0xC0 = 'Just 'BC0
    NatToByte 0xC1 = 'Just 'BC1
    NatToByte 0xC2 = 'Just 'BC2
    NatToByte 0xC3 = 'Just 'BC3
    NatToByte 0xC4 = 'Just 'BC4
    NatToByte 0xC5 = 'Just 'BC5
    NatToByte 0xC6 = 'Just 'BC6
    NatToByte 0xC7 = 'Just 'BC7
    NatToByte 0xC8 = 'Just 'BC8
    NatToByte 0xC9 = 'Just 'BC9
    NatToByte 0xCA = 'Just 'BCA
    NatToByte 0xCB = 'Just 'BCB
    NatToByte 0xCC = 'Just 'BCC
    NatToByte 0xCD = 'Just 'BCD
    NatToByte 0xCE = 'Just 'BCE
    NatToByte 0xCF = 'Just 'BCF
    NatToByte 0xD0 = 'Just 'BD0
    NatToByte 0xD1 = 'Just 'BD1
    NatToByte 0xD2 = 'Just 'BD2
    NatToByte 0xD3 = 'Just 'BD3
    NatToByte 0xD4 = 'Just 'BD4
    NatToByte 0xD5 = 'Just 'BD5
    NatToByte 0xD6 = 'Just 'BD6
    NatToByte 0xD7 = 'Just 'BD7
    NatToByte 0xD8 = 'Just 'BD8
    NatToByte 0xD9 = 'Just 'BD9
    NatToByte 0xDA = 'Just 'BDA
    NatToByte 0xDB = 'Just 'BDB
    NatToByte 0xDC = 'Just 'BDC
    NatToByte 0xDD = 'Just 'BDD
    NatToByte 0xDE = 'Just 'BDE
    NatToByte 0xDF = 'Just 'BDF
    NatToByte 0xE0 = 'Just 'BE0
    NatToByte 0xE1 = 'Just 'BE1
    NatToByte 0xE2 = 'Just 'BE2
    NatToByte 0xE3 = 'Just 'BE3
    NatToByte 0xE4 = 'Just 'BE4
    NatToByte 0xE5 = 'Just 'BE5
    NatToByte 0xE6 = 'Just 'BE6
    NatToByte 0xE7 = 'Just 'BE7
    NatToByte 0xE8 = 'Just 'BE8
    NatToByte 0xE9 = 'Just 'BE9
    NatToByte 0xEA = 'Just 'BEA
    NatToByte 0xEB = 'Just 'BEB
    NatToByte 0xEC = 'Just 'BEC
    NatToByte 0xED = 'Just 'BED
    NatToByte 0xEE = 'Just 'BEE
    NatToByte 0xEF = 'Just 'BEF
    NatToByte 0xF0 = 'Just 'BF0
    NatToByte 0xF1 = 'Just 'BF1
    NatToByte 0xF2 = 'Just 'BF2
    NatToByte 0xF3 = 'Just 'BF3
    NatToByte 0xF4 = 'Just 'BF4
    NatToByte 0xF5 = 'Just 'BF5
    NatToByte 0xF6 = 'Just 'BF6
    NatToByte 0xF7 = 'Just 'BF7
    NatToByte 0xF8 = 'Just 'BF8
    NatToByte 0xF9 = 'Just 'BF9
    NatToByte 0xFA = 'Just 'BFA
    NatToByte 0xFB = 'Just 'BFB
    NatToByte 0xFC = 'Just 'BFC
    NatToByte 0xFD = 'Just 'BFD
    NatToByte 0xFE = 'Just 'BFE
    NatToByte 0xFF = 'Just 'BFF
    NatToByte _    = 'Nothing

type family ByteToNat (n :: Byte) :: Natural where
    ByteToNat 'B00 = 0x00
    ByteToNat 'B01 = 0x01
    ByteToNat 'B02 = 0x02
    ByteToNat 'B03 = 0x03
    ByteToNat 'B04 = 0x04
    ByteToNat 'B05 = 0x05
    ByteToNat 'B06 = 0x06
    ByteToNat 'B07 = 0x07
    ByteToNat 'B08 = 0x08
    ByteToNat 'B09 = 0x09
    ByteToNat 'B0A = 0x0A
    ByteToNat 'B0B = 0x0B
    ByteToNat 'B0C = 0x0C
    ByteToNat 'B0D = 0x0D
    ByteToNat 'B0E = 0x0E
    ByteToNat 'B0F = 0x0F
    ByteToNat 'B10 = 0x10
    ByteToNat 'B11 = 0x11
    ByteToNat 'B12 = 0x12
    ByteToNat 'B13 = 0x13
    ByteToNat 'B14 = 0x14
    ByteToNat 'B15 = 0x15
    ByteToNat 'B16 = 0x16
    ByteToNat 'B17 = 0x17
    ByteToNat 'B18 = 0x18
    ByteToNat 'B19 = 0x19
    ByteToNat 'B1A = 0x1A
    ByteToNat 'B1B = 0x1B
    ByteToNat 'B1C = 0x1C
    ByteToNat 'B1D = 0x1D
    ByteToNat 'B1E = 0x1E
    ByteToNat 'B1F = 0x1F
    ByteToNat 'B20 = 0x20
    ByteToNat 'B21 = 0x21
    ByteToNat 'B22 = 0x22
    ByteToNat 'B23 = 0x23
    ByteToNat 'B24 = 0x24
    ByteToNat 'B25 = 0x25
    ByteToNat 'B26 = 0x26
    ByteToNat 'B27 = 0x27
    ByteToNat 'B28 = 0x28
    ByteToNat 'B29 = 0x29
    ByteToNat 'B2A = 0x2A
    ByteToNat 'B2B = 0x2B
    ByteToNat 'B2C = 0x2C
    ByteToNat 'B2D = 0x2D
    ByteToNat 'B2E = 0x2E
    ByteToNat 'B2F = 0x2F
    ByteToNat 'B30 = 0x30
    ByteToNat 'B31 = 0x31
    ByteToNat 'B32 = 0x32
    ByteToNat 'B33 = 0x33
    ByteToNat 'B34 = 0x34
    ByteToNat 'B35 = 0x35
    ByteToNat 'B36 = 0x36
    ByteToNat 'B37 = 0x37
    ByteToNat 'B38 = 0x38
    ByteToNat 'B39 = 0x39
    ByteToNat 'B3A = 0x3A
    ByteToNat 'B3B = 0x3B
    ByteToNat 'B3C = 0x3C
    ByteToNat 'B3D = 0x3D
    ByteToNat 'B3E = 0x3E
    ByteToNat 'B3F = 0x3F
    ByteToNat 'B40 = 0x40
    ByteToNat 'B41 = 0x41
    ByteToNat 'B42 = 0x42
    ByteToNat 'B43 = 0x43
    ByteToNat 'B44 = 0x44
    ByteToNat 'B45 = 0x45
    ByteToNat 'B46 = 0x46
    ByteToNat 'B47 = 0x47
    ByteToNat 'B48 = 0x48
    ByteToNat 'B49 = 0x49
    ByteToNat 'B4A = 0x4A
    ByteToNat 'B4B = 0x4B
    ByteToNat 'B4C = 0x4C
    ByteToNat 'B4D = 0x4D
    ByteToNat 'B4E = 0x4E
    ByteToNat 'B4F = 0x4F
    ByteToNat 'B50 = 0x50
    ByteToNat 'B51 = 0x51
    ByteToNat 'B52 = 0x52
    ByteToNat 'B53 = 0x53
    ByteToNat 'B54 = 0x54
    ByteToNat 'B55 = 0x55
    ByteToNat 'B56 = 0x56
    ByteToNat 'B57 = 0x57
    ByteToNat 'B58 = 0x58
    ByteToNat 'B59 = 0x59
    ByteToNat 'B5A = 0x5A
    ByteToNat 'B5B = 0x5B
    ByteToNat 'B5C = 0x5C
    ByteToNat 'B5D = 0x5D
    ByteToNat 'B5E = 0x5E
    ByteToNat 'B5F = 0x5F
    ByteToNat 'B60 = 0x60
    ByteToNat 'B61 = 0x61
    ByteToNat 'B62 = 0x62
    ByteToNat 'B63 = 0x63
    ByteToNat 'B64 = 0x64
    ByteToNat 'B65 = 0x65
    ByteToNat 'B66 = 0x66
    ByteToNat 'B67 = 0x67
    ByteToNat 'B68 = 0x68
    ByteToNat 'B69 = 0x69
    ByteToNat 'B6A = 0x6A
    ByteToNat 'B6B = 0x6B
    ByteToNat 'B6C = 0x6C
    ByteToNat 'B6D = 0x6D
    ByteToNat 'B6E = 0x6E
    ByteToNat 'B6F = 0x6F
    ByteToNat 'B70 = 0x70
    ByteToNat 'B71 = 0x71
    ByteToNat 'B72 = 0x72
    ByteToNat 'B73 = 0x73
    ByteToNat 'B74 = 0x74
    ByteToNat 'B75 = 0x75
    ByteToNat 'B76 = 0x76
    ByteToNat 'B77 = 0x77
    ByteToNat 'B78 = 0x78
    ByteToNat 'B79 = 0x79
    ByteToNat 'B7A = 0x7A
    ByteToNat 'B7B = 0x7B
    ByteToNat 'B7C = 0x7C
    ByteToNat 'B7D = 0x7D
    ByteToNat 'B7E = 0x7E
    ByteToNat 'B7F = 0x7F
    ByteToNat 'B80 = 0x80
    ByteToNat 'B81 = 0x81
    ByteToNat 'B82 = 0x82
    ByteToNat 'B83 = 0x83
    ByteToNat 'B84 = 0x84
    ByteToNat 'B85 = 0x85
    ByteToNat 'B86 = 0x86
    ByteToNat 'B87 = 0x87
    ByteToNat 'B88 = 0x88
    ByteToNat 'B89 = 0x89
    ByteToNat 'B8A = 0x8A
    ByteToNat 'B8B = 0x8B
    ByteToNat 'B8C = 0x8C
    ByteToNat 'B8D = 0x8D
    ByteToNat 'B8E = 0x8E
    ByteToNat 'B8F = 0x8F
    ByteToNat 'B90 = 0x90
    ByteToNat 'B91 = 0x91
    ByteToNat 'B92 = 0x92
    ByteToNat 'B93 = 0x93
    ByteToNat 'B94 = 0x94
    ByteToNat 'B95 = 0x95
    ByteToNat 'B96 = 0x96
    ByteToNat 'B97 = 0x97
    ByteToNat 'B98 = 0x98
    ByteToNat 'B99 = 0x99
    ByteToNat 'B9A = 0x9A
    ByteToNat 'B9B = 0x9B
    ByteToNat 'B9C = 0x9C
    ByteToNat 'B9D = 0x9D
    ByteToNat 'B9E = 0x9E
    ByteToNat 'B9F = 0x9F
    ByteToNat 'BA0 = 0xA0
    ByteToNat 'BA1 = 0xA1
    ByteToNat 'BA2 = 0xA2
    ByteToNat 'BA3 = 0xA3
    ByteToNat 'BA4 = 0xA4
    ByteToNat 'BA5 = 0xA5
    ByteToNat 'BA6 = 0xA6
    ByteToNat 'BA7 = 0xA7
    ByteToNat 'BA8 = 0xA8
    ByteToNat 'BA9 = 0xA9
    ByteToNat 'BAA = 0xAA
    ByteToNat 'BAB = 0xAB
    ByteToNat 'BAC = 0xAC
    ByteToNat 'BAD = 0xAD
    ByteToNat 'BAE = 0xAE
    ByteToNat 'BAF = 0xAF
    ByteToNat 'BB0 = 0xB0
    ByteToNat 'BB1 = 0xB1
    ByteToNat 'BB2 = 0xB2
    ByteToNat 'BB3 = 0xB3
    ByteToNat 'BB4 = 0xB4
    ByteToNat 'BB5 = 0xB5
    ByteToNat 'BB6 = 0xB6
    ByteToNat 'BB7 = 0xB7
    ByteToNat 'BB8 = 0xB8
    ByteToNat 'BB9 = 0xB9
    ByteToNat 'BBA = 0xBA
    ByteToNat 'BBB = 0xBB
    ByteToNat 'BBC = 0xBC
    ByteToNat 'BBD = 0xBD
    ByteToNat 'BBE = 0xBE
    ByteToNat 'BBF = 0xBF
    ByteToNat 'BC0 = 0xC0
    ByteToNat 'BC1 = 0xC1
    ByteToNat 'BC2 = 0xC2
    ByteToNat 'BC3 = 0xC3
    ByteToNat 'BC4 = 0xC4
    ByteToNat 'BC5 = 0xC5
    ByteToNat 'BC6 = 0xC6
    ByteToNat 'BC7 = 0xC7
    ByteToNat 'BC8 = 0xC8
    ByteToNat 'BC9 = 0xC9
    ByteToNat 'BCA = 0xCA
    ByteToNat 'BCB = 0xCB
    ByteToNat 'BCC = 0xCC
    ByteToNat 'BCD = 0xCD
    ByteToNat 'BCE = 0xCE
    ByteToNat 'BCF = 0xCF
    ByteToNat 'BD0 = 0xD0
    ByteToNat 'BD1 = 0xD1
    ByteToNat 'BD2 = 0xD2
    ByteToNat 'BD3 = 0xD3
    ByteToNat 'BD4 = 0xD4
    ByteToNat 'BD5 = 0xD5
    ByteToNat 'BD6 = 0xD6
    ByteToNat 'BD7 = 0xD7
    ByteToNat 'BD8 = 0xD8
    ByteToNat 'BD9 = 0xD9
    ByteToNat 'BDA = 0xDA
    ByteToNat 'BDB = 0xDB
    ByteToNat 'BDC = 0xDC
    ByteToNat 'BDD = 0xDD
    ByteToNat 'BDE = 0xDE
    ByteToNat 'BDF = 0xDF
    ByteToNat 'BE0 = 0xE0
    ByteToNat 'BE1 = 0xE1
    ByteToNat 'BE2 = 0xE2
    ByteToNat 'BE3 = 0xE3
    ByteToNat 'BE4 = 0xE4
    ByteToNat 'BE5 = 0xE5
    ByteToNat 'BE6 = 0xE6
    ByteToNat 'BE7 = 0xE7
    ByteToNat 'BE8 = 0xE8
    ByteToNat 'BE9 = 0xE9
    ByteToNat 'BEA = 0xEA
    ByteToNat 'BEB = 0xEB
    ByteToNat 'BEC = 0xEC
    ByteToNat 'BED = 0xED
    ByteToNat 'BEE = 0xEE
    ByteToNat 'BEF = 0xEF
    ByteToNat 'BF0 = 0xF0
    ByteToNat 'BF1 = 0xF1
    ByteToNat 'BF2 = 0xF2
    ByteToNat 'BF3 = 0xF3
    ByteToNat 'BF4 = 0xF4
    ByteToNat 'BF5 = 0xF5
    ByteToNat 'BF6 = 0xF6
    ByteToNat 'BF7 = 0xF7
    ByteToNat 'BF8 = 0xF8
    ByteToNat 'BF9 = 0xF9
    ByteToNat 'BFA = 0xFA
    ByteToNat 'BFB = 0xFB
    ByteToNat 'BFC = 0xFC
    ByteToNat 'BFD = 0xFD
    ByteToNat 'BFE = 0xFE
    ByteToNat 'BFF = 0xFF

-- | Inefficient reification via 'Natural'.
byteVal' :: forall (b :: Byte) n. (n ~ ByteToNat b, KnownNat n) => Natural
byteVal' = natVal' (proxy# :: Proxy# n)

-- | Converts a byte-encodable 'Natural' to a byte.
--
-- Type errors on 'Natural's larger than 255 (= 0xFF).
type family NatToByte' (n :: Natural) :: Byte where
    NatToByte' 0x00 = 'B00
    NatToByte' 0x01 = 'B01
    NatToByte' 0x02 = 'B02
    NatToByte' 0x03 = 'B03
    NatToByte' 0x04 = 'B04
    NatToByte' 0x05 = 'B05
    NatToByte' 0x06 = 'B06
    NatToByte' 0x07 = 'B07
    NatToByte' 0x08 = 'B08
    NatToByte' 0x09 = 'B09
    NatToByte' 0x0A = 'B0A
    NatToByte' 0x0B = 'B0B
    NatToByte' 0x0C = 'B0C
    NatToByte' 0x0D = 'B0D
    NatToByte' 0x0E = 'B0E
    NatToByte' 0x0F = 'B0F
    NatToByte' 0x10 = 'B10
    NatToByte' 0x11 = 'B11
    NatToByte' 0x12 = 'B12
    NatToByte' 0x13 = 'B13
    NatToByte' 0x14 = 'B14
    NatToByte' 0x15 = 'B15
    NatToByte' 0x16 = 'B16
    NatToByte' 0x17 = 'B17
    NatToByte' 0x18 = 'B18
    NatToByte' 0x19 = 'B19
    NatToByte' 0x1A = 'B1A
    NatToByte' 0x1B = 'B1B
    NatToByte' 0x1C = 'B1C
    NatToByte' 0x1D = 'B1D
    NatToByte' 0x1E = 'B1E
    NatToByte' 0x1F = 'B1F
    NatToByte' 0x20 = 'B20
    NatToByte' 0x21 = 'B21
    NatToByte' 0x22 = 'B22
    NatToByte' 0x23 = 'B23
    NatToByte' 0x24 = 'B24
    NatToByte' 0x25 = 'B25
    NatToByte' 0x26 = 'B26
    NatToByte' 0x27 = 'B27
    NatToByte' 0x28 = 'B28
    NatToByte' 0x29 = 'B29
    NatToByte' 0x2A = 'B2A
    NatToByte' 0x2B = 'B2B
    NatToByte' 0x2C = 'B2C
    NatToByte' 0x2D = 'B2D
    NatToByte' 0x2E = 'B2E
    NatToByte' 0x2F = 'B2F
    NatToByte' 0x30 = 'B30
    NatToByte' 0x31 = 'B31
    NatToByte' 0x32 = 'B32
    NatToByte' 0x33 = 'B33
    NatToByte' 0x34 = 'B34
    NatToByte' 0x35 = 'B35
    NatToByte' 0x36 = 'B36
    NatToByte' 0x37 = 'B37
    NatToByte' 0x38 = 'B38
    NatToByte' 0x39 = 'B39
    NatToByte' 0x3A = 'B3A
    NatToByte' 0x3B = 'B3B
    NatToByte' 0x3C = 'B3C
    NatToByte' 0x3D = 'B3D
    NatToByte' 0x3E = 'B3E
    NatToByte' 0x3F = 'B3F
    NatToByte' 0x40 = 'B40
    NatToByte' 0x41 = 'B41
    NatToByte' 0x42 = 'B42
    NatToByte' 0x43 = 'B43
    NatToByte' 0x44 = 'B44
    NatToByte' 0x45 = 'B45
    NatToByte' 0x46 = 'B46
    NatToByte' 0x47 = 'B47
    NatToByte' 0x48 = 'B48
    NatToByte' 0x49 = 'B49
    NatToByte' 0x4A = 'B4A
    NatToByte' 0x4B = 'B4B
    NatToByte' 0x4C = 'B4C
    NatToByte' 0x4D = 'B4D
    NatToByte' 0x4E = 'B4E
    NatToByte' 0x4F = 'B4F
    NatToByte' 0x50 = 'B50
    NatToByte' 0x51 = 'B51
    NatToByte' 0x52 = 'B52
    NatToByte' 0x53 = 'B53
    NatToByte' 0x54 = 'B54
    NatToByte' 0x55 = 'B55
    NatToByte' 0x56 = 'B56
    NatToByte' 0x57 = 'B57
    NatToByte' 0x58 = 'B58
    NatToByte' 0x59 = 'B59
    NatToByte' 0x5A = 'B5A
    NatToByte' 0x5B = 'B5B
    NatToByte' 0x5C = 'B5C
    NatToByte' 0x5D = 'B5D
    NatToByte' 0x5E = 'B5E
    NatToByte' 0x5F = 'B5F
    NatToByte' 0x60 = 'B60
    NatToByte' 0x61 = 'B61
    NatToByte' 0x62 = 'B62
    NatToByte' 0x63 = 'B63
    NatToByte' 0x64 = 'B64
    NatToByte' 0x65 = 'B65
    NatToByte' 0x66 = 'B66
    NatToByte' 0x67 = 'B67
    NatToByte' 0x68 = 'B68
    NatToByte' 0x69 = 'B69
    NatToByte' 0x6A = 'B6A
    NatToByte' 0x6B = 'B6B
    NatToByte' 0x6C = 'B6C
    NatToByte' 0x6D = 'B6D
    NatToByte' 0x6E = 'B6E
    NatToByte' 0x6F = 'B6F
    NatToByte' 0x70 = 'B70
    NatToByte' 0x71 = 'B71
    NatToByte' 0x72 = 'B72
    NatToByte' 0x73 = 'B73
    NatToByte' 0x74 = 'B74
    NatToByte' 0x75 = 'B75
    NatToByte' 0x76 = 'B76
    NatToByte' 0x77 = 'B77
    NatToByte' 0x78 = 'B78
    NatToByte' 0x79 = 'B79
    NatToByte' 0x7A = 'B7A
    NatToByte' 0x7B = 'B7B
    NatToByte' 0x7C = 'B7C
    NatToByte' 0x7D = 'B7D
    NatToByte' 0x7E = 'B7E
    NatToByte' 0x7F = 'B7F
    NatToByte' 0x80 = 'B80
    NatToByte' 0x81 = 'B81
    NatToByte' 0x82 = 'B82
    NatToByte' 0x83 = 'B83
    NatToByte' 0x84 = 'B84
    NatToByte' 0x85 = 'B85
    NatToByte' 0x86 = 'B86
    NatToByte' 0x87 = 'B87
    NatToByte' 0x88 = 'B88
    NatToByte' 0x89 = 'B89
    NatToByte' 0x8A = 'B8A
    NatToByte' 0x8B = 'B8B
    NatToByte' 0x8C = 'B8C
    NatToByte' 0x8D = 'B8D
    NatToByte' 0x8E = 'B8E
    NatToByte' 0x8F = 'B8F
    NatToByte' 0x90 = 'B90
    NatToByte' 0x91 = 'B91
    NatToByte' 0x92 = 'B92
    NatToByte' 0x93 = 'B93
    NatToByte' 0x94 = 'B94
    NatToByte' 0x95 = 'B95
    NatToByte' 0x96 = 'B96
    NatToByte' 0x97 = 'B97
    NatToByte' 0x98 = 'B98
    NatToByte' 0x99 = 'B99
    NatToByte' 0x9A = 'B9A
    NatToByte' 0x9B = 'B9B
    NatToByte' 0x9C = 'B9C
    NatToByte' 0x9D = 'B9D
    NatToByte' 0x9E = 'B9E
    NatToByte' 0x9F = 'B9F
    NatToByte' 0xA0 = 'BA0
    NatToByte' 0xA1 = 'BA1
    NatToByte' 0xA2 = 'BA2
    NatToByte' 0xA3 = 'BA3
    NatToByte' 0xA4 = 'BA4
    NatToByte' 0xA5 = 'BA5
    NatToByte' 0xA6 = 'BA6
    NatToByte' 0xA7 = 'BA7
    NatToByte' 0xA8 = 'BA8
    NatToByte' 0xA9 = 'BA9
    NatToByte' 0xAA = 'BAA
    NatToByte' 0xAB = 'BAB
    NatToByte' 0xAC = 'BAC
    NatToByte' 0xAD = 'BAD
    NatToByte' 0xAE = 'BAE
    NatToByte' 0xAF = 'BAF
    NatToByte' 0xB0 = 'BB0
    NatToByte' 0xB1 = 'BB1
    NatToByte' 0xB2 = 'BB2
    NatToByte' 0xB3 = 'BB3
    NatToByte' 0xB4 = 'BB4
    NatToByte' 0xB5 = 'BB5
    NatToByte' 0xB6 = 'BB6
    NatToByte' 0xB7 = 'BB7
    NatToByte' 0xB8 = 'BB8
    NatToByte' 0xB9 = 'BB9
    NatToByte' 0xBA = 'BBA
    NatToByte' 0xBB = 'BBB
    NatToByte' 0xBC = 'BBC
    NatToByte' 0xBD = 'BBD
    NatToByte' 0xBE = 'BBE
    NatToByte' 0xBF = 'BBF
    NatToByte' 0xC0 = 'BC0
    NatToByte' 0xC1 = 'BC1
    NatToByte' 0xC2 = 'BC2
    NatToByte' 0xC3 = 'BC3
    NatToByte' 0xC4 = 'BC4
    NatToByte' 0xC5 = 'BC5
    NatToByte' 0xC6 = 'BC6
    NatToByte' 0xC7 = 'BC7
    NatToByte' 0xC8 = 'BC8
    NatToByte' 0xC9 = 'BC9
    NatToByte' 0xCA = 'BCA
    NatToByte' 0xCB = 'BCB
    NatToByte' 0xCC = 'BCC
    NatToByte' 0xCD = 'BCD
    NatToByte' 0xCE = 'BCE
    NatToByte' 0xCF = 'BCF
    NatToByte' 0xD0 = 'BD0
    NatToByte' 0xD1 = 'BD1
    NatToByte' 0xD2 = 'BD2
    NatToByte' 0xD3 = 'BD3
    NatToByte' 0xD4 = 'BD4
    NatToByte' 0xD5 = 'BD5
    NatToByte' 0xD6 = 'BD6
    NatToByte' 0xD7 = 'BD7
    NatToByte' 0xD8 = 'BD8
    NatToByte' 0xD9 = 'BD9
    NatToByte' 0xDA = 'BDA
    NatToByte' 0xDB = 'BDB
    NatToByte' 0xDC = 'BDC
    NatToByte' 0xDD = 'BDD
    NatToByte' 0xDE = 'BDE
    NatToByte' 0xDF = 'BDF
    NatToByte' 0xE0 = 'BE0
    NatToByte' 0xE1 = 'BE1
    NatToByte' 0xE2 = 'BE2
    NatToByte' 0xE3 = 'BE3
    NatToByte' 0xE4 = 'BE4
    NatToByte' 0xE5 = 'BE5
    NatToByte' 0xE6 = 'BE6
    NatToByte' 0xE7 = 'BE7
    NatToByte' 0xE8 = 'BE8
    NatToByte' 0xE9 = 'BE9
    NatToByte' 0xEA = 'BEA
    NatToByte' 0xEB = 'BEB
    NatToByte' 0xEC = 'BEC
    NatToByte' 0xED = 'BED
    NatToByte' 0xEE = 'BEE
    NatToByte' 0xEF = 'BEF
    NatToByte' 0xF0 = 'BF0
    NatToByte' 0xF1 = 'BF1
    NatToByte' 0xF2 = 'BF2
    NatToByte' 0xF3 = 'BF3
    NatToByte' 0xF4 = 'BF4
    NatToByte' 0xF5 = 'BF5
    NatToByte' 0xF6 = 'BF6
    NatToByte' 0xF7 = 'BF7
    NatToByte' 0xF8 = 'BF8
    NatToByte' 0xF9 = 'BF9
    NatToByte' 0xFA = 'BFA
    NatToByte' 0xFB = 'BFB
    NatToByte' 0xFC = 'BFC
    NatToByte' 0xFD = 'BFD
    NatToByte' 0xFE = 'BFE
    NatToByte' 0xFF = 'BFF
    NatToByte' _    = TypeError ('Text "Natural too big for byte")