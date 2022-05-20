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

import Mason.Builder qualified as Mason
import Data.ByteString.Builder.Prim.Internal qualified as BI
import Binrep.Util ( natVal'' )
import Binrep.Put ( Builder )
import GHC.TypeNats
import GHC.Exts

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

-- Needs to be a function type to work. Interesting. It's perhaps not an
-- improvement on regular boxed. But interesting idea, so sticking with it.
class ByteVal (b :: Byte) where byteVal :: Proxy# b -> Word8#

instance ByteVal 'B00 where
    byteVal _ = wordToWord8# 0x00##
    {-# INLINE byteVal #-}
instance ByteVal 'B01 where
    byteVal _ = wordToWord8# 0x01##
    {-# INLINE byteVal #-}
instance ByteVal 'B02 where
    byteVal _ = wordToWord8# 0x02##
    {-# INLINE byteVal #-}
instance ByteVal 'B03 where
    byteVal _ = wordToWord8# 0x03##
    {-# INLINE byteVal #-}
instance ByteVal 'B04 where
    byteVal _ = wordToWord8# 0x04##
    {-# INLINE byteVal #-}
instance ByteVal 'B05 where
    byteVal _ = wordToWord8# 0x05##
    {-# INLINE byteVal #-}
instance ByteVal 'B06 where
    byteVal _ = wordToWord8# 0x06##
    {-# INLINE byteVal #-}
instance ByteVal 'B07 where
    byteVal _ = wordToWord8# 0x07##
    {-# INLINE byteVal #-}
instance ByteVal 'B08 where
    byteVal _ = wordToWord8# 0x08##
    {-# INLINE byteVal #-}
instance ByteVal 'B09 where
    byteVal _ = wordToWord8# 0x09##
    {-# INLINE byteVal #-}
instance ByteVal 'B0A where
    byteVal _ = wordToWord8# 0x0A##
    {-# INLINE byteVal #-}
instance ByteVal 'B0B where
    byteVal _ = wordToWord8# 0x0B##
    {-# INLINE byteVal #-}
instance ByteVal 'B0C where
    byteVal _ = wordToWord8# 0x0C##
    {-# INLINE byteVal #-}
instance ByteVal 'B0D where
    byteVal _ = wordToWord8# 0x0D##
    {-# INLINE byteVal #-}
instance ByteVal 'B0E where
    byteVal _ = wordToWord8# 0x0E##
    {-# INLINE byteVal #-}
instance ByteVal 'B0F where
    byteVal _ = wordToWord8# 0x0F##
    {-# INLINE byteVal #-}
instance ByteVal 'B10 where
    byteVal _ = wordToWord8# 0x10##
    {-# INLINE byteVal #-}
instance ByteVal 'B11 where
    byteVal _ = wordToWord8# 0x11##
    {-# INLINE byteVal #-}
instance ByteVal 'B12 where
    byteVal _ = wordToWord8# 0x12##
    {-# INLINE byteVal #-}
instance ByteVal 'B13 where
    byteVal _ = wordToWord8# 0x13##
    {-# INLINE byteVal #-}
instance ByteVal 'B14 where
    byteVal _ = wordToWord8# 0x14##
    {-# INLINE byteVal #-}
instance ByteVal 'B15 where
    byteVal _ = wordToWord8# 0x15##
    {-# INLINE byteVal #-}
instance ByteVal 'B16 where
    byteVal _ = wordToWord8# 0x16##
    {-# INLINE byteVal #-}
instance ByteVal 'B17 where
    byteVal _ = wordToWord8# 0x17##
    {-# INLINE byteVal #-}
instance ByteVal 'B18 where
    byteVal _ = wordToWord8# 0x18##
    {-# INLINE byteVal #-}
instance ByteVal 'B19 where
    byteVal _ = wordToWord8# 0x19##
    {-# INLINE byteVal #-}
instance ByteVal 'B1A where
    byteVal _ = wordToWord8# 0x1A##
    {-# INLINE byteVal #-}
instance ByteVal 'B1B where
    byteVal _ = wordToWord8# 0x1B##
    {-# INLINE byteVal #-}
instance ByteVal 'B1C where
    byteVal _ = wordToWord8# 0x1C##
    {-# INLINE byteVal #-}
instance ByteVal 'B1D where
    byteVal _ = wordToWord8# 0x1D##
    {-# INLINE byteVal #-}
instance ByteVal 'B1E where
    byteVal _ = wordToWord8# 0x1E##
    {-# INLINE byteVal #-}
instance ByteVal 'B1F where
    byteVal _ = wordToWord8# 0x1F##
    {-# INLINE byteVal #-}
instance ByteVal 'B20 where
    byteVal _ = wordToWord8# 0x20##
    {-# INLINE byteVal #-}
instance ByteVal 'B21 where
    byteVal _ = wordToWord8# 0x21##
    {-# INLINE byteVal #-}
instance ByteVal 'B22 where
    byteVal _ = wordToWord8# 0x22##
    {-# INLINE byteVal #-}
instance ByteVal 'B23 where
    byteVal _ = wordToWord8# 0x23##
    {-# INLINE byteVal #-}
instance ByteVal 'B24 where
    byteVal _ = wordToWord8# 0x24##
    {-# INLINE byteVal #-}
instance ByteVal 'B25 where
    byteVal _ = wordToWord8# 0x25##
    {-# INLINE byteVal #-}
instance ByteVal 'B26 where
    byteVal _ = wordToWord8# 0x26##
    {-# INLINE byteVal #-}
instance ByteVal 'B27 where
    byteVal _ = wordToWord8# 0x27##
    {-# INLINE byteVal #-}
instance ByteVal 'B28 where
    byteVal _ = wordToWord8# 0x28##
    {-# INLINE byteVal #-}
instance ByteVal 'B29 where
    byteVal _ = wordToWord8# 0x29##
    {-# INLINE byteVal #-}
instance ByteVal 'B2A where
    byteVal _ = wordToWord8# 0x2A##
    {-# INLINE byteVal #-}
instance ByteVal 'B2B where
    byteVal _ = wordToWord8# 0x2B##
    {-# INLINE byteVal #-}
instance ByteVal 'B2C where
    byteVal _ = wordToWord8# 0x2C##
    {-# INLINE byteVal #-}
instance ByteVal 'B2D where
    byteVal _ = wordToWord8# 0x2D##
    {-# INLINE byteVal #-}
instance ByteVal 'B2E where
    byteVal _ = wordToWord8# 0x2E##
    {-# INLINE byteVal #-}
instance ByteVal 'B2F where
    byteVal _ = wordToWord8# 0x2F##
    {-# INLINE byteVal #-}
instance ByteVal 'B30 where
    byteVal _ = wordToWord8# 0x30##
    {-# INLINE byteVal #-}
instance ByteVal 'B31 where
    byteVal _ = wordToWord8# 0x31##
    {-# INLINE byteVal #-}
instance ByteVal 'B32 where
    byteVal _ = wordToWord8# 0x32##
    {-# INLINE byteVal #-}
instance ByteVal 'B33 where
    byteVal _ = wordToWord8# 0x33##
    {-# INLINE byteVal #-}
instance ByteVal 'B34 where
    byteVal _ = wordToWord8# 0x34##
    {-# INLINE byteVal #-}
instance ByteVal 'B35 where
    byteVal _ = wordToWord8# 0x35##
    {-# INLINE byteVal #-}
instance ByteVal 'B36 where
    byteVal _ = wordToWord8# 0x36##
    {-# INLINE byteVal #-}
instance ByteVal 'B37 where
    byteVal _ = wordToWord8# 0x37##
    {-# INLINE byteVal #-}
instance ByteVal 'B38 where
    byteVal _ = wordToWord8# 0x38##
    {-# INLINE byteVal #-}
instance ByteVal 'B39 where
    byteVal _ = wordToWord8# 0x39##
    {-# INLINE byteVal #-}
instance ByteVal 'B3A where
    byteVal _ = wordToWord8# 0x3A##
    {-# INLINE byteVal #-}
instance ByteVal 'B3B where
    byteVal _ = wordToWord8# 0x3B##
    {-# INLINE byteVal #-}
instance ByteVal 'B3C where
    byteVal _ = wordToWord8# 0x3C##
    {-# INLINE byteVal #-}
instance ByteVal 'B3D where
    byteVal _ = wordToWord8# 0x3D##
    {-# INLINE byteVal #-}
instance ByteVal 'B3E where
    byteVal _ = wordToWord8# 0x3E##
    {-# INLINE byteVal #-}
instance ByteVal 'B3F where
    byteVal _ = wordToWord8# 0x3F##
    {-# INLINE byteVal #-}
instance ByteVal 'B40 where
    byteVal _ = wordToWord8# 0x40##
    {-# INLINE byteVal #-}
instance ByteVal 'B41 where
    byteVal _ = wordToWord8# 0x41##
    {-# INLINE byteVal #-}
instance ByteVal 'B42 where
    byteVal _ = wordToWord8# 0x42##
    {-# INLINE byteVal #-}
instance ByteVal 'B43 where
    byteVal _ = wordToWord8# 0x43##
    {-# INLINE byteVal #-}
instance ByteVal 'B44 where
    byteVal _ = wordToWord8# 0x44##
    {-# INLINE byteVal #-}
instance ByteVal 'B45 where
    byteVal _ = wordToWord8# 0x45##
    {-# INLINE byteVal #-}
instance ByteVal 'B46 where
    byteVal _ = wordToWord8# 0x46##
    {-# INLINE byteVal #-}
instance ByteVal 'B47 where
    byteVal _ = wordToWord8# 0x47##
    {-# INLINE byteVal #-}
instance ByteVal 'B48 where
    byteVal _ = wordToWord8# 0x48##
    {-# INLINE byteVal #-}
instance ByteVal 'B49 where
    byteVal _ = wordToWord8# 0x49##
    {-# INLINE byteVal #-}
instance ByteVal 'B4A where
    byteVal _ = wordToWord8# 0x4A##
    {-# INLINE byteVal #-}
instance ByteVal 'B4B where
    byteVal _ = wordToWord8# 0x4B##
    {-# INLINE byteVal #-}
instance ByteVal 'B4C where
    byteVal _ = wordToWord8# 0x4C##
    {-# INLINE byteVal #-}
instance ByteVal 'B4D where
    byteVal _ = wordToWord8# 0x4D##
    {-# INLINE byteVal #-}
instance ByteVal 'B4E where
    byteVal _ = wordToWord8# 0x4E##
    {-# INLINE byteVal #-}
instance ByteVal 'B4F where
    byteVal _ = wordToWord8# 0x4F##
    {-# INLINE byteVal #-}
instance ByteVal 'B50 where
    byteVal _ = wordToWord8# 0x50##
    {-# INLINE byteVal #-}
instance ByteVal 'B51 where
    byteVal _ = wordToWord8# 0x51##
    {-# INLINE byteVal #-}
instance ByteVal 'B52 where
    byteVal _ = wordToWord8# 0x52##
    {-# INLINE byteVal #-}
instance ByteVal 'B53 where
    byteVal _ = wordToWord8# 0x53##
    {-# INLINE byteVal #-}
instance ByteVal 'B54 where
    byteVal _ = wordToWord8# 0x54##
    {-# INLINE byteVal #-}
instance ByteVal 'B55 where
    byteVal _ = wordToWord8# 0x55##
    {-# INLINE byteVal #-}
instance ByteVal 'B56 where
    byteVal _ = wordToWord8# 0x56##
    {-# INLINE byteVal #-}
instance ByteVal 'B57 where
    byteVal _ = wordToWord8# 0x57##
    {-# INLINE byteVal #-}
instance ByteVal 'B58 where
    byteVal _ = wordToWord8# 0x58##
    {-# INLINE byteVal #-}
instance ByteVal 'B59 where
    byteVal _ = wordToWord8# 0x59##
    {-# INLINE byteVal #-}
instance ByteVal 'B5A where
    byteVal _ = wordToWord8# 0x5A##
    {-# INLINE byteVal #-}
instance ByteVal 'B5B where
    byteVal _ = wordToWord8# 0x5B##
    {-# INLINE byteVal #-}
instance ByteVal 'B5C where
    byteVal _ = wordToWord8# 0x5C##
    {-# INLINE byteVal #-}
instance ByteVal 'B5D where
    byteVal _ = wordToWord8# 0x5D##
    {-# INLINE byteVal #-}
instance ByteVal 'B5E where
    byteVal _ = wordToWord8# 0x5E##
    {-# INLINE byteVal #-}
instance ByteVal 'B5F where
    byteVal _ = wordToWord8# 0x5F##
    {-# INLINE byteVal #-}
instance ByteVal 'B60 where
    byteVal _ = wordToWord8# 0x60##
    {-# INLINE byteVal #-}
instance ByteVal 'B61 where
    byteVal _ = wordToWord8# 0x61##
    {-# INLINE byteVal #-}
instance ByteVal 'B62 where
    byteVal _ = wordToWord8# 0x62##
    {-# INLINE byteVal #-}
instance ByteVal 'B63 where
    byteVal _ = wordToWord8# 0x63##
    {-# INLINE byteVal #-}
instance ByteVal 'B64 where
    byteVal _ = wordToWord8# 0x64##
    {-# INLINE byteVal #-}
instance ByteVal 'B65 where
    byteVal _ = wordToWord8# 0x65##
    {-# INLINE byteVal #-}
instance ByteVal 'B66 where
    byteVal _ = wordToWord8# 0x66##
    {-# INLINE byteVal #-}
instance ByteVal 'B67 where
    byteVal _ = wordToWord8# 0x67##
    {-# INLINE byteVal #-}
instance ByteVal 'B68 where
    byteVal _ = wordToWord8# 0x68##
    {-# INLINE byteVal #-}
instance ByteVal 'B69 where
    byteVal _ = wordToWord8# 0x69##
    {-# INLINE byteVal #-}
instance ByteVal 'B6A where
    byteVal _ = wordToWord8# 0x6A##
    {-# INLINE byteVal #-}
instance ByteVal 'B6B where
    byteVal _ = wordToWord8# 0x6B##
    {-# INLINE byteVal #-}
instance ByteVal 'B6C where
    byteVal _ = wordToWord8# 0x6C##
    {-# INLINE byteVal #-}
instance ByteVal 'B6D where
    byteVal _ = wordToWord8# 0x6D##
    {-# INLINE byteVal #-}
instance ByteVal 'B6E where
    byteVal _ = wordToWord8# 0x6E##
    {-# INLINE byteVal #-}
instance ByteVal 'B6F where
    byteVal _ = wordToWord8# 0x6F##
    {-# INLINE byteVal #-}
instance ByteVal 'B70 where
    byteVal _ = wordToWord8# 0x70##
    {-# INLINE byteVal #-}
instance ByteVal 'B71 where
    byteVal _ = wordToWord8# 0x71##
    {-# INLINE byteVal #-}
instance ByteVal 'B72 where
    byteVal _ = wordToWord8# 0x72##
    {-# INLINE byteVal #-}
instance ByteVal 'B73 where
    byteVal _ = wordToWord8# 0x73##
    {-# INLINE byteVal #-}
instance ByteVal 'B74 where
    byteVal _ = wordToWord8# 0x74##
    {-# INLINE byteVal #-}
instance ByteVal 'B75 where
    byteVal _ = wordToWord8# 0x75##
    {-# INLINE byteVal #-}
instance ByteVal 'B76 where
    byteVal _ = wordToWord8# 0x76##
    {-# INLINE byteVal #-}
instance ByteVal 'B77 where
    byteVal _ = wordToWord8# 0x77##
    {-# INLINE byteVal #-}
instance ByteVal 'B78 where
    byteVal _ = wordToWord8# 0x78##
    {-# INLINE byteVal #-}
instance ByteVal 'B79 where
    byteVal _ = wordToWord8# 0x79##
    {-# INLINE byteVal #-}
instance ByteVal 'B7A where
    byteVal _ = wordToWord8# 0x7A##
    {-# INLINE byteVal #-}
instance ByteVal 'B7B where
    byteVal _ = wordToWord8# 0x7B##
    {-# INLINE byteVal #-}
instance ByteVal 'B7C where
    byteVal _ = wordToWord8# 0x7C##
    {-# INLINE byteVal #-}
instance ByteVal 'B7D where
    byteVal _ = wordToWord8# 0x7D##
    {-# INLINE byteVal #-}
instance ByteVal 'B7E where
    byteVal _ = wordToWord8# 0x7E##
    {-# INLINE byteVal #-}
instance ByteVal 'B7F where
    byteVal _ = wordToWord8# 0x7F##
    {-# INLINE byteVal #-}
instance ByteVal 'B80 where
    byteVal _ = wordToWord8# 0x80##
    {-# INLINE byteVal #-}
instance ByteVal 'B81 where
    byteVal _ = wordToWord8# 0x81##
    {-# INLINE byteVal #-}
instance ByteVal 'B82 where
    byteVal _ = wordToWord8# 0x82##
    {-# INLINE byteVal #-}
instance ByteVal 'B83 where
    byteVal _ = wordToWord8# 0x83##
    {-# INLINE byteVal #-}
instance ByteVal 'B84 where
    byteVal _ = wordToWord8# 0x84##
    {-# INLINE byteVal #-}
instance ByteVal 'B85 where
    byteVal _ = wordToWord8# 0x85##
    {-# INLINE byteVal #-}
instance ByteVal 'B86 where
    byteVal _ = wordToWord8# 0x86##
    {-# INLINE byteVal #-}
instance ByteVal 'B87 where
    byteVal _ = wordToWord8# 0x87##
    {-# INLINE byteVal #-}
instance ByteVal 'B88 where
    byteVal _ = wordToWord8# 0x88##
    {-# INLINE byteVal #-}
instance ByteVal 'B89 where
    byteVal _ = wordToWord8# 0x89##
    {-# INLINE byteVal #-}
instance ByteVal 'B8A where
    byteVal _ = wordToWord8# 0x8A##
    {-# INLINE byteVal #-}
instance ByteVal 'B8B where
    byteVal _ = wordToWord8# 0x8B##
    {-# INLINE byteVal #-}
instance ByteVal 'B8C where
    byteVal _ = wordToWord8# 0x8C##
    {-# INLINE byteVal #-}
instance ByteVal 'B8D where
    byteVal _ = wordToWord8# 0x8D##
    {-# INLINE byteVal #-}
instance ByteVal 'B8E where
    byteVal _ = wordToWord8# 0x8E##
    {-# INLINE byteVal #-}
instance ByteVal 'B8F where
    byteVal _ = wordToWord8# 0x8F##
    {-# INLINE byteVal #-}
instance ByteVal 'B90 where
    byteVal _ = wordToWord8# 0x90##
    {-# INLINE byteVal #-}
instance ByteVal 'B91 where
    byteVal _ = wordToWord8# 0x91##
    {-# INLINE byteVal #-}
instance ByteVal 'B92 where
    byteVal _ = wordToWord8# 0x92##
    {-# INLINE byteVal #-}
instance ByteVal 'B93 where
    byteVal _ = wordToWord8# 0x93##
    {-# INLINE byteVal #-}
instance ByteVal 'B94 where
    byteVal _ = wordToWord8# 0x94##
    {-# INLINE byteVal #-}
instance ByteVal 'B95 where
    byteVal _ = wordToWord8# 0x95##
    {-# INLINE byteVal #-}
instance ByteVal 'B96 where
    byteVal _ = wordToWord8# 0x96##
    {-# INLINE byteVal #-}
instance ByteVal 'B97 where
    byteVal _ = wordToWord8# 0x97##
    {-# INLINE byteVal #-}
instance ByteVal 'B98 where
    byteVal _ = wordToWord8# 0x98##
    {-# INLINE byteVal #-}
instance ByteVal 'B99 where
    byteVal _ = wordToWord8# 0x99##
    {-# INLINE byteVal #-}
instance ByteVal 'B9A where
    byteVal _ = wordToWord8# 0x9A##
    {-# INLINE byteVal #-}
instance ByteVal 'B9B where
    byteVal _ = wordToWord8# 0x9B##
    {-# INLINE byteVal #-}
instance ByteVal 'B9C where
    byteVal _ = wordToWord8# 0x9C##
    {-# INLINE byteVal #-}
instance ByteVal 'B9D where
    byteVal _ = wordToWord8# 0x9D##
    {-# INLINE byteVal #-}
instance ByteVal 'B9E where
    byteVal _ = wordToWord8# 0x9E##
    {-# INLINE byteVal #-}
instance ByteVal 'B9F where
    byteVal _ = wordToWord8# 0x9F##
    {-# INLINE byteVal #-}
instance ByteVal 'BA0 where
    byteVal _ = wordToWord8# 0xA0##
    {-# INLINE byteVal #-}
instance ByteVal 'BA1 where
    byteVal _ = wordToWord8# 0xA1##
    {-# INLINE byteVal #-}
instance ByteVal 'BA2 where
    byteVal _ = wordToWord8# 0xA2##
    {-# INLINE byteVal #-}
instance ByteVal 'BA3 where
    byteVal _ = wordToWord8# 0xA3##
    {-# INLINE byteVal #-}
instance ByteVal 'BA4 where
    byteVal _ = wordToWord8# 0xA4##
    {-# INLINE byteVal #-}
instance ByteVal 'BA5 where
    byteVal _ = wordToWord8# 0xA5##
    {-# INLINE byteVal #-}
instance ByteVal 'BA6 where
    byteVal _ = wordToWord8# 0xA6##
    {-# INLINE byteVal #-}
instance ByteVal 'BA7 where
    byteVal _ = wordToWord8# 0xA7##
    {-# INLINE byteVal #-}
instance ByteVal 'BA8 where
    byteVal _ = wordToWord8# 0xA8##
    {-# INLINE byteVal #-}
instance ByteVal 'BA9 where
    byteVal _ = wordToWord8# 0xA9##
    {-# INLINE byteVal #-}
instance ByteVal 'BAA where
    byteVal _ = wordToWord8# 0xAA##
    {-# INLINE byteVal #-}
instance ByteVal 'BAB where
    byteVal _ = wordToWord8# 0xAB##
    {-# INLINE byteVal #-}
instance ByteVal 'BAC where
    byteVal _ = wordToWord8# 0xAC##
    {-# INLINE byteVal #-}
instance ByteVal 'BAD where
    byteVal _ = wordToWord8# 0xAD##
    {-# INLINE byteVal #-}
instance ByteVal 'BAE where
    byteVal _ = wordToWord8# 0xAE##
    {-# INLINE byteVal #-}
instance ByteVal 'BAF where
    byteVal _ = wordToWord8# 0xAF##
    {-# INLINE byteVal #-}
instance ByteVal 'BB0 where
    byteVal _ = wordToWord8# 0xB0##
    {-# INLINE byteVal #-}
instance ByteVal 'BB1 where
    byteVal _ = wordToWord8# 0xB1##
    {-# INLINE byteVal #-}
instance ByteVal 'BB2 where
    byteVal _ = wordToWord8# 0xB2##
    {-# INLINE byteVal #-}
instance ByteVal 'BB3 where
    byteVal _ = wordToWord8# 0xB3##
    {-# INLINE byteVal #-}
instance ByteVal 'BB4 where
    byteVal _ = wordToWord8# 0xB4##
    {-# INLINE byteVal #-}
instance ByteVal 'BB5 where
    byteVal _ = wordToWord8# 0xB5##
    {-# INLINE byteVal #-}
instance ByteVal 'BB6 where
    byteVal _ = wordToWord8# 0xB6##
    {-# INLINE byteVal #-}
instance ByteVal 'BB7 where
    byteVal _ = wordToWord8# 0xB7##
    {-# INLINE byteVal #-}
instance ByteVal 'BB8 where
    byteVal _ = wordToWord8# 0xB8##
    {-# INLINE byteVal #-}
instance ByteVal 'BB9 where
    byteVal _ = wordToWord8# 0xB9##
    {-# INLINE byteVal #-}
instance ByteVal 'BBA where
    byteVal _ = wordToWord8# 0xBA##
    {-# INLINE byteVal #-}
instance ByteVal 'BBB where
    byteVal _ = wordToWord8# 0xBB##
    {-# INLINE byteVal #-}
instance ByteVal 'BBC where
    byteVal _ = wordToWord8# 0xBC##
    {-# INLINE byteVal #-}
instance ByteVal 'BBD where
    byteVal _ = wordToWord8# 0xBD##
    {-# INLINE byteVal #-}
instance ByteVal 'BBE where
    byteVal _ = wordToWord8# 0xBE##
    {-# INLINE byteVal #-}
instance ByteVal 'BBF where
    byteVal _ = wordToWord8# 0xBF##
    {-# INLINE byteVal #-}
instance ByteVal 'BC0 where
    byteVal _ = wordToWord8# 0xC0##
    {-# INLINE byteVal #-}
instance ByteVal 'BC1 where
    byteVal _ = wordToWord8# 0xC1##
    {-# INLINE byteVal #-}
instance ByteVal 'BC2 where
    byteVal _ = wordToWord8# 0xC2##
    {-# INLINE byteVal #-}
instance ByteVal 'BC3 where
    byteVal _ = wordToWord8# 0xC3##
    {-# INLINE byteVal #-}
instance ByteVal 'BC4 where
    byteVal _ = wordToWord8# 0xC4##
    {-# INLINE byteVal #-}
instance ByteVal 'BC5 where
    byteVal _ = wordToWord8# 0xC5##
    {-# INLINE byteVal #-}
instance ByteVal 'BC6 where
    byteVal _ = wordToWord8# 0xC6##
    {-# INLINE byteVal #-}
instance ByteVal 'BC7 where
    byteVal _ = wordToWord8# 0xC7##
    {-# INLINE byteVal #-}
instance ByteVal 'BC8 where
    byteVal _ = wordToWord8# 0xC8##
    {-# INLINE byteVal #-}
instance ByteVal 'BC9 where
    byteVal _ = wordToWord8# 0xC9##
    {-# INLINE byteVal #-}
instance ByteVal 'BCA where
    byteVal _ = wordToWord8# 0xCA##
    {-# INLINE byteVal #-}
instance ByteVal 'BCB where
    byteVal _ = wordToWord8# 0xCB##
    {-# INLINE byteVal #-}
instance ByteVal 'BCC where
    byteVal _ = wordToWord8# 0xCC##
    {-# INLINE byteVal #-}
instance ByteVal 'BCD where
    byteVal _ = wordToWord8# 0xCD##
    {-# INLINE byteVal #-}
instance ByteVal 'BCE where
    byteVal _ = wordToWord8# 0xCE##
    {-# INLINE byteVal #-}
instance ByteVal 'BCF where
    byteVal _ = wordToWord8# 0xCF##
    {-# INLINE byteVal #-}
instance ByteVal 'BD0 where
    byteVal _ = wordToWord8# 0xD0##
    {-# INLINE byteVal #-}
instance ByteVal 'BD1 where
    byteVal _ = wordToWord8# 0xD1##
    {-# INLINE byteVal #-}
instance ByteVal 'BD2 where
    byteVal _ = wordToWord8# 0xD2##
    {-# INLINE byteVal #-}
instance ByteVal 'BD3 where
    byteVal _ = wordToWord8# 0xD3##
    {-# INLINE byteVal #-}
instance ByteVal 'BD4 where
    byteVal _ = wordToWord8# 0xD4##
    {-# INLINE byteVal #-}
instance ByteVal 'BD5 where
    byteVal _ = wordToWord8# 0xD5##
    {-# INLINE byteVal #-}
instance ByteVal 'BD6 where
    byteVal _ = wordToWord8# 0xD6##
    {-# INLINE byteVal #-}
instance ByteVal 'BD7 where
    byteVal _ = wordToWord8# 0xD7##
    {-# INLINE byteVal #-}
instance ByteVal 'BD8 where
    byteVal _ = wordToWord8# 0xD8##
    {-# INLINE byteVal #-}
instance ByteVal 'BD9 where
    byteVal _ = wordToWord8# 0xD9##
    {-# INLINE byteVal #-}
instance ByteVal 'BDA where
    byteVal _ = wordToWord8# 0xDA##
    {-# INLINE byteVal #-}
instance ByteVal 'BDB where
    byteVal _ = wordToWord8# 0xDB##
    {-# INLINE byteVal #-}
instance ByteVal 'BDC where
    byteVal _ = wordToWord8# 0xDC##
    {-# INLINE byteVal #-}
instance ByteVal 'BDD where
    byteVal _ = wordToWord8# 0xDD##
    {-# INLINE byteVal #-}
instance ByteVal 'BDE where
    byteVal _ = wordToWord8# 0xDE##
    {-# INLINE byteVal #-}
instance ByteVal 'BDF where
    byteVal _ = wordToWord8# 0xDF##
    {-# INLINE byteVal #-}
instance ByteVal 'BE0 where
    byteVal _ = wordToWord8# 0xE0##
    {-# INLINE byteVal #-}
instance ByteVal 'BE1 where
    byteVal _ = wordToWord8# 0xE1##
    {-# INLINE byteVal #-}
instance ByteVal 'BE2 where
    byteVal _ = wordToWord8# 0xE2##
    {-# INLINE byteVal #-}
instance ByteVal 'BE3 where
    byteVal _ = wordToWord8# 0xE3##
    {-# INLINE byteVal #-}
instance ByteVal 'BE4 where
    byteVal _ = wordToWord8# 0xE4##
    {-# INLINE byteVal #-}
instance ByteVal 'BE5 where
    byteVal _ = wordToWord8# 0xE5##
    {-# INLINE byteVal #-}
instance ByteVal 'BE6 where
    byteVal _ = wordToWord8# 0xE6##
    {-# INLINE byteVal #-}
instance ByteVal 'BE7 where
    byteVal _ = wordToWord8# 0xE7##
    {-# INLINE byteVal #-}
instance ByteVal 'BE8 where
    byteVal _ = wordToWord8# 0xE8##
    {-# INLINE byteVal #-}
instance ByteVal 'BE9 where
    byteVal _ = wordToWord8# 0xE9##
    {-# INLINE byteVal #-}
instance ByteVal 'BEA where
    byteVal _ = wordToWord8# 0xEA##
    {-# INLINE byteVal #-}
instance ByteVal 'BEB where
    byteVal _ = wordToWord8# 0xEB##
    {-# INLINE byteVal #-}
instance ByteVal 'BEC where
    byteVal _ = wordToWord8# 0xEC##
    {-# INLINE byteVal #-}
instance ByteVal 'BED where
    byteVal _ = wordToWord8# 0xED##
    {-# INLINE byteVal #-}
instance ByteVal 'BEE where
    byteVal _ = wordToWord8# 0xEE##
    {-# INLINE byteVal #-}
instance ByteVal 'BEF where
    byteVal _ = wordToWord8# 0xEF##
    {-# INLINE byteVal #-}
instance ByteVal 'BF0 where
    byteVal _ = wordToWord8# 0xF0##
    {-# INLINE byteVal #-}
instance ByteVal 'BF1 where
    byteVal _ = wordToWord8# 0xF1##
    {-# INLINE byteVal #-}
instance ByteVal 'BF2 where
    byteVal _ = wordToWord8# 0xF2##
    {-# INLINE byteVal #-}
instance ByteVal 'BF3 where
    byteVal _ = wordToWord8# 0xF3##
    {-# INLINE byteVal #-}
instance ByteVal 'BF4 where
    byteVal _ = wordToWord8# 0xF4##
    {-# INLINE byteVal #-}
instance ByteVal 'BF5 where
    byteVal _ = wordToWord8# 0xF5##
    {-# INLINE byteVal #-}
instance ByteVal 'BF6 where
    byteVal _ = wordToWord8# 0xF6##
    {-# INLINE byteVal #-}
instance ByteVal 'BF7 where
    byteVal _ = wordToWord8# 0xF7##
    {-# INLINE byteVal #-}
instance ByteVal 'BF8 where
    byteVal _ = wordToWord8# 0xF8##
    {-# INLINE byteVal #-}
instance ByteVal 'BF9 where
    byteVal _ = wordToWord8# 0xF9##
    {-# INLINE byteVal #-}
instance ByteVal 'BFA where
    byteVal _ = wordToWord8# 0xFA##
    {-# INLINE byteVal #-}
instance ByteVal 'BFB where
    byteVal _ = wordToWord8# 0xFB##
    {-# INLINE byteVal #-}
instance ByteVal 'BFC where
    byteVal _ = wordToWord8# 0xFC##
    {-# INLINE byteVal #-}
instance ByteVal 'BFD where
    byteVal _ = wordToWord8# 0xFD##
    {-# INLINE byteVal #-}
instance ByteVal 'BFE where
    byteVal _ = wordToWord8# 0xFE##
    {-# INLINE byteVal #-}
instance ByteVal 'BFF where
    byteVal _ = wordToWord8# 0xFF##
    {-# INLINE byteVal #-}

type family Length (a :: [k]) :: Natural where
    Length '[]       = 0
    Length (a ': as) = 1 + Length as

-- | Efficiently reify a list of type-level 'Byte's to a bytestring builder.
--
-- This is about as far as one should go for pointless performance here, I
-- should think.
class ByteVals (bs :: [Byte]) where byteVals :: Builder
instance (n ~ Length bs, KnownNat n, WriteByteVals bs) => ByteVals bs where
    byteVals = Mason.primFixed (BI.fixedPrim (fromIntegral n) go) ()
      where
        n = natVal'' @n
        go = \() (Ptr p#) -> writeByteVals @bs p#

class WriteByteVals (bs :: [Byte]) where writeByteVals :: Addr# -> IO ()
instance WriteByteVals '[] where writeByteVals _ = pure ()
instance (ByteVal b, WriteByteVals bs) => WriteByteVals (b ': bs) where
    writeByteVals p# =
        case runRW# (writeWord8OffAddr# p# 0# w#) of
          _ -> writeByteVals @bs (plusAddr# p# 1#)
      where w# = byteVal @b proxy#
