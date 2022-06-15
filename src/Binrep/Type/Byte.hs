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

import Mason.Builder qualified as Mason
import Data.ByteString.Builder.Prim.Internal qualified as BI
import Binrep.Util ( natVal'' )
import Binrep.Put ( Builder )
import GHC.TypeNats
import GHC.Exts

-- Needs to be a function type to work. Interesting. It's perhaps not an
-- improvement on regular boxed. But interesting idea, so sticking with it.
class ByteVal (n :: Natural) where byteVal :: Proxy# n -> Word8#

instance ByteVal 0x00 where
    byteVal _ = wordToWord8# 0x00##
    {-# INLINE byteVal #-}
instance ByteVal 0x01 where
    byteVal _ = wordToWord8# 0x01##
    {-# INLINE byteVal #-}
instance ByteVal 0x02 where
    byteVal _ = wordToWord8# 0x02##
    {-# INLINE byteVal #-}
instance ByteVal 0x03 where
    byteVal _ = wordToWord8# 0x03##
    {-# INLINE byteVal #-}
instance ByteVal 0x04 where
    byteVal _ = wordToWord8# 0x04##
    {-# INLINE byteVal #-}
instance ByteVal 0x05 where
    byteVal _ = wordToWord8# 0x05##
    {-# INLINE byteVal #-}
instance ByteVal 0x06 where
    byteVal _ = wordToWord8# 0x06##
    {-# INLINE byteVal #-}
instance ByteVal 0x07 where
    byteVal _ = wordToWord8# 0x07##
    {-# INLINE byteVal #-}
instance ByteVal 0x08 where
    byteVal _ = wordToWord8# 0x08##
    {-# INLINE byteVal #-}
instance ByteVal 0x09 where
    byteVal _ = wordToWord8# 0x09##
    {-# INLINE byteVal #-}
instance ByteVal 0x0a where
    byteVal _ = wordToWord8# 0x0a##
    {-# INLINE byteVal #-}
instance ByteVal 0x0b where
    byteVal _ = wordToWord8# 0x0b##
    {-# INLINE byteVal #-}
instance ByteVal 0x0c where
    byteVal _ = wordToWord8# 0x0c##
    {-# INLINE byteVal #-}
instance ByteVal 0x0d where
    byteVal _ = wordToWord8# 0x0d##
    {-# INLINE byteVal #-}
instance ByteVal 0x0e where
    byteVal _ = wordToWord8# 0x0e##
    {-# INLINE byteVal #-}
instance ByteVal 0x0f where
    byteVal _ = wordToWord8# 0x0f##
    {-# INLINE byteVal #-}
instance ByteVal 0x10 where
    byteVal _ = wordToWord8# 0x10##
    {-# INLINE byteVal #-}
instance ByteVal 0x11 where
    byteVal _ = wordToWord8# 0x11##
    {-# INLINE byteVal #-}
instance ByteVal 0x12 where
    byteVal _ = wordToWord8# 0x12##
    {-# INLINE byteVal #-}
instance ByteVal 0x13 where
    byteVal _ = wordToWord8# 0x13##
    {-# INLINE byteVal #-}
instance ByteVal 0x14 where
    byteVal _ = wordToWord8# 0x14##
    {-# INLINE byteVal #-}
instance ByteVal 0x15 where
    byteVal _ = wordToWord8# 0x15##
    {-# INLINE byteVal #-}
instance ByteVal 0x16 where
    byteVal _ = wordToWord8# 0x16##
    {-# INLINE byteVal #-}
instance ByteVal 0x17 where
    byteVal _ = wordToWord8# 0x17##
    {-# INLINE byteVal #-}
instance ByteVal 0x18 where
    byteVal _ = wordToWord8# 0x18##
    {-# INLINE byteVal #-}
instance ByteVal 0x19 where
    byteVal _ = wordToWord8# 0x19##
    {-# INLINE byteVal #-}
instance ByteVal 0x1a where
    byteVal _ = wordToWord8# 0x1a##
    {-# INLINE byteVal #-}
instance ByteVal 0x1b where
    byteVal _ = wordToWord8# 0x1b##
    {-# INLINE byteVal #-}
instance ByteVal 0x1c where
    byteVal _ = wordToWord8# 0x1c##
    {-# INLINE byteVal #-}
instance ByteVal 0x1d where
    byteVal _ = wordToWord8# 0x1d##
    {-# INLINE byteVal #-}
instance ByteVal 0x1e where
    byteVal _ = wordToWord8# 0x1e##
    {-# INLINE byteVal #-}
instance ByteVal 0x1f where
    byteVal _ = wordToWord8# 0x1f##
    {-# INLINE byteVal #-}
instance ByteVal 0x20 where
    byteVal _ = wordToWord8# 0x20##
    {-# INLINE byteVal #-}
instance ByteVal 0x21 where
    byteVal _ = wordToWord8# 0x21##
    {-# INLINE byteVal #-}
instance ByteVal 0x22 where
    byteVal _ = wordToWord8# 0x22##
    {-# INLINE byteVal #-}
instance ByteVal 0x23 where
    byteVal _ = wordToWord8# 0x23##
    {-# INLINE byteVal #-}
instance ByteVal 0x24 where
    byteVal _ = wordToWord8# 0x24##
    {-# INLINE byteVal #-}
instance ByteVal 0x25 where
    byteVal _ = wordToWord8# 0x25##
    {-# INLINE byteVal #-}
instance ByteVal 0x26 where
    byteVal _ = wordToWord8# 0x26##
    {-# INLINE byteVal #-}
instance ByteVal 0x27 where
    byteVal _ = wordToWord8# 0x27##
    {-# INLINE byteVal #-}
instance ByteVal 0x28 where
    byteVal _ = wordToWord8# 0x28##
    {-# INLINE byteVal #-}
instance ByteVal 0x29 where
    byteVal _ = wordToWord8# 0x29##
    {-# INLINE byteVal #-}
instance ByteVal 0x2a where
    byteVal _ = wordToWord8# 0x2a##
    {-# INLINE byteVal #-}
instance ByteVal 0x2b where
    byteVal _ = wordToWord8# 0x2b##
    {-# INLINE byteVal #-}
instance ByteVal 0x2c where
    byteVal _ = wordToWord8# 0x2c##
    {-# INLINE byteVal #-}
instance ByteVal 0x2d where
    byteVal _ = wordToWord8# 0x2d##
    {-# INLINE byteVal #-}
instance ByteVal 0x2e where
    byteVal _ = wordToWord8# 0x2e##
    {-# INLINE byteVal #-}
instance ByteVal 0x2f where
    byteVal _ = wordToWord8# 0x2f##
    {-# INLINE byteVal #-}
instance ByteVal 0x30 where
    byteVal _ = wordToWord8# 0x30##
    {-# INLINE byteVal #-}
instance ByteVal 0x31 where
    byteVal _ = wordToWord8# 0x31##
    {-# INLINE byteVal #-}
instance ByteVal 0x32 where
    byteVal _ = wordToWord8# 0x32##
    {-# INLINE byteVal #-}
instance ByteVal 0x33 where
    byteVal _ = wordToWord8# 0x33##
    {-# INLINE byteVal #-}
instance ByteVal 0x34 where
    byteVal _ = wordToWord8# 0x34##
    {-# INLINE byteVal #-}
instance ByteVal 0x35 where
    byteVal _ = wordToWord8# 0x35##
    {-# INLINE byteVal #-}
instance ByteVal 0x36 where
    byteVal _ = wordToWord8# 0x36##
    {-# INLINE byteVal #-}
instance ByteVal 0x37 where
    byteVal _ = wordToWord8# 0x37##
    {-# INLINE byteVal #-}
instance ByteVal 0x38 where
    byteVal _ = wordToWord8# 0x38##
    {-# INLINE byteVal #-}
instance ByteVal 0x39 where
    byteVal _ = wordToWord8# 0x39##
    {-# INLINE byteVal #-}
instance ByteVal 0x3a where
    byteVal _ = wordToWord8# 0x3a##
    {-# INLINE byteVal #-}
instance ByteVal 0x3b where
    byteVal _ = wordToWord8# 0x3b##
    {-# INLINE byteVal #-}
instance ByteVal 0x3c where
    byteVal _ = wordToWord8# 0x3c##
    {-# INLINE byteVal #-}
instance ByteVal 0x3d where
    byteVal _ = wordToWord8# 0x3d##
    {-# INLINE byteVal #-}
instance ByteVal 0x3e where
    byteVal _ = wordToWord8# 0x3e##
    {-# INLINE byteVal #-}
instance ByteVal 0x3f where
    byteVal _ = wordToWord8# 0x3f##
    {-# INLINE byteVal #-}
instance ByteVal 0x40 where
    byteVal _ = wordToWord8# 0x40##
    {-# INLINE byteVal #-}
instance ByteVal 0x41 where
    byteVal _ = wordToWord8# 0x41##
    {-# INLINE byteVal #-}
instance ByteVal 0x42 where
    byteVal _ = wordToWord8# 0x42##
    {-# INLINE byteVal #-}
instance ByteVal 0x43 where
    byteVal _ = wordToWord8# 0x43##
    {-# INLINE byteVal #-}
instance ByteVal 0x44 where
    byteVal _ = wordToWord8# 0x44##
    {-# INLINE byteVal #-}
instance ByteVal 0x45 where
    byteVal _ = wordToWord8# 0x45##
    {-# INLINE byteVal #-}
instance ByteVal 0x46 where
    byteVal _ = wordToWord8# 0x46##
    {-# INLINE byteVal #-}
instance ByteVal 0x47 where
    byteVal _ = wordToWord8# 0x47##
    {-# INLINE byteVal #-}
instance ByteVal 0x48 where
    byteVal _ = wordToWord8# 0x48##
    {-# INLINE byteVal #-}
instance ByteVal 0x49 where
    byteVal _ = wordToWord8# 0x49##
    {-# INLINE byteVal #-}
instance ByteVal 0x4a where
    byteVal _ = wordToWord8# 0x4a##
    {-# INLINE byteVal #-}
instance ByteVal 0x4b where
    byteVal _ = wordToWord8# 0x4b##
    {-# INLINE byteVal #-}
instance ByteVal 0x4c where
    byteVal _ = wordToWord8# 0x4c##
    {-# INLINE byteVal #-}
instance ByteVal 0x4d where
    byteVal _ = wordToWord8# 0x4d##
    {-# INLINE byteVal #-}
instance ByteVal 0x4e where
    byteVal _ = wordToWord8# 0x4e##
    {-# INLINE byteVal #-}
instance ByteVal 0x4f where
    byteVal _ = wordToWord8# 0x4f##
    {-# INLINE byteVal #-}
instance ByteVal 0x50 where
    byteVal _ = wordToWord8# 0x50##
    {-# INLINE byteVal #-}
instance ByteVal 0x51 where
    byteVal _ = wordToWord8# 0x51##
    {-# INLINE byteVal #-}
instance ByteVal 0x52 where
    byteVal _ = wordToWord8# 0x52##
    {-# INLINE byteVal #-}
instance ByteVal 0x53 where
    byteVal _ = wordToWord8# 0x53##
    {-# INLINE byteVal #-}
instance ByteVal 0x54 where
    byteVal _ = wordToWord8# 0x54##
    {-# INLINE byteVal #-}
instance ByteVal 0x55 where
    byteVal _ = wordToWord8# 0x55##
    {-# INLINE byteVal #-}
instance ByteVal 0x56 where
    byteVal _ = wordToWord8# 0x56##
    {-# INLINE byteVal #-}
instance ByteVal 0x57 where
    byteVal _ = wordToWord8# 0x57##
    {-# INLINE byteVal #-}
instance ByteVal 0x58 where
    byteVal _ = wordToWord8# 0x58##
    {-# INLINE byteVal #-}
instance ByteVal 0x59 where
    byteVal _ = wordToWord8# 0x59##
    {-# INLINE byteVal #-}
instance ByteVal 0x5a where
    byteVal _ = wordToWord8# 0x5a##
    {-# INLINE byteVal #-}
instance ByteVal 0x5b where
    byteVal _ = wordToWord8# 0x5b##
    {-# INLINE byteVal #-}
instance ByteVal 0x5c where
    byteVal _ = wordToWord8# 0x5c##
    {-# INLINE byteVal #-}
instance ByteVal 0x5d where
    byteVal _ = wordToWord8# 0x5d##
    {-# INLINE byteVal #-}
instance ByteVal 0x5e where
    byteVal _ = wordToWord8# 0x5e##
    {-# INLINE byteVal #-}
instance ByteVal 0x5f where
    byteVal _ = wordToWord8# 0x5f##
    {-# INLINE byteVal #-}
instance ByteVal 0x60 where
    byteVal _ = wordToWord8# 0x60##
    {-# INLINE byteVal #-}
instance ByteVal 0x61 where
    byteVal _ = wordToWord8# 0x61##
    {-# INLINE byteVal #-}
instance ByteVal 0x62 where
    byteVal _ = wordToWord8# 0x62##
    {-# INLINE byteVal #-}
instance ByteVal 0x63 where
    byteVal _ = wordToWord8# 0x63##
    {-# INLINE byteVal #-}
instance ByteVal 0x64 where
    byteVal _ = wordToWord8# 0x64##
    {-# INLINE byteVal #-}
instance ByteVal 0x65 where
    byteVal _ = wordToWord8# 0x65##
    {-# INLINE byteVal #-}
instance ByteVal 0x66 where
    byteVal _ = wordToWord8# 0x66##
    {-# INLINE byteVal #-}
instance ByteVal 0x67 where
    byteVal _ = wordToWord8# 0x67##
    {-# INLINE byteVal #-}
instance ByteVal 0x68 where
    byteVal _ = wordToWord8# 0x68##
    {-# INLINE byteVal #-}
instance ByteVal 0x69 where
    byteVal _ = wordToWord8# 0x69##
    {-# INLINE byteVal #-}
instance ByteVal 0x6a where
    byteVal _ = wordToWord8# 0x6a##
    {-# INLINE byteVal #-}
instance ByteVal 0x6b where
    byteVal _ = wordToWord8# 0x6b##
    {-# INLINE byteVal #-}
instance ByteVal 0x6c where
    byteVal _ = wordToWord8# 0x6c##
    {-# INLINE byteVal #-}
instance ByteVal 0x6d where
    byteVal _ = wordToWord8# 0x6d##
    {-# INLINE byteVal #-}
instance ByteVal 0x6e where
    byteVal _ = wordToWord8# 0x6e##
    {-# INLINE byteVal #-}
instance ByteVal 0x6f where
    byteVal _ = wordToWord8# 0x6f##
    {-# INLINE byteVal #-}
instance ByteVal 0x70 where
    byteVal _ = wordToWord8# 0x70##
    {-# INLINE byteVal #-}
instance ByteVal 0x71 where
    byteVal _ = wordToWord8# 0x71##
    {-# INLINE byteVal #-}
instance ByteVal 0x72 where
    byteVal _ = wordToWord8# 0x72##
    {-# INLINE byteVal #-}
instance ByteVal 0x73 where
    byteVal _ = wordToWord8# 0x73##
    {-# INLINE byteVal #-}
instance ByteVal 0x74 where
    byteVal _ = wordToWord8# 0x74##
    {-# INLINE byteVal #-}
instance ByteVal 0x75 where
    byteVal _ = wordToWord8# 0x75##
    {-# INLINE byteVal #-}
instance ByteVal 0x76 where
    byteVal _ = wordToWord8# 0x76##
    {-# INLINE byteVal #-}
instance ByteVal 0x77 where
    byteVal _ = wordToWord8# 0x77##
    {-# INLINE byteVal #-}
instance ByteVal 0x78 where
    byteVal _ = wordToWord8# 0x78##
    {-# INLINE byteVal #-}
instance ByteVal 0x79 where
    byteVal _ = wordToWord8# 0x79##
    {-# INLINE byteVal #-}
instance ByteVal 0x7a where
    byteVal _ = wordToWord8# 0x7a##
    {-# INLINE byteVal #-}
instance ByteVal 0x7b where
    byteVal _ = wordToWord8# 0x7b##
    {-# INLINE byteVal #-}
instance ByteVal 0x7c where
    byteVal _ = wordToWord8# 0x7c##
    {-# INLINE byteVal #-}
instance ByteVal 0x7d where
    byteVal _ = wordToWord8# 0x7d##
    {-# INLINE byteVal #-}
instance ByteVal 0x7e where
    byteVal _ = wordToWord8# 0x7e##
    {-# INLINE byteVal #-}
instance ByteVal 0x7f where
    byteVal _ = wordToWord8# 0x7f##
    {-# INLINE byteVal #-}
instance ByteVal 0x80 where
    byteVal _ = wordToWord8# 0x80##
    {-# INLINE byteVal #-}
instance ByteVal 0x81 where
    byteVal _ = wordToWord8# 0x81##
    {-# INLINE byteVal #-}
instance ByteVal 0x82 where
    byteVal _ = wordToWord8# 0x82##
    {-# INLINE byteVal #-}
instance ByteVal 0x83 where
    byteVal _ = wordToWord8# 0x83##
    {-# INLINE byteVal #-}
instance ByteVal 0x84 where
    byteVal _ = wordToWord8# 0x84##
    {-# INLINE byteVal #-}
instance ByteVal 0x85 where
    byteVal _ = wordToWord8# 0x85##
    {-# INLINE byteVal #-}
instance ByteVal 0x86 where
    byteVal _ = wordToWord8# 0x86##
    {-# INLINE byteVal #-}
instance ByteVal 0x87 where
    byteVal _ = wordToWord8# 0x87##
    {-# INLINE byteVal #-}
instance ByteVal 0x88 where
    byteVal _ = wordToWord8# 0x88##
    {-# INLINE byteVal #-}
instance ByteVal 0x89 where
    byteVal _ = wordToWord8# 0x89##
    {-# INLINE byteVal #-}
instance ByteVal 0x8a where
    byteVal _ = wordToWord8# 0x8a##
    {-# INLINE byteVal #-}
instance ByteVal 0x8b where
    byteVal _ = wordToWord8# 0x8b##
    {-# INLINE byteVal #-}
instance ByteVal 0x8c where
    byteVal _ = wordToWord8# 0x8c##
    {-# INLINE byteVal #-}
instance ByteVal 0x8d where
    byteVal _ = wordToWord8# 0x8d##
    {-# INLINE byteVal #-}
instance ByteVal 0x8e where
    byteVal _ = wordToWord8# 0x8e##
    {-# INLINE byteVal #-}
instance ByteVal 0x8f where
    byteVal _ = wordToWord8# 0x8f##
    {-# INLINE byteVal #-}
instance ByteVal 0x90 where
    byteVal _ = wordToWord8# 0x90##
    {-# INLINE byteVal #-}
instance ByteVal 0x91 where
    byteVal _ = wordToWord8# 0x91##
    {-# INLINE byteVal #-}
instance ByteVal 0x92 where
    byteVal _ = wordToWord8# 0x92##
    {-# INLINE byteVal #-}
instance ByteVal 0x93 where
    byteVal _ = wordToWord8# 0x93##
    {-# INLINE byteVal #-}
instance ByteVal 0x94 where
    byteVal _ = wordToWord8# 0x94##
    {-# INLINE byteVal #-}
instance ByteVal 0x95 where
    byteVal _ = wordToWord8# 0x95##
    {-# INLINE byteVal #-}
instance ByteVal 0x96 where
    byteVal _ = wordToWord8# 0x96##
    {-# INLINE byteVal #-}
instance ByteVal 0x97 where
    byteVal _ = wordToWord8# 0x97##
    {-# INLINE byteVal #-}
instance ByteVal 0x98 where
    byteVal _ = wordToWord8# 0x98##
    {-# INLINE byteVal #-}
instance ByteVal 0x99 where
    byteVal _ = wordToWord8# 0x99##
    {-# INLINE byteVal #-}
instance ByteVal 0x9a where
    byteVal _ = wordToWord8# 0x9a##
    {-# INLINE byteVal #-}
instance ByteVal 0x9b where
    byteVal _ = wordToWord8# 0x9b##
    {-# INLINE byteVal #-}
instance ByteVal 0x9c where
    byteVal _ = wordToWord8# 0x9c##
    {-# INLINE byteVal #-}
instance ByteVal 0x9d where
    byteVal _ = wordToWord8# 0x9d##
    {-# INLINE byteVal #-}
instance ByteVal 0x9e where
    byteVal _ = wordToWord8# 0x9e##
    {-# INLINE byteVal #-}
instance ByteVal 0x9f where
    byteVal _ = wordToWord8# 0x9f##
    {-# INLINE byteVal #-}
instance ByteVal 0xa0 where
    byteVal _ = wordToWord8# 0xa0##
    {-# INLINE byteVal #-}
instance ByteVal 0xa1 where
    byteVal _ = wordToWord8# 0xa1##
    {-# INLINE byteVal #-}
instance ByteVal 0xa2 where
    byteVal _ = wordToWord8# 0xa2##
    {-# INLINE byteVal #-}
instance ByteVal 0xa3 where
    byteVal _ = wordToWord8# 0xa3##
    {-# INLINE byteVal #-}
instance ByteVal 0xa4 where
    byteVal _ = wordToWord8# 0xa4##
    {-# INLINE byteVal #-}
instance ByteVal 0xa5 where
    byteVal _ = wordToWord8# 0xa5##
    {-# INLINE byteVal #-}
instance ByteVal 0xa6 where
    byteVal _ = wordToWord8# 0xa6##
    {-# INLINE byteVal #-}
instance ByteVal 0xa7 where
    byteVal _ = wordToWord8# 0xa7##
    {-# INLINE byteVal #-}
instance ByteVal 0xa8 where
    byteVal _ = wordToWord8# 0xa8##
    {-# INLINE byteVal #-}
instance ByteVal 0xa9 where
    byteVal _ = wordToWord8# 0xa9##
    {-# INLINE byteVal #-}
instance ByteVal 0xaa where
    byteVal _ = wordToWord8# 0xaa##
    {-# INLINE byteVal #-}
instance ByteVal 0xab where
    byteVal _ = wordToWord8# 0xab##
    {-# INLINE byteVal #-}
instance ByteVal 0xac where
    byteVal _ = wordToWord8# 0xac##
    {-# INLINE byteVal #-}
instance ByteVal 0xad where
    byteVal _ = wordToWord8# 0xad##
    {-# INLINE byteVal #-}
instance ByteVal 0xae where
    byteVal _ = wordToWord8# 0xae##
    {-# INLINE byteVal #-}
instance ByteVal 0xaf where
    byteVal _ = wordToWord8# 0xaf##
    {-# INLINE byteVal #-}
instance ByteVal 0xb0 where
    byteVal _ = wordToWord8# 0xb0##
    {-# INLINE byteVal #-}
instance ByteVal 0xb1 where
    byteVal _ = wordToWord8# 0xb1##
    {-# INLINE byteVal #-}
instance ByteVal 0xb2 where
    byteVal _ = wordToWord8# 0xb2##
    {-# INLINE byteVal #-}
instance ByteVal 0xb3 where
    byteVal _ = wordToWord8# 0xb3##
    {-# INLINE byteVal #-}
instance ByteVal 0xb4 where
    byteVal _ = wordToWord8# 0xb4##
    {-# INLINE byteVal #-}
instance ByteVal 0xb5 where
    byteVal _ = wordToWord8# 0xb5##
    {-# INLINE byteVal #-}
instance ByteVal 0xb6 where
    byteVal _ = wordToWord8# 0xb6##
    {-# INLINE byteVal #-}
instance ByteVal 0xb7 where
    byteVal _ = wordToWord8# 0xb7##
    {-# INLINE byteVal #-}
instance ByteVal 0xb8 where
    byteVal _ = wordToWord8# 0xb8##
    {-# INLINE byteVal #-}
instance ByteVal 0xb9 where
    byteVal _ = wordToWord8# 0xb9##
    {-# INLINE byteVal #-}
instance ByteVal 0xba where
    byteVal _ = wordToWord8# 0xba##
    {-# INLINE byteVal #-}
instance ByteVal 0xbb where
    byteVal _ = wordToWord8# 0xbb##
    {-# INLINE byteVal #-}
instance ByteVal 0xbc where
    byteVal _ = wordToWord8# 0xbc##
    {-# INLINE byteVal #-}
instance ByteVal 0xbd where
    byteVal _ = wordToWord8# 0xbd##
    {-# INLINE byteVal #-}
instance ByteVal 0xbe where
    byteVal _ = wordToWord8# 0xbe##
    {-# INLINE byteVal #-}
instance ByteVal 0xbf where
    byteVal _ = wordToWord8# 0xbf##
    {-# INLINE byteVal #-}
instance ByteVal 0xc0 where
    byteVal _ = wordToWord8# 0xc0##
    {-# INLINE byteVal #-}
instance ByteVal 0xc1 where
    byteVal _ = wordToWord8# 0xc1##
    {-# INLINE byteVal #-}
instance ByteVal 0xc2 where
    byteVal _ = wordToWord8# 0xc2##
    {-# INLINE byteVal #-}
instance ByteVal 0xc3 where
    byteVal _ = wordToWord8# 0xc3##
    {-# INLINE byteVal #-}
instance ByteVal 0xc4 where
    byteVal _ = wordToWord8# 0xc4##
    {-# INLINE byteVal #-}
instance ByteVal 0xc5 where
    byteVal _ = wordToWord8# 0xc5##
    {-# INLINE byteVal #-}
instance ByteVal 0xc6 where
    byteVal _ = wordToWord8# 0xc6##
    {-# INLINE byteVal #-}
instance ByteVal 0xc7 where
    byteVal _ = wordToWord8# 0xc7##
    {-# INLINE byteVal #-}
instance ByteVal 0xc8 where
    byteVal _ = wordToWord8# 0xc8##
    {-# INLINE byteVal #-}
instance ByteVal 0xc9 where
    byteVal _ = wordToWord8# 0xc9##
    {-# INLINE byteVal #-}
instance ByteVal 0xca where
    byteVal _ = wordToWord8# 0xca##
    {-# INLINE byteVal #-}
instance ByteVal 0xcb where
    byteVal _ = wordToWord8# 0xcb##
    {-# INLINE byteVal #-}
instance ByteVal 0xcc where
    byteVal _ = wordToWord8# 0xcc##
    {-# INLINE byteVal #-}
instance ByteVal 0xcd where
    byteVal _ = wordToWord8# 0xcd##
    {-# INLINE byteVal #-}
instance ByteVal 0xce where
    byteVal _ = wordToWord8# 0xce##
    {-# INLINE byteVal #-}
instance ByteVal 0xcf where
    byteVal _ = wordToWord8# 0xcf##
    {-# INLINE byteVal #-}
instance ByteVal 0xd0 where
    byteVal _ = wordToWord8# 0xd0##
    {-# INLINE byteVal #-}
instance ByteVal 0xd1 where
    byteVal _ = wordToWord8# 0xd1##
    {-# INLINE byteVal #-}
instance ByteVal 0xd2 where
    byteVal _ = wordToWord8# 0xd2##
    {-# INLINE byteVal #-}
instance ByteVal 0xd3 where
    byteVal _ = wordToWord8# 0xd3##
    {-# INLINE byteVal #-}
instance ByteVal 0xd4 where
    byteVal _ = wordToWord8# 0xd4##
    {-# INLINE byteVal #-}
instance ByteVal 0xd5 where
    byteVal _ = wordToWord8# 0xd5##
    {-# INLINE byteVal #-}
instance ByteVal 0xd6 where
    byteVal _ = wordToWord8# 0xd6##
    {-# INLINE byteVal #-}
instance ByteVal 0xd7 where
    byteVal _ = wordToWord8# 0xd7##
    {-# INLINE byteVal #-}
instance ByteVal 0xd8 where
    byteVal _ = wordToWord8# 0xd8##
    {-# INLINE byteVal #-}
instance ByteVal 0xd9 where
    byteVal _ = wordToWord8# 0xd9##
    {-# INLINE byteVal #-}
instance ByteVal 0xda where
    byteVal _ = wordToWord8# 0xda##
    {-# INLINE byteVal #-}
instance ByteVal 0xdb where
    byteVal _ = wordToWord8# 0xdb##
    {-# INLINE byteVal #-}
instance ByteVal 0xdc where
    byteVal _ = wordToWord8# 0xdc##
    {-# INLINE byteVal #-}
instance ByteVal 0xdd where
    byteVal _ = wordToWord8# 0xdd##
    {-# INLINE byteVal #-}
instance ByteVal 0xde where
    byteVal _ = wordToWord8# 0xde##
    {-# INLINE byteVal #-}
instance ByteVal 0xdf where
    byteVal _ = wordToWord8# 0xdf##
    {-# INLINE byteVal #-}
instance ByteVal 0xe0 where
    byteVal _ = wordToWord8# 0xe0##
    {-# INLINE byteVal #-}
instance ByteVal 0xe1 where
    byteVal _ = wordToWord8# 0xe1##
    {-# INLINE byteVal #-}
instance ByteVal 0xe2 where
    byteVal _ = wordToWord8# 0xe2##
    {-# INLINE byteVal #-}
instance ByteVal 0xe3 where
    byteVal _ = wordToWord8# 0xe3##
    {-# INLINE byteVal #-}
instance ByteVal 0xe4 where
    byteVal _ = wordToWord8# 0xe4##
    {-# INLINE byteVal #-}
instance ByteVal 0xe5 where
    byteVal _ = wordToWord8# 0xe5##
    {-# INLINE byteVal #-}
instance ByteVal 0xe6 where
    byteVal _ = wordToWord8# 0xe6##
    {-# INLINE byteVal #-}
instance ByteVal 0xe7 where
    byteVal _ = wordToWord8# 0xe7##
    {-# INLINE byteVal #-}
instance ByteVal 0xe8 where
    byteVal _ = wordToWord8# 0xe8##
    {-# INLINE byteVal #-}
instance ByteVal 0xe9 where
    byteVal _ = wordToWord8# 0xe9##
    {-# INLINE byteVal #-}
instance ByteVal 0xea where
    byteVal _ = wordToWord8# 0xea##
    {-# INLINE byteVal #-}
instance ByteVal 0xeb where
    byteVal _ = wordToWord8# 0xeb##
    {-# INLINE byteVal #-}
instance ByteVal 0xec where
    byteVal _ = wordToWord8# 0xec##
    {-# INLINE byteVal #-}
instance ByteVal 0xed where
    byteVal _ = wordToWord8# 0xed##
    {-# INLINE byteVal #-}
instance ByteVal 0xee where
    byteVal _ = wordToWord8# 0xee##
    {-# INLINE byteVal #-}
instance ByteVal 0xef where
    byteVal _ = wordToWord8# 0xef##
    {-# INLINE byteVal #-}
instance ByteVal 0xf0 where
    byteVal _ = wordToWord8# 0xf0##
    {-# INLINE byteVal #-}
instance ByteVal 0xf1 where
    byteVal _ = wordToWord8# 0xf1##
    {-# INLINE byteVal #-}
instance ByteVal 0xf2 where
    byteVal _ = wordToWord8# 0xf2##
    {-# INLINE byteVal #-}
instance ByteVal 0xf3 where
    byteVal _ = wordToWord8# 0xf3##
    {-# INLINE byteVal #-}
instance ByteVal 0xf4 where
    byteVal _ = wordToWord8# 0xf4##
    {-# INLINE byteVal #-}
instance ByteVal 0xf5 where
    byteVal _ = wordToWord8# 0xf5##
    {-# INLINE byteVal #-}
instance ByteVal 0xf6 where
    byteVal _ = wordToWord8# 0xf6##
    {-# INLINE byteVal #-}
instance ByteVal 0xf7 where
    byteVal _ = wordToWord8# 0xf7##
    {-# INLINE byteVal #-}
instance ByteVal 0xf8 where
    byteVal _ = wordToWord8# 0xf8##
    {-# INLINE byteVal #-}
instance ByteVal 0xf9 where
    byteVal _ = wordToWord8# 0xf9##
    {-# INLINE byteVal #-}
instance ByteVal 0xfa where
    byteVal _ = wordToWord8# 0xfa##
    {-# INLINE byteVal #-}
instance ByteVal 0xfb where
    byteVal _ = wordToWord8# 0xfb##
    {-# INLINE byteVal #-}
instance ByteVal 0xfc where
    byteVal _ = wordToWord8# 0xfc##
    {-# INLINE byteVal #-}
instance ByteVal 0xfd where
    byteVal _ = wordToWord8# 0xfd##
    {-# INLINE byteVal #-}
instance ByteVal 0xfe where
    byteVal _ = wordToWord8# 0xfe##
    {-# INLINE byteVal #-}
instance ByteVal 0xff where
    byteVal _ = wordToWord8# 0xff##
    {-# INLINE byteVal #-}

type family Length (a :: [k]) :: Natural where
    Length '[]       = 0
    Length (a ': as) = 1 + Length as

-- | Efficiently reify a list of type-level 'Byte's to a bytestring builder.
--
-- This is about as far as one should go for pointless performance here, I
-- should think.
class ByteVals (ns :: [Natural]) where byteVals :: Builder
instance (n ~ Length ns, KnownNat n, WriteByteVals ns) => ByteVals ns where
    byteVals = Mason.primFixed (BI.fixedPrim (fromIntegral n) go) ()
      where
        n = natVal'' @n
        go = \() (Ptr p#) -> writeByteVals @ns p#

class WriteByteVals (ns :: [Natural]) where writeByteVals :: Addr# -> IO ()
instance WriteByteVals '[] where writeByteVals _ = pure ()
instance (ByteVal n, WriteByteVals ns) => WriteByteVals (n ': ns) where
    writeByteVals p# =
        case runRW# (writeWord8OffAddr# p# 0# w#) of
          _ -> writeByteVals @ns (plusAddr# p# 1#)
      where w# = byteVal @n proxy#
