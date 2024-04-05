{-# LANGUAGE UndecidableInstances #-} -- for convenient type level arithmetic

module Binrep.Util.Prefix where

import Binrep.Util.ByteOrder ( ByteOrdered(ByteOrdered) )
import GHC.TypeNats
import Data.Word

import Data.Kind

-- | Types which may be used as prefixes.
--
-- Generally, these will be integer types.
--
-- Note that this is separate to binary representation, so endianness is
-- irrelevant.
--
-- Note that we are also limited by the host architecture's 'Int' type.
-- We don't try to work around this, because most types are indexed with 'Int's,
-- so I think other things will break before we do.
class Prefix a where
    type Max a :: Natural

    -- | used by put. guaranteed that it fits from refined. that is, lenToPfx <=
    --   Max.
    lenToPfx :: Int -> a

    -- | used by get. better not lie.
    pfxToLen :: a -> Int

-- | Length prefixing with the unit means a length of 0.
--
-- This is the only sensible case. 1 doesn't work because refining checks @<=@.
--
-- I think there are laws here, where using this is the same as doing nothing at
-- all.
instance Prefix () where
    type Max () = 0
    lenToPfx = \case
      0 -> ()
      _ -> error "you lied to refine and broke everything :("
    pfxToLen () = 0

-- | Byte ordering doesn't change how prefixes work.
deriving via (a :: Type) instance Prefix a => Prefix (ByteOrdered end a)

instance Prefix Word8  where
    type Max Word8  = 2^8  - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
instance Prefix Word16 where
    type Max Word16 = 2^16 - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
instance Prefix Word32 where
    type Max Word32 = 2^32 - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
instance Prefix Word64 where
    type Max Word64 = 2^64 - 1
    lenToPfx = fromIntegral
    pfxToLen = fromIntegral
