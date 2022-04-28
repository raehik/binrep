{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.CBLen where

import GHC.TypeNats
import GHC.Exts ( proxy#, Proxy# )
import Data.Word
import Data.Int

-- | The length in bytes of a value of the given type is constant.
type family CBLen a :: Natural

-- | Reflect a type's constant byte length to the term level.
cblen :: forall a. KnownNat (CBLen a) => Natural
cblen = natVal' (proxy# :: Proxy# (CBLen a))

-- Explicitly-sized Haskell machine words are constant size.
type instance CBLen Word8  = 1
type instance CBLen  Int8  = 1
type instance CBLen Word16 = 2
type instance CBLen  Int16 = 2
type instance CBLen Word32 = 4
type instance CBLen  Int32 = 4
type instance CBLen Word64 = 8
type instance CBLen  Int64 = 8
