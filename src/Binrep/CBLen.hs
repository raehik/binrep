module Binrep.CBLen where

import Data.Word
import Data.Int
import GHC.TypeNats ( Natural )

-- | The length in bytes of any value of the given type is constant.
--
-- Many binary representation primitives are constant, or store their size in
-- their type. This is a stronger statement about their length than @BLen@.
type family CBLen a :: Natural

-- Explicitly-sized Haskell machine words are constant size.
type instance CBLen Word8  = 1
type instance CBLen  Int8  = 1
type instance CBLen Word16 = 2
type instance CBLen  Int16 = 2
type instance CBLen Word32 = 4
type instance CBLen  Int32 = 4
type instance CBLen Word64 = 8
type instance CBLen  Int64 = 8
