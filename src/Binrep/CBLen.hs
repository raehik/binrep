module Binrep.CBLen where

import GHC.TypeNats
import Data.Word
import Data.Int

-- | The length in bytes of a value of the given type is constant.
--
-- This is "generically" derivable if you can prove that each constructor in a
-- sum type has the _same_ constant size. The rest is easier, like proving each
-- field in a record has a constant size.
--
-- I'm not sure how that's done, and I don't think you should be creating new
-- instances for this type family very often, so, exercise for the reader :)
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
