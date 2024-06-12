{-# LANGUAGE UndecidableInstances #-} -- for convenient type level arithmetic

module Binrep.Type.Prefix.Internal where

import Binrep.Util.ByteOrder ( ByteOrdered(ByteOrdered) )
import GHC.TypeNats
import GHC.TypeLits ( type Symbol )
import Data.Word
import Data.Kind ( type Type )

-- | Types which can encode natural (positive integer) lengths.
--
-- Types must provide convert to and from 'Int', which is the most common type
-- used for data lengths.
class LenNat a where
    -- | The maximum value the type can encode.
    type LenNatMax a :: Natural

    -- | The name of the type, to display when used as part of a predicate.
    type LenNatName a :: Symbol

    -- | Turn an 'Int' length into an @a@.
    --
    -- It is guaranteed that the 'Int' fits i.e. @<= 'LenNatMax' a@.
    lenToNat :: Int -> a

    -- | Turn an @a@ into an 'Int' length.
    --
    -- Don't worry if @a@ may encode larger numbers than 'Int'. I think other
    -- things will be breaking at that point. Or perhaps it's our responsibility
    -- to emit the runtime error? TODO.
    natToLen :: a -> Int

-- | The unit can only encode 1 value -> lengths of 0 only.
instance LenNat () where
    type LenNatMax  () = 0
    type LenNatName () = "()"
    lenToNat = \case
      0 -> ()
      _ -> error "you lied to refine and broke everything :("
    natToLen () = 0

-- | Byte ordering doesn't change how prefixes work.
deriving via (a :: Type) instance LenNat a => LenNat (ByteOrdered end a)

instance LenNat Word8  where
    type LenNatMax  Word8  = 2^8  - 1
    type LenNatName Word8 = "Word8"
    lenToNat = fromIntegral
    natToLen = fromIntegral
instance LenNat Word16 where
    type LenNatMax  Word16 = 2^16 - 1
    type LenNatName Word16 = "Word16"
    lenToNat = fromIntegral
    natToLen = fromIntegral
instance LenNat Word32 where
    type LenNatMax  Word32 = 2^32 - 1
    type LenNatName Word32 = "Word32"
    lenToNat = fromIntegral
    natToLen = fromIntegral -- TODO check for overflow?
instance LenNat Word64 where
    type LenNatMax  Word64 = 2^64 - 1
    type LenNatName Word64 = "Word64"
    lenToNat = fromIntegral
    natToLen = fromIntegral -- TODO check for overflow?
