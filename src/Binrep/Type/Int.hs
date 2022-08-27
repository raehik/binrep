{-# LANGUAGE UndecidableInstances #-}

{- TODO can I replace this with a closed newtype family?? idk if I even want to
    it's just this is clumsy to use sometimes
-}

module Binrep.Type.Int where

import Binrep
import Binrep.Type.Common ( Endianness(..) )
import Strongweak

import Data.Word
import Data.Int
import Data.Aeson
import FlatParse.Basic qualified as FP
import Mason.Builder qualified as Mason

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )
import GHC.TypeNats

-- | Wrapper type grouping machine integers (sign, size) along with an explicit
--   endianness.
--
-- The internal representation is selected via a type family to correspond to
-- the relevant Haskell data type, so common overflow behaviour should match.
-- We derive lots of handy instances, so you may perform regular arithmetic on
-- pairs of these types. For example:
--
-- >>> 255 + 1 :: I 'U 'I1 e
-- 0
--
-- >>> 255 + 1 :: I 'U 'I2 e
-- 256
newtype I (sign :: ISign) (size :: ISize) (e :: Endianness)
  = I { getI :: IRep sign size }
    deriving stock (Generic)

deriving instance (Data (IRep sign size), Typeable sign, Typeable size, Typeable e) => Data (I sign size e)
deriving via (IRep sign size) instance Show     (IRep sign size) => Show     (I sign size e)

-- Steal various numeric instances from the representation types.
deriving via (IRep sign size) instance Eq       (IRep sign size) => Eq       (I sign size e)
deriving via (IRep sign size) instance Ord      (IRep sign size) => Ord      (I sign size e)
deriving via (IRep sign size) instance Bounded  (IRep sign size) => Bounded  (I sign size e)
deriving via (IRep sign size) instance Num      (IRep sign size) => Num      (I sign size e)
deriving via (IRep sign size) instance Real     (IRep sign size) => Real     (I sign size e)
deriving via (IRep sign size) instance Enum     (IRep sign size) => Enum     (I sign size e)
deriving via (IRep sign size) instance Integral (IRep sign size) => Integral (I sign size e)

-- | Unsigned machine integers can be idealized as naturals.
instance (irep ~ IRep 'U size, Integral irep) => Weaken (I 'U size end) where
    type Weak (I 'U size end) = Natural
    weaken = fromIntegral
instance (irep ~ IRep 'U size, Integral irep, Bounded irep, Show irep, Typeable size, Typeable end)
  => Strengthen (I 'U size end) where
      strengthen = strengthenBounded

-- | Signed machine integers can be idealized as integers.
instance (irep ~ IRep 'S size, Integral irep) => Weaken (I 'S size end) where
    type Weak (I 'S size end) = Integer
    weaken = fromIntegral
instance (irep ~ IRep 'S size, Integral irep, Bounded irep, Show irep, Typeable size, Typeable end)
  => Strengthen (I 'S size end) where
      strengthen = strengthenBounded

-- | Machine integer sign.
data ISign
  = S -- ^   signed
  | U -- ^ unsigned
    deriving stock (Generic, Data, Show, Eq)

-- | Machine integer size in number of bytes.
data ISize = I1 | I2 | I4 | I8
    deriving stock (Generic, Data, Show, Eq)

-- | Grouping for matching a signedness and size to a Haskell integer data type.
type family IRep (sign :: ISign) (size :: ISize) where
    IRep 'U 'I1 = Word8
    IRep 'S 'I1 =  Int8
    IRep 'U 'I2 = Word16
    IRep 'S 'I2 =  Int16
    IRep 'U 'I4 = Word32
    IRep 'S 'I4 =  Int32
    IRep 'U 'I8 = Word64
    IRep 'S 'I8 =  Int64

-- Also steal Aeson instances. The parser applies bounding checks appropriately.
deriving via (IRep sign size) instance ToJSON   (IRep sign size) => ToJSON   (I sign size e)
deriving via (IRep sign size) instance FromJSON (IRep sign size) => FromJSON (I sign size e)

instance KnownNat (CBLen (I sign size end)) => BLen (I sign size end) where
    type CBLen (I sign size end) = CBLen (IRep sign size)

instance Put (I 'U 'I1 e) where put = put . getI
instance Get (I 'U 'I1 e) where get = I <$> get
instance Put (I 'S 'I1 e) where put = put . getI
instance Get (I 'S 'I1 e) where get = I <$> get

instance Put (I 'U 'I2 'BE) where put (I i) = Mason.word16BE i
instance Get (I 'U 'I2 'BE) where get = I <$> cutEBase FP.anyWord16be (ERanOut 2)
instance Put (I 'U 'I2 'LE) where put (I i) = Mason.word16LE i
instance Get (I 'U 'I2 'LE) where get = I <$> cutEBase FP.anyWord16le (ERanOut 2)
instance Put (I 'S 'I2 'BE) where put (I i) = Mason.int16BE i
instance Get (I 'S 'I2 'BE) where get = I <$> cutEBase FP.anyInt16be  (ERanOut 2)
instance Put (I 'S 'I2 'LE) where put (I i) = Mason.int16LE i
instance Get (I 'S 'I2 'LE) where get = I <$> cutEBase FP.anyInt16le  (ERanOut 2)

instance Put (I 'U 'I4 'BE) where put (I i) = Mason.word32BE i
instance Get (I 'U 'I4 'BE) where get = I <$> cutEBase FP.anyWord32be (ERanOut 4)
instance Put (I 'U 'I4 'LE) where put (I i) = Mason.word32LE i
instance Get (I 'U 'I4 'LE) where get = I <$> cutEBase FP.anyWord32le (ERanOut 4)
instance Put (I 'S 'I4 'BE) where put (I i) = Mason.int32BE i
instance Get (I 'S 'I4 'BE) where get = I <$> cutEBase FP.anyInt32be  (ERanOut 4)
instance Put (I 'S 'I4 'LE) where put (I i) = Mason.int32LE i
instance Get (I 'S 'I4 'LE) where get = I <$> cutEBase FP.anyInt32le  (ERanOut 4)

instance Put (I 'U 'I8 'BE) where put (I i) = Mason.word64BE i
instance Get (I 'U 'I8 'BE) where get = I <$> cutEBase FP.anyWord64be (ERanOut 8)
instance Put (I 'U 'I8 'LE) where put (I i) = Mason.word64LE i
instance Get (I 'U 'I8 'LE) where get = I <$> cutEBase FP.anyWord64le (ERanOut 8)
instance Put (I 'S 'I8 'BE) where put (I i) = Mason.int64BE i
instance Get (I 'S 'I8 'BE) where get = I <$> cutEBase FP.anyInt64be  (ERanOut 8)
instance Put (I 'S 'I8 'LE) where put (I i) = Mason.int64LE i
instance Get (I 'S 'I8 'LE) where get = I <$> cutEBase FP.anyInt64le  (ERanOut 8)

-- | Shortcut.
type family IMax (sign :: ISign) (size :: ISize) :: Natural where
    IMax sign size = MaxBound (IRep sign size)

-- | Restricted reflected version of @maxBound@.
type family MaxBound w :: Natural where
    MaxBound Word8  = 255
    MaxBound  Int8  = 127
    MaxBound Word16 = 65535
    MaxBound  Int16 = 32767
    MaxBound Word32 = 4294967295
    MaxBound  Int32 = 2147483647
    MaxBound Word64 = 18446744073709551615
    MaxBound  Int64 = 9223372036854775807
