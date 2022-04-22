module Binscheme.Types.Ints where

import Binscheme.Codec
import Binscheme.ByteLen
import GHC.Generics ( Generic )
import Data.Typeable
import Data.Word
import Data.Int
import Data.Aeson
import Data.Serialize

-- | Wrapper type grouping machine integers (sign, size) along with an explicit
--   endianness.
newtype I (sign :: ISign) (size :: ISize) (e :: Endianness)
  = I { getI :: IRep sign size }
    deriving stock (Generic, Typeable)

-- | Lots of deriving boilerplate due to the type family usage.
deriving stock                instance Show     (IRep sign size) => Show     (I sign size e)
deriving via (IRep sign size) instance Eq       (IRep sign size) => Eq       (I sign size e)
deriving via (IRep sign size) instance Ord      (IRep sign size) => Ord      (I sign size e)
deriving via (IRep sign size) instance Bounded  (IRep sign size) => Bounded  (I sign size e)
deriving via (IRep sign size) instance Num      (IRep sign size) => Num      (I sign size e)
deriving via (IRep sign size) instance Real     (IRep sign size) => Real     (I sign size e)
deriving via (IRep sign size) instance Enum     (IRep sign size) => Enum     (I sign size e)
deriving via (IRep sign size) instance Integral (IRep sign size) => Integral (I sign size e)
deriving via (IRep sign size) instance ToJSON   (IRep sign size) => ToJSON   (I sign size e)
deriving via (IRep sign size) instance FromJSON (IRep sign size) => FromJSON (I sign size e)

instance ByteLen (I s 'I1 e) where blen = const 1
instance ByteLen (I s 'I2 e) where blen = const 2
instance ByteLen (I s 'I4 e) where blen = const 4
instance ByteLen (I s 'I8 e) where blen = const 8

-- | Endianness doesn't apply for single-byte machine integers.
instance BinaryCodec (I 'U 'I1 e) where toBin   = putWord8 . getI
                                        fromBin = I <$> getWord8
instance BinaryCodec (I 'S 'I1 e) where toBin   = putInt8 . getI
                                        fromBin = I <$> getInt8

instance BinaryCodec (I 'U 'I2 'BE) where toBin   = putWord16be . getI
                                          fromBin = I <$> getWord16be
instance BinaryCodec (I 'U 'I2 'LE) where toBin   = putWord16le . getI
                                          fromBin = I <$> getWord16le
instance BinaryCodec (I 'S 'I2 'BE) where toBin   = putInt16be . getI
                                          fromBin = I <$> getInt16be
instance BinaryCodec (I 'S 'I2 'LE) where toBin   = putInt16le . getI
                                          fromBin = I <$> getInt16le

instance BinaryCodec (I 'U 'I4 'BE) where toBin   = putWord32be . getI
                                          fromBin = I <$> getWord32be
instance BinaryCodec (I 'U 'I4 'LE) where toBin   = putWord32le . getI
                                          fromBin = I <$> getWord32le
instance BinaryCodec (I 'S 'I4 'BE) where toBin   = putInt32be . getI
                                          fromBin = I <$> getInt32be
instance BinaryCodec (I 'S 'I4 'LE) where toBin   = putInt32le . getI
                                          fromBin = I <$> getInt32le

instance BinaryCodec (I 'U 'I8 'BE) where toBin   = putWord64be . getI
                                          fromBin = I <$> getWord64be
instance BinaryCodec (I 'U 'I8 'LE) where toBin   = putWord64le . getI
                                          fromBin = I <$> getWord64le
instance BinaryCodec (I 'S 'I8 'BE) where toBin   = putInt64be . getI
                                          fromBin = I <$> getInt64be
instance BinaryCodec (I 'S 'I8 'LE) where toBin   = putInt64le . getI
                                          fromBin = I <$> getInt64le

-- | Byte order.
data Endianness
  = BE -- ^ big    endian, MSB first. e.g. most network protocols
  | LE -- ^ little endian, MSB last.  e.g. most processor architectures

-- | Machine integer sign
data ISign
  = S -- ^   signed
  | U -- ^ unsigned

-- | Machine integer size in number of bytes.
data ISize = I1 | I2 | I4 | I8

type family IRep (sign :: ISign) (size :: ISize) where
    IRep 'U 'I1 = Word8
    IRep 'S 'I1 =  Int8
    IRep 'U 'I2 = Word16
    IRep 'S 'I2 =  Int16
    IRep 'U 'I4 = Word32
    IRep 'S 'I4 =  Int32
    IRep 'U 'I8 = Word64
    IRep 'S 'I8 =  Int64
