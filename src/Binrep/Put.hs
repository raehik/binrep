module Binrep.Put where

import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as BB

import Data.ByteString qualified as B
import Data.Serialize.Put qualified as Cereal

import Data.Word
import Data.Int

class Put a where
    -- | Serialize to binary.
    put :: Cereal.Putter a

-- | Run the serializer.
runPut :: Put a => a -> B.ByteString
runPut = Cereal.runPut . put

-- | Serialize each element in order. No length indicator, so parse until either
--   error or EOF. Usually not what you want, but sometimes used at the "top" of
--   binary formats.
instance Put a => Put [a] where
    put = mapM_ put

instance (Put a, Put b) => Put (a, b) where
    put (a, b) = put a *> put b

-- TODO: We have to be very careful if we want to provide an 'Either' instance.
-- cereal serializes Left=0, Right=1 (and, uh, doesn't check ==1 when parsing
-- lol. just 0 or Right). Similarly for Maybe (0=Nothing, 1=Just).

-- | Serialize the bytestring as-is.
--
-- Careful -- the only way you're going to be able to parse this is to read
-- until EOF.
instance Put B.ByteString where
    put = Cereal.putByteString

instance Put Word8 where put = Cereal.putWord8
instance Put  Int8 where put = Cereal.putInt8

class PutWith r a where
    -- | Serialize to binary with the given environment.
    putWith :: r -> a -> Either String BB.Builder
    default putWith :: Put a => r -> a -> Either String BB.Builder
    putWith = const $ putWithout

-- | Helper for wrapping a 'BinRep' into a 'BinRepWith' (for encoding).
putWithout :: Put a => a -> Either String BB.Builder
putWithout = Right . Cereal.execPut . put

instance Put a => PutWith r [a]

-- | Run the serializer with the given environment.
runPutWith :: PutWith r a => r -> a -> Either String B.ByteString
runPutWith r a = BL.toStrict . BB.toLazyByteString <$> putWith r a
