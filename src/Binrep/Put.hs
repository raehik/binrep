module Binrep.Put where

import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as B

import Data.ByteString qualified as BS
import Data.Serialize.Put qualified as Cereal

import Data.Word
import Data.Int

class Put a where
    -- | Serialize to binary.
    put :: Cereal.Putter a

-- | Run the serializer.
runPut :: Put a => a -> BS.ByteString
runPut = Cereal.runPut . put

-- | Serialize each element in order. No length indicator, so parse until either
--   error or EOF. Usually not what you want, but sometimes used at the "top" of
--   binary formats.
instance Put a => Put [a] where
    put = mapM_ put

instance Put Word8 where put = Cereal.putWord8
instance Put  Int8 where put = Cereal.putInt8

class PutWith r a where
    -- | Serialize to binary with the given environment.
    putWith :: r -> a -> Either String B.Builder
    default putWith :: Put a => r -> a -> Either String B.Builder
    putWith = const $ putWithout

-- | Helper for wrapping a 'BinRep' into a 'BinRepWith' (for encoding).
putWithout :: Put a => a -> Either String B.Builder
putWithout = Right . Cereal.execPut . put

instance Put a => PutWith r [a]

-- | Run the serializer with the given environment.
runPutWith :: PutWith r a => r -> a -> Either String BS.ByteString
runPutWith r a = BL.toStrict . B.toLazyByteString <$> putWith r a
