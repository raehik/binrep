-- {-# LANGUAGE ImpredicativeTypes #-}

module Binrep.Put where

import Mason.Builder qualified as Mason

import Data.ByteString qualified as B
import Data.Word
import Data.Int

type Builder = Mason.BuilderFor Mason.StrictByteStringBackend

class Put a where
    -- | Serialize to binary.
    put :: a -> Builder

-- | Run the serializer.
runPut :: Put a => a -> B.ByteString
runPut = Mason.toStrictByteString . put

-- | Serialize each element in order. No length indicator, so parse until either
--   error or EOF. Usually not what you want, but sometimes used at the "top" of
--   binary formats.
instance Put a => Put [a] where
    put = mconcat . map put

instance (Put a, Put b) => Put (a, b) where
    put (a, b) = put a <> put b

-- TODO: We have to be very careful if we want to provide an 'Either' instance.
-- cereal serializes Left=0, Right=1 (and, uh, doesn't check ==1 when parsing
-- lol. just 0 or Right). Similarly for Maybe (0=Nothing, 1=Just).

-- | Serialize the bytestring as-is.
--
-- Careful -- the only way you're going to be able to parse this is to read
-- until EOF.
instance Put B.ByteString where
    put = Mason.byteString

-- need to give args for RankNTypes reasons I don't understand
instance Put Word8 where put w = Mason.word8 w
instance Put  Int8 where put w = Mason.int8 w

-- | Put with inlined checks via an environment.
class PutWith r a where
    -- | Attempt to serialize to binary with the given environment.
    putWith :: r -> a -> Either String Builder
    default putWith :: Put a => r -> a -> Either String Builder
    putWith _ = putWithout

-- | Helper for wrapping a 'BinRep' into a 'BinRepWith' (for encoding).
putWithout :: Put a => a -> Either String Builder
putWithout = Right . put

instance Put a => PutWith r [a]

-- | Run the serializer with the given environment.
-- TODO reformat
runPutWith :: PutWith r a => r -> a -> Either String B.ByteString
runPutWith r a = do
    case putWith r a of
      Left e -> Left e
      Right x -> Right $ Mason.toStrictByteString x
