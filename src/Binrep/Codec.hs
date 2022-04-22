module Binrep.Codec where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as B
import Data.Serialize

-- | Types that can be coded precisely between binary and a Haskell type.
--
-- This class looks identical to cereal's 'Serialize', but we take a different
-- approach to instances.
--
-- 'Data.Serialize.Serialize' defines an internal binary codec for common
-- Haskell types. It makes implicit decisions, such as big-endian for all
-- integer types, and coding lists with a (big-endian) 'Word64' length prefix.
-- It only works with other data serialized with the same library.
--
-- This typeclass defines composable binary codec combinators. If you want to
-- write a codec for a 'Word64', it needs to specify its endianness in the type.
-- If you want to write a codec for a list with a 'Word8' length prefix, it must
-- come with a proof that it's not oversized.
--
-- The idea is to use this typeclass along with various annotated newtypes to
-- allow defining a Haskell type's binary representation directly in the types.
-- In cases where you're doing intermediate serialization and not much else, it
-- may be convenient. You will need to do a bunch of (free at runtime) wrapping
-- though.
class BinaryCodec a where
    toBin   :: Putter a     -- ^ Encode to   binary. Same as cereal's 'put'.
    fromBin :: Get a        -- ^ Decode from binary. Same as cereal's 'get'.

-- | Run the encoder for a supporting type.
binEncode :: BinaryCodec a => a -> BS.ByteString
binEncode = runPut . toBin

-- | Run the decoder a supporting type.
binDecode :: BinaryCodec a => BS.ByteString -> Either String a
binDecode = runGet fromBin

-- | Serialize each element in order. No length indicator, so parse until either
--   error or EOF. Usually not what you want, but sometimes used at the "top" of
--   binary formats.
instance BinaryCodec a => BinaryCodec [a] where
    toBin as = mapM_ toBin as
    fromBin = go []
      where
        go as = do
            a <- fromBin
            isEmpty >>= \case
              True -> return $ reverse $ a : as
              False -> go $ a : as

-- | Types that can be coded between binary and a Haskell type given some
--   runtime information.
--
-- We can't prove something related to a value and pass that proof along without
-- dependent types, meaning we can't split validation from encoding like in
-- 'BinaryCodec', so encoding can fail. However, by allowing an arbitrary
-- environment, we can define many more convenient instances.
--
-- For example, you can't write a 'BinaryCodec' instance for 'Word16' because it
-- doesn't specify its endianness. But you can define 'BinaryCodecWith
-- Endianness Word16'! This was, you can decide how much of the binary schema
-- you want to place directly in the types, and how much to configure
-- dynamically.
--
-- This class defaults to the free implementation provided by 'BinaryCodec',
-- which ignores the environment and wraps serializing with 'Right'.
class BinaryCodecWith r a where
    -- | Encode to binary with the given environment.
    toBinWith   :: r -> a -> Either String B.Builder
    default toBinWith :: BinaryCodec a => r -> a -> Either String B.Builder
    toBinWith = const $ Right . execPut . toBin

    -- | Decode to binary with the given environment.
    fromBinWith :: r -> Get a
    default fromBinWith :: BinaryCodec a => r -> Get a
    fromBinWith = const fromBin

-- | Run the encoder for a supporting type using the given environment.
binEncodeWith :: BinaryCodecWith r a => r -> a -> Either String BS.ByteString
binEncodeWith r a = BL.toStrict . B.toLazyByteString <$> toBinWith r a

-- | Run the decoder for a supporting type using the given environment.
binDecodeWith :: BinaryCodecWith r a => r -> BS.ByteString -> Either String a
binDecodeWith = runGet . fromBinWith
