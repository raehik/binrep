{-# OPTIONS_GHC -fno-warn-orphans #-} -- for generic data op instance

-- TODO some instances are wrong, Void should be typeerror

module Binrep.Put.Mason where

import Mason.Builder qualified as Mason

import Data.ByteString qualified as B

import Data.Word
import Data.Int
import Data.Void ( Void, absurd )

import GHC.Generics ( Generic, type Rep )
import Generic.Data.FoldMap

type Builder = Mason.BuilderFor Mason.StrictByteStringBackend

class Put a where
    -- | Serialize to binary.
    put :: a -> Builder

-- | Run the serializer.
runPut :: Put a => a -> B.ByteString
runPut = runBuilder . put

runBuilder :: Builder -> B.ByteString
runBuilder = Mason.toStrictByteString

instance GenericFoldMap Builder where
    type GenericFoldMapC Builder a = Put a
    genericFoldMapF = put

-- | Serialize a term of the non-sum type @a@ via its 'Generic' instance.
putGenericNonSum
    :: (Generic a, GFoldMapNonSum Builder (Rep a))
    => a -> Builder
putGenericNonSum = genericFoldMapNonSum

-- | Serialize a term of the sum type @a@ via its 'Generic' instance.
--
-- You must provide a serializer for @a@'s constructors. This is regrettably
-- inefficient due to having to use 'String's. Alas. Do write your own instance
-- if you want better performance!
putGenericSum
    :: (Generic a, GFoldMapSum Builder (Rep a))
    => (String -> Builder) -> a -> Builder
putGenericSum = genericFoldMapSum

-- | Impossible to serialize 'Void'.
instance Put Void where
    put = absurd

-- | Serialize each element in order. No length indicator, so parse until either
--   error or EOF. Usually not what you want, but sometimes used at the "top" of
--   binary formats.
instance Put a => Put [a] where
    put = mconcat . map put

instance (Put a, Put b) => Put (a, b) where
    put (a, b) = put a <> put b

-- | Serialize the bytestring as-is.
--
-- Careful -- the only way you're going to be able to parse this is to read
-- until EOF.
instance Put B.ByteString where
    put = Mason.byteString
    {-# INLINE put #-}

-- need to give args for RankNTypes reasons I don't understand
instance Put Word8 where
    put w = Mason.word8 w
    {-# INLINE put #-}
instance Put  Int8 where
    put w = Mason.int8 w
    {-# INLINE put #-}

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
runPutWith :: PutWith r a => r -> a -> Either String B.ByteString
runPutWith r a = case putWith r a of Left  e -> Left e
                                     Right x -> Right $ runBuilder x
