{- | Machine bytestrings.

I mix string and bytestring terminology here due to bad C influences, but this
module is specifically interested in bytestrings and their encoding. String/text
encoding is handled in another module.

Note that the length prefix predicate is also defined here... because that's
just Pascal-style bytestrings, extended to other types. I can't easily put it in
an orphan module, because we define byte length for *all length-prefixed types*
in one fell swoop.
-}

-- TODO redocument. pretty all over the place

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.ByteString where

import Binrep
import Binrep.Type.Common ( Endianness )
import Binrep.Type.Int
import Binrep.Util

import Refined
import Refined.Unsafe

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as B
import Data.Serialize qualified as Cereal
import Data.Word ( Word8 )
import GHC.TypeNats ( KnownNat )

import GHC.Generics ( Generic )
import Data.Data ( Data )

import Data.Typeable ( Typeable, typeRep )

-- | Bytestring representation.
data Rep
  = C
  -- ^ C-style bytestring. Arbitrary length, terminated with a null byte.
  --   Permits no null bytes inside the bytestring.

  | Pascal ISize Endianness
  -- ^ Pascal-style bytestring. Length defined in a prefixing integer of given
  --   size and endianness.
    deriving stock (Generic, Data, Show, Eq)

-- | A bytestring using the given representation, stored in the 'Text' type.
type AsByteString (rep :: Rep) = Refined rep B.ByteString

getCString :: Cereal.Get B.ByteString
getCString = go mempty
  where go buf = do
            get @Word8 >>= \case
              0x00    -> return $ BL.toStrict $ B.toLazyByteString buf
              nonNull -> go $ buf <> B.word8 nonNull

instance BLen (AsByteString 'C) where
    blen cbs = unsafePosIntToNat (B.length (unrefine cbs)) + 1

instance Put (AsByteString 'C) where
    put cbs = put (unrefine cbs) <> put @Word8 0x00

-- | Total shite parsing efficiency. But, to be fair, that's why we don't
--   serialize arbitrary-length C strings!
instance Get (AsByteString 'C) where
    get = reallyUnsafeRefine <$> getCString

instance (itype ~ I 'U size end, irep ~ IRep 'U size, KnownNat (CBLen irep)) => BLen (AsByteString ('Pascal size end)) where
    blen pbs = cblen @itype + blen (unrefine pbs)

instance (itype ~ I 'U size end, irep ~ IRep 'U size, Put itype, Num irep) => Put (AsByteString ('Pascal size end)) where
    put pbs = put @itype (fromIntegral (B.length bs)) <> put bs
      where bs = unrefine pbs

instance (itype ~ I 'U size end, irep ~ IRep 'U size, Integral irep, Get itype) => Get (AsByteString ('Pascal size end)) where
    get = do
        len <- get @itype
        bs <- Cereal.getBytes (fromIntegral len)
        return $ reallyUnsafeRefine bs


{-
-- TODO finish and explain why safe. actually should use singletons!
instance PutWith Rep B.ByteString where
    putWith strRep bs =
        case strRep of
          C -> case refine @'C bs of
                 Left  e   -> Left $ show e
                 Right rbs -> putWithout rbs
          Pascal size _e -> do
            case size of
              I1 -> do
                if   len > fromIntegral (maxBound @Word8)
                then Left "bytestring too long for configured static-size length prefix"
                else Right $ B.byteString bs
              _ -> undefined
      where len = B.length bs
-}

-- TODO finish and explain why safe. actually should use singletons!
instance GetWith Rep B.ByteString where
    getWith = \case C -> getCString
                    Pascal _size _e -> undefined

-- | A C-style bytestring must not contain any null bytes.
instance Predicate 'C B.ByteString where
    validate p bs
     | B.any (== 0x00) bs = throwRefineOtherException (typeRep p) $
        "null byte not permitted in in C-style bytestring"
     | otherwise = success

instance
    ( irep ~ IRep 'U size
    , Bounded irep, Integral irep
    , Show irep, Typeable size, Typeable e
    ) => Predicate ('Pascal size e) B.ByteString where
    validate p bs
     | len > fromIntegral max'
        = throwRefineOtherException (typeRep p) $
              "bytestring too long for given length prefix type: "
            <>tshow len<>" > "<>tshow max'
     | otherwise = success
      where
        len  = B.length bs
        max' = maxBound @irep
