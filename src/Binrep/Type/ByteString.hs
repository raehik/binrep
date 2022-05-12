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
import Binrep.Type.Array
import Binrep.Util

import Refined
import Refined.Unsafe

import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as B
import Data.Serialize qualified as Cereal
import Data.Word ( Word8 )
import Data.Typeable ( typeRep, typeOf )
import GHC.TypeNats ( KnownNat )

{-
import GHC.TypeNats ( KnownNat, type (<=), cmpNat, natVal' )
import GHC.TypeLits ( OrderingI(..) )
import Data.Proxy ( Proxy(..) )
import GHC.Exts ( proxy#, Proxy# )
-}

import Control.Monad ( replicateM )

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

-- | Bytestring representation.
data Rep
  = C
  -- ^ C-style bytestring. Arbitrary length, terminated with a null byte.
  --   Permits no null bytes inside the bytestring.

  | Pascal ISize Endianness
  -- ^ Pascal-style bytestring. Length defined in a prefixing integer of given
  --   size and endianness.
    deriving stock (Generic, Typeable, Data, Show, Eq)

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

-- | Look at this! Fully type safe!! Oh my god!!!!!!
instance (BLen a, itype ~ I 'U size end, KnownNat (CBLen itype))
      => BLen (LenPfx size end a) where
    blen rpa = cblen @itype + blen (unrefine rpa)

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
    , ALen a
    , Show irep, Typeable size, Typeable e, Typeable a
    ) => Predicate ('Pascal size e) a where
    validate p a
     | len > fromIntegral max'
        = throwRefineOtherException (typeRep p) $
              tshow (typeOf a)
            <>" too long for given length prefix type: "
            <>tshow len<>" > "<>tshow max'
     | otherwise = success
      where
        len  = alen a
        max' = maxBound @irep

--------------------------------------------------------------------------------

-- | An "array" type prefixed with its length via an unsigned machine integer
--   using the given size and endianness.
--
-- This could be represented more accurately via an existential wrapper over
-- @Array n a@ where @n <= IMax 'U size@. But I'm having issues defining it. So
-- definitions here will be similar to @Array@'s.
type LenPfx (size :: ISize) (end :: Endianness) a = Refined ('Pascal size end) a

instance
    ( Put a
    , ALen a
    , irep ~ IRep 'U size
    , Integral irep
    , itype ~ I 'U size end
    , Put itype)
      => Put (LenPfx size end a) where
    put ra = put @itype (fromIntegral (alen a)) <> put a
      where a = unrefine ra

-- TODO why safe
instance
    ( Get a
    , irep ~ IRep 'U size
    , Integral irep
    , itype ~ I 'U size end
    , Get itype)
      => Get (LenPfx size end [a]) where
    get = do
        len <- get @itype
        as <- replicateM (fromIntegral len) get
        return $ reallyUnsafeRefine as

instance (irep ~ IRep 'U size, Integral irep, itype ~ I 'U size end, Get itype) => Get (LenPfx size end B.ByteString) where
    get = do
        len <- get @itype
        a <- Cereal.isolate (fromIntegral len) get
        return $ reallyUnsafeRefine a

--------------------------------------------------------------------------------

{-

-- | More accurate & safer representation, but harder to use.
newtype LenPfx' (size :: ISize) (end :: Endianness) a =
    --LenPfx' { unLenPfx' :: forall n. (n <= IMax 'U size, KnownNat n) => Array n a }
    LenPfx' { unLenPfx' :: forall n. Array n a }

validateLenPfx'
    :: forall size end n a max
    .  (KnownNat n, max ~ IMax 'U size, KnownNat max)
    => Array n a
    -> Maybe (LenPfx' size end a)
validateLenPfx' a =
    case cmpNat (Proxy :: Proxy n) (Proxy :: Proxy max) of
      GTI -> Nothing
      _   -> Just $ LenPfx' $ reallyUnsafeRefine $ unrefine a -- TODO

instance (itype ~ I 'U size end, KnownNat (CBLen itype), BLen a)
  => BLen (LenPfx' size end a) where
    blen (LenPfx' x) = cblen @itype + blen x

instance (itype ~ I 'U size end, Put itype) => Put (LenPfx' size end a) where
    put (LenPfx' x) = put (f @itype x)

f :: forall x n a. (KnownNat n, Integral x) => Array n a -> x
f _ = fromIntegral n
  where n = natVal' (proxy# :: Proxy# n)

-}
