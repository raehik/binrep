{- | Machine bytestrings.

I mix string and bytestring terminology here due to bad C influences, but this
module is specifically interested in bytestrings and their encoding. String/text
encoding is handled in another module.

Note that the length prefix predicate is also defined here... because that's
just Pascal-style bytestrings, extended to other types. I can't easily put it in
an orphan module, because we define byte length for *all length-prefixed types*
in one fell swoop.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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
import Data.Word
import Numeric.Natural
import Data.Typeable ( Typeable, typeRep, Proxy, typeOf )
import GHC.TypeNats ( KnownNat )

import Control.Monad ( replicateM )

-- | Bytestring representation.
data Rep
  = C
  -- ^ C-style bytestring. Arbitrary length, terminated with a null byte.
  --   Permits no null bytes inside the bytestring.

  | Pascal ISize Endianness
  -- ^ Pascal-style bytestring. Length defined in a prefixing integer of given
  --   size and endianness.

-- | A bytestring using the given representation, stored in the 'Text' type.
type AsByteString (rep :: Rep) = Refined rep B.ByteString

getCString :: Cereal.Get B.ByteString
getCString = go mempty
  where go buf = do
            Cereal.getWord8 >>= \case
              0x00    -> return $ BL.toStrict $ B.toLazyByteString buf
              nonNull -> go $ buf <> B.word8 nonNull

instance BLen (AsByteString 'C) where
    blen cbs = naturalFromPosInt (B.length (unrefine cbs)) + 1

instance Put (AsByteString 'C) where
    put cbs = do
        Cereal.putByteString $ unrefine cbs
        Cereal.putWord8 0x00

-- | Total shite parsing efficiency. But, to be fair, that's why we don't
--   serialize arbitrary-length C strings!
instance Get (AsByteString 'C) where
    get = reallyUnsafeRefine <$> getCString

-- | Look at this! Fully type safe!! Oh my god!!!!!!
instance (BLen a, itype ~ I 'U size end, KnownNat (CBLen itype))
      => BLen (LenPfx size end a) where
    blen rpa = cblen @itype + blen (unrefine rpa)

instance Put (AsByteString ('Pascal 'I1 e)) where
    put rpbs = do
        put @(I 'U 'I1 e) $ fromIntegral $ B.length pbs
        Cereal.putByteString pbs
      where pbs = unrefine rpbs

-- | Safe: The number of bytes we get is limited by the prefix type.
instance Get (AsByteString ('Pascal 'I1 e)) where
    get = do
        len <- get @(I 'U 'I1 e)
        pbs <- Cereal.getByteString $ fromIntegral len
        return $ reallyUnsafeRefine pbs

deriving anyclass instance PutWith r (AsByteString ('Pascal 'I1 e))
deriving anyclass instance GetWith r (AsByteString ('Pascal 'I1 e))

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

-- | Is the given 'B.ByteString' short enough to allow placing its length in the
--   given size prefix?
instance
    ( irep ~ IRep 'U size
    , Bounded irep, Integral irep
    , Show irep, Typeable size, Typeable e
    ) => Predicate ('Pascal size e) B.ByteString where
    validate p = validateLengthPrefixed @size p (fromIntegral . B.length)

-- | Is the given list-like short enough to allow placing its length in the
--   given size prefix?
--
-- Note that we don't care about the elements inside or their size.
instance
    ( irep ~ IRep 'U size
    , Bounded irep, Integral irep
    , Foldable t
    , Show irep, Typeable size, Typeable e, Typeable t, Typeable a
    ) => Predicate ('Pascal size e) (t a) where
    validate p = validateLengthPrefixed @size p (fromIntegral . length)

-- | Instance helper. We cheat with 'Typeable's to obtain type tags without
--   asking the user explicitly. It's good enough, and refined uses them anyway.
validateLengthPrefixed
    :: forall size irep p a
    .  ( irep ~ IRep 'U size
       , Bounded irep, Integral irep
       , Show irep, Typeable size, Typeable p, Typeable a
       )
    => Proxy p
    -> (a -> Natural) -> a -> Maybe RefineException
validateLengthPrefixed p f a
 | len > fromIntegral max'
    = throwRefineOtherException (typeRep p) $
          tshow (typeOf a)
        <>" too long for given length prefix type: "
        <>tshow len<>" > "<>tshow max'
 | otherwise = success
  where
    len  = f a
    max' = maxBound @irep

--------------------------------------------------------------------------------

-- | A value prefixed with its length via an unsigned machine integer using the
--   given size and endianness.
type LenPfx (size :: ISize) (end :: Endianness) a = Refined ('Pascal size end) a

instance
    ( Put a
    , irep ~ IRep 'U size
    , Num irep, Integral irep
    , itype ~ I 'U size end
    , Put itype)
      => Put (LenPfx size end [a]) where
    put ras = do
        put @itype $ fromIntegral $ length as
        mapM_ (put @a) as
      where as = unrefine ras

-- TODO why safe
instance
    ( Get a
    , irep ~ IRep 'U size
    , Num irep, Integral irep
    , itype ~ I 'U size end
    , Get itype)
      => Get (LenPfx size end [a]) where
    get = do
        len <- get @itype
        as <- replicateM (fromIntegral len) (get @a)
        return $ reallyUnsafeRefine as
