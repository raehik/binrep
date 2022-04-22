{-# LANGUAGE OverloadedStrings #-}

module Binscheme.Types.Strings where

import Binscheme.Codec
import Binscheme.ByteLen
import Binscheme.Types.Ints
import Binscheme.Util
import Refined
import Refined.WithRefine
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder qualified as B
import Data.Serialize
import Data.Word
import Data.Typeable ( Typeable, typeRep )
import Control.Monad ( replicateM )

-- | TODO
data StrRep = C | Pascal ISize Endianness

-- | TODO
--
-- We also use this as a predicate, because the 'Pascal' constructor looks
-- identical to what we would want for a @LengthPrefixed@ predicate.
newtype Str (rep :: StrRep)
  = Str { getStr :: BS.ByteString }

fromBinCString :: Get BS.ByteString
fromBinCString = go mempty
  where go buf = do
            getWord8 >>= \case
              0x00    -> return $ BL.toStrict $ B.toLazyByteString buf
              nonNull -> go $ buf <> B.word8 nonNull

instance ByteLen (Str 'C) where
    blen cstr = fromIntegral $ BS.length (getStr cstr) + 1

instance ByteLen (I 'U size e) => ByteLen (Str ('Pascal size e )) where
    blen pstr = fromIntegral (blen @(I 'U size e) undefined) + fromIntegral (BS.length (getStr pstr))

-- | Total shite parsing efficiency. But, to be fair, that's why we don't
--   serialize arbitrary-length C strings!
instance BinaryCodec (Str 'C) where
    toBin cstr = do
        putByteString $ getStr cstr
        putWord8 0x00
    fromBin = Str <$> fromBinCString

instance BinaryCodecWith _r (Str 'C)

-- TODO yeah I gotta do this because now the size info is actually in the
-- newtype -- which makes the most sense, because I want to do similar on the
-- value level! looks a tiny bit jank but ✓
data WellSized

-- | TODO explain why safe
instance BinaryCodec (WithRefine 'Enforced WellSized (Str ('Pascal 'I1 e))) where
    toBin wrepstr = do
        toBin @(I 'U 'I1 e) $ fromIntegral $ BS.length bs
        putByteString bs
      where bs = getStr $ unWithRefine wrepstr
    fromBin = do
        len <- fromBin @(I 'U 'I1 e)
        bs <- getByteString $ fromIntegral len
        return $ reallyUnsafeEnforce $ Str bs

instance BinaryCodecWith StrRep BS.ByteString where
    toBinWith strRep bs =
        case strRep of
          C -> toBinWith undefined $ Str @'C bs
          Pascal size _e -> do
            case size of
              I1 -> do
                if   len > fromIntegral (maxBound @Word8)
                then Left "bytestring too long for configured static-size length prefix"
                else Right $ B.byteString bs
              _ -> undefined
      where len = BS.length bs
    fromBinWith = \case C -> fromBinCString
                        Pascal _size _e -> undefined

-- Fun and correct, but it does give us an orphan instance lol
type LenPfx size e = Str ('Pascal size e)

-- | TODO why safe
instance (ByteLen a, itype ~ I 'U size e, ByteLen itype)
      => ByteLen (WithRefine 'Enforced (LenPfx size e) a) where
    blen wrelpa = blen @itype undefined + blen (unWithRefine wrelpa)

instance (Foldable f, Typeable f, Typeable e) => Predicate (LenPfx 'I4 e) (f a) where
    validate p a
      | len > fromIntegral max'
          = throwRefineOtherException (typeRep p) $
              "too long for given length prefix type: "<>tshow len<>" > "<>tshow max'
      | otherwise = success
      where
        len = length a
        max' = maxBound @(IRep 'U 'I4)

-- | TODO why safe
instance (BinaryCodec a, irep ~ IRep 'U size, itype ~ I 'U size e, Num irep, Integral irep, BinaryCodec itype)
      => BinaryCodec (WithRefine 'Enforced (LenPfx size e) [a]) where
    fromBin = do
        len <- fromBin @itype
        as <- replicateM (fromIntegral len) (fromBin @a)
        return $ reallyUnsafeEnforce as
    toBin wreas = do
        toBin @itype $ fromIntegral $ length as
        mapM_ (toBin @a) as
      where as = unWithRefine wreas
