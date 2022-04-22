{-# LANGUAGE OverloadedStrings #-}

module Binrep.Predicates.NullPadTo where

import Binrep.Codec
import Binrep.ByteLen
import Binrep.Util ( tshow )
import Refined
import Refined.WithRefine
import Data.Serialize
import GHC.TypeNats
import Numeric.Natural
import GHC.Natural ( minusNaturalMaybe )
import GHC.Exts ( proxy#, Proxy# )
import Data.Typeable
import Data.ByteString qualified as BS

data NullPadTo (n :: Nat)

instance KnownNat n => ByteLen (WithRefine 'Enforced (NullPadTo n) a) where
    blen = const n
      where n = natVal' (proxy# :: Proxy# n)

instance (ByteLen a, KnownNat n) => Predicate (NullPadTo n) a where
    validate p a
      | len > n
          = throwRefineOtherException (typeRep p) $
                   "too long: " <> tshow len <> " > " <> tshow n
      | otherwise = success
      where
        n = natVal' (proxy# :: Proxy# n)
        len = blen a

-- | predicate is inherently enforced due to checking length to calculate how
--   many succeeding nulls to parse
--
-- Note that the consumer probably doesn't care about the content of the
-- padding, just that the data is chunked correctly. I figure we care about
-- correctness here, so it'd be nice to know about the padding well-formedness
-- (i.e. that it's all nulls).
instance (BinaryCodec a, ByteLen a, KnownNat n)
      => BinaryCodec (WithRefine 'Enforced (NullPadTo n) a) where
    fromBin = do
        a <- fromBin
        let len = blen a
        case minusNaturalMaybe n len of
          Nothing -> fail $ "too long: " <> show len <> " > " <> show n
          Just nullstrLen -> do
            getNNulls nullstrLen
            return $ reallyUnsafeEnforce a
      where
        n = natVal' (proxy# :: Proxy# n)
    toBin wrnpa = do
        let npa = unWithRefine wrnpa
        toBin npa
        let paddingLength = n - blen npa
        putByteString $ BS.replicate (fromIntegral paddingLength) 0x00
      where
        n = natVal' (proxy# :: Proxy# n)

getNNulls :: Natural -> Get ()
getNNulls = \case 0 -> return ()
                  n -> getWord8 >>= \case
                         0x00    -> getNNulls $ n-1
                         nonNull -> do
                           offset <- bytesRead
                           fail $  "expected null, found: "<> show nonNull
                                <> " at offset " <> show offset
                                <> ", " <> show n <> " more nulls to go"
