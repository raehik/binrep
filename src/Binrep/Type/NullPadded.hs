{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.NullPadded where

import Binrep
import Binrep.Util ( tshow, natVal'' )

import Refined
import Refined.Unsafe

import GHC.TypeNats
import GHC.Natural ( minusNaturalMaybe )
import Data.Typeable ( typeRep )
import FlatParse.Basic qualified as FP
import FlatParse.Basic ( Parser )
import Mason.Builder qualified as Mason
import Data.ByteString qualified as BS

data NullPad (n :: Natural)

type NullPadded n a = Refined (NullPad n) a

-- | The size of some null-padded data is known - at compile time!
type instance CBLen (NullPadded n a) = n

deriving anyclass instance KnownNat n => BLen (NullPadded n a)

instance (BLen a, KnownNat n) => Predicate (NullPad n) a where
    validate p a
      | len > n
          = throwRefineOtherException (typeRep p) $
                   "too long: " <> tshow len <> " > " <> tshow n
      | otherwise = success
      where
        n = natVal'' @n
        len = blen a

-- TODO cleanup
instance (Put a, BLen a, KnownNat n) => Put (NullPadded n a) where
    put wrnpa =
        let npa = unrefine wrnpa
            paddingLength = n - blen npa
         in put npa <> Mason.byteString (BS.replicate (fromIntegral paddingLength) 0x00)
      where
        n = natVal'' @n

-- | Safety: we assert actual length is within expected length (in order to
--   calculate how much padding to parse).
--
-- Note that the consumer probably doesn't care about the content of the
-- padding, just that the data is chunked correctly. I figure we care about
-- correctness here, so it'd be nice to know about the padding well-formedness
-- (i.e. that it's all nulls).
--
-- TODO maybe better definition via isolate
instance (Get a, BLen a, KnownNat n) => Get (NullPadded n a) where
    get = do
        a <- get
        let len = blen a
        case minusNaturalMaybe n len of
          Nothing -> FP.err $ "too long: " <> show len <> " > " <> show n
          Just nullstrLen -> do
            getNNulls nullstrLen
            return $ reallyUnsafeRefine a
      where
        n = natVal'' @n

getNNulls :: Natural -> Parser String ()
getNNulls = \case 0 -> return ()
                  n -> FP.anyWord8 >>= \case
                         0x00    -> getNNulls $ n-1
                         nonNull -> FP.err "TODO expected null, wasn't"
