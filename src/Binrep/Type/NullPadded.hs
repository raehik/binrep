-- | Static-length data via null-padding.
--
-- For null-terminated strings, see 'Binrep.Type.ByteString'.

{-# LANGUAGE OverloadedStrings #-}

module Binrep.Type.NullPadded where

import Binrep
import Binrep.Util ( tshow )

import Refined
import Refined.Unsafe

import GHC.TypeNats
import Data.Typeable ( typeRep )
import FlatParse.Basic qualified as FP
import Mason.Builder qualified as Mason
import Data.ByteString qualified as BS

data NullPad (n :: Natural)

-- | A type which is to be null-padded to a given total length.
--
-- Given some @'NullPadded' n a@ term @x@, it is guaranteed that
--
--     blen x <= typeNatToBLen @n
--
-- That is, the serialized stored data will not be longer than the total length.
type NullPadded n a = Refined (NullPad n) a

instance KnownNat n => BLen (NullPadded n a) where
    -- | The size of some null-padded data is known - at compile time!
    type CBLen (NullPadded n a) = n

instance (BLen a, KnownNat n) => Predicate (NullPad n) a where
    validate p a
      | len <= n = success
      | otherwise
          = throwRefineOtherException (typeRep p) $
                   "too long: " <> tshow len <> " > " <> tshow n
      where
        n = typeNatToBLen @n
        len = blen a

instance (Put a, BLen a, KnownNat n) => Put (NullPadded n a) where
    put npa = put a <> Mason.byteString paddingBs
      where
        paddingBs = BS.replicate (blenToPosInt paddingLength) 0x00
        -- note that regular subtraction is legal here because we have a
        -- guarantee that @blen a <= n@. this is safe subtraction of naturals!
        paddingLength = n - blen a
        n = typeNatToBLen @n
        a = unrefine npa

-- | Safety: we assert actual length is within expected length (in order to
--   calculate how much padding to parse).
--
-- Note that the consumer probably doesn't care about the content of the
-- padding, just that the data is chunked correctly. I figure we care about
-- correctness here, so it'd be nice to know about the padding well-formedness
-- (i.e. that it's all nulls).
--
-- TODO: there is a possible clearer definition via isolate.
instance (Get a, BLen a, KnownNat n) => Get (NullPadded n a) where
    get = do
        a <- get
        let len = blen a
        case n `safeBLenSub` len of
          Nothing -> eBase $ EOverlong n len
          Just nullStrLen -> do
            let nullStrLen' = blenToPosInt nullStrLen
            paddingBs <- FP.takeBs nullStrLen'
            let expectedPaddingBs = BS.replicate nullStrLen' 0x00
            if   paddingBs == expectedPaddingBs
            then pure $ reallyUnsafeRefine a
            else eBase $ EExpected expectedPaddingBs paddingBs
      where n = typeNatToBLen @n
