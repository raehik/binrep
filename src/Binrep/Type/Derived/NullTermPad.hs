-- {-# LANGUAGE OverloadedStrings #-} -- for refined errors

module Binrep.Type.Derived.NullTermPad where

{-

import Binrep
import Refined
import GHC.TypeNats ( KnownNat )
import Binrep.Type.NullTerminated
import Binrep.Type.NullPadded

type NullTermPad n = NullTerminate `And` NullPad n

-- sadly we can't use a refined typesym here or we get binrep orphans
--newtype NullTermPadded n a = NullTermPadded { unNullTermPadded :: NullTermPad n }
type NullTermPadded n = Refined (NullTermPad n)

instance IsCBLen (NullTermPadded n a) where type CBLen (NullTermPadded n a) = n
deriving via ViaCBLen (NullTermPadded n a) instance
  KnownNat n => BLen (NullTermPadded n a)

-- | Serialization of null-terminated data may be defined generally using the
--   data's underlying serializer.
instance Put a => Put (NullTerminated a) where
    {-# INLINE put #-}
    put a = put (unrefine a) <> put @Word8 0x00

-- | We may parse any null-terminated data using a special flatparse combinator.
instance Get a => Get (NullTerminated a) where
    {-# INLINE get #-}
    get = reallyUnsafeRefine <$> getEBase (FP.isolateToNextNull get) (EFailNamed "cstring")

-}
