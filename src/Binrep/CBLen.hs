{-# LANGUAGE UndecidableInstances #-} -- for nested type families
{-# LANGUAGE AllowAmbiguousTypes  #-} -- for reification util

module Binrep.CBLen where

import GHC.TypeNats
import Data.Word
import Data.Int
import Binrep.Util.ByteOrder

import GHC.Exts ( Int#, Int(I#), Proxy# )
import Util.TypeNats ( natValInt )

import DeFun.Core ( type (~>), type App )

class IsCBLen a where type CBLen a :: Natural

instance IsCBLen () where type CBLen () = 0
instance (IsCBLen l, IsCBLen r) => IsCBLen (l, r) where
    type CBLen (l, r) = CBLen l + CBLen r

instance IsCBLen Word8  where type CBLen Word8  = 2^0
instance IsCBLen  Int8  where type CBLen  Int8  = 2^0
instance IsCBLen Word16 where type CBLen Word16 = 2^1
instance IsCBLen  Int16 where type CBLen  Int16 = 2^1
instance IsCBLen Word32 where type CBLen Word32 = 2^2
instance IsCBLen  Int32 where type CBLen  Int32 = 2^2
instance IsCBLen Word64 where type CBLen Word64 = 2^3
instance IsCBLen  Int64 where type CBLen  Int64 = 2^3

instance IsCBLen a => IsCBLen (ByteOrdered end a) where
    type CBLen (ByteOrdered end a) = CBLen a

-- | Reify a type's constant byte length to the term level.
cblen :: forall a. KnownNat (CBLen a) => Int
cblen = natValInt @(CBLen a)

cblen# :: forall a. KnownNat (CBLen a) => Int#
cblen# = i#
  where !(I# i#) = natValInt @(CBLen a)

cblenProxy# :: forall a. KnownNat (CBLen a) => Proxy# a -> Int#
cblenProxy# _ = i#
  where !(I# i#) = natValInt @(CBLen a)

type CBLenSym :: a ~> Natural
data CBLenSym a
type instance App CBLenSym a = CBLen a
