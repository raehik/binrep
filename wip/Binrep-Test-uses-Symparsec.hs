-- TODO this is fine but uses Symparsec. move to tests or another pkg

{-# LANGUAGE UndecidableInstances #-} -- for CBLen instances
{-# LANGUAGE TemplateHaskell #-} -- for type-level cstr parsing

module Binrep.Test where

import Binrep
import Binrep.Type.Magic
import GHC.Generics ( Generic )
import Data.Word
import Binrep.Util.ByteOrder

import Generic.Data.MetaParse.Cstr
import Symparsec.Parsers
import Symparsec.Run ( Run'_ )
import GHC.TypeNats

import Binrep.Common.Via.Generically.NonSum

data DMagic = DMagic
  { dMagic1_8b :: Magic '[0xFF, 0, 1, 0, 1, 0, 1, 0xFF]
  } deriving stock (Show, Generic)
    deriving (IsCBLen, PutC, GetC) via GenericallyNonSum DMagic

data DStruct = DStruct
  { dStruct1 :: Magic '[0xFF, 0, 1, 0xFF]
  , dStruct2 :: ByteOrdered LE Word32
  , dStruct3 :: ()
  } deriving stock (Generic, Show)
    deriving (IsCBLen, PutC, GetC) via GenericallyNonSum DStruct

data DMagicSum = DMagicSum1 (Magic '[0]) | DMagicSum2 (Magic '[0xFF])
    deriving stock (Generic, Show)

instance CstrParser' DMagicSum where
    type CstrParseResult DMagicSum = Natural
$(pure [])
instance CstrParser  DMagicSum where
    type ReifyCstrParseResult DMagicSum n = KnownNat n
    type ParseCstr DMagicSum str = Run'_ (Literal "DMagicSum" :*>: NatHex) str

instance BLen DMagicSum where
    blen = blenGenericSum @DMagicSum (\_p -> 1)

instance Put DMagicSum where
    put = putGenericSum @DMagicSum
        (\p -> put (fromIntegral @_ @Word8 (natVal' p)))

instance Get DMagicSum where
    get = getGenericSum @DMagicSum
        (\p -> fromIntegral @_ @Word8 (natVal' p))
        (==)
