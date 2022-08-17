{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Binrep.Example.Tiff where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.Magic
import Binrep.Type.Byte
import FlatParse.Basic ( (<|>) )

import GHC.Generics ( Generic )
import Data.Data ( Data, Typeable )
import GHC.TypeLits

import Data.ByteString qualified as B

type W8 = I 'U 'I1 'LE

brCfgNoSum :: BR.Cfg W8
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

data Tiff where
    Tiff :: (Put (I 'U 'I4 end), bs ~ MagicBytes (TiffMagic end), ReifyBytes bs, KnownNat (Length bs)) => TiffBody end -> Tiff

instance Show Tiff where
    show (Tiff body) = "Tiff " <> show body

data TiffBody (end :: Endianness) = TiffBody
  { tiffBodyMagic :: Magic (TiffMagic end)
  , tiffBodyExInt :: I 'U 'I4 end
  } deriving stock (Generic, Show, Eq)
deriving stock instance (KnownSymbol (TiffMagic end), Typeable end) => Data (TiffBody end)

instance (bs ~ MagicBytes (TiffMagic end), KnownNat (Length bs)) => BLen (TiffBody end) where
    blen = blenGeneric brCfgNoSum
instance (bs ~ MagicBytes (TiffMagic end), ReifyBytes bs, irep ~ I 'U 'I4 end, Put irep) => Put  (TiffBody end) where
    put  = putGeneric  brCfgNoSum
instance (bs ~ MagicBytes (TiffMagic end), ReifyBytes bs, irep ~ I 'U 'I4 end, Get irep) => Get  (TiffBody end) where
    get  = getGeneric  brCfgNoSum

instance BLen Tiff where
    blen (Tiff body) = blen body

instance Put Tiff where
    put (Tiff body) = put body

instance Get Tiff where
    get = fmap Tiff (get @(TiffBody 'LE)) <|> fmap Tiff (get @(TiffBody 'BE))

type family TiffMagic (end :: Endianness) :: Symbol where
    TiffMagic 'LE = "II"
    TiffMagic 'BE = "MM"

tiffLEbs :: B.ByteString
tiffLEbs = B.pack [0x49, 0x49, 0xFF, 0x00, 0x00, 0x00]

tiffBEbs :: B.ByteString
tiffBEbs = B.pack [0x4D, 0x4D, 0x00, 0x00, 0x00, 0xFF]
