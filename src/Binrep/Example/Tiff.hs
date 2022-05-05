{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Example.Tiff where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Data.Serialize.Get qualified as Cereal

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

import Data.ByteString qualified as B

type W8 = I 'U 'I1 'LE

brCfgNoSum :: BR.Cfg W8
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

data Tiff where
    Tiff :: (TiffHeader end, Put (I 'U 'I4 end)) => TiffBody end -> Tiff
    deriving stock (Typeable)

instance Show Tiff where
    show (Tiff body) = "Tiff " <> show body

data TiffBody (end :: Endianness) = TiffBody
  { tiffBodyExInt :: I 'U 'I4 end
  } deriving stock (Generic, Typeable, Data, Show, Eq)

instance BLen (TiffBody end) where
    blen = blenGeneric brCfgNoSum
instance (irep ~ I 'U 'I4 end, Put irep) => Put  (TiffBody end) where
    put  = putGeneric  brCfgNoSum
instance (irep ~ I 'U 'I4 end, Get irep) => Get  (TiffBody end) where
    get  = getGeneric  brCfgNoSum

instance Get Tiff where
    get = do
        let headLE = tiffHeader @'LE
            headBE = tiffHeader @'BE
        bs <- Cereal.getByteString 2
        if      bs == headLE then do
            body <- get @(TiffBody 'LE)
            return $ Tiff body
        else if bs == headBE then do
            body <- get @(TiffBody 'BE)
            return $ Tiff body
        else fail "bad TIFF header"

instance Put Tiff where
    put (Tiff (body :: TiffBody end)) = do
        put (tiffHeader @end) <> put body

class TiffHeader (end :: Endianness) where tiffHeader :: B.ByteString
instance TiffHeader 'LE where tiffHeader = B.pack [0x49, 0x49]
instance TiffHeader 'BE where tiffHeader = B.pack [0x4D, 0x4D]

tiffLEbs :: B.ByteString
tiffLEbs = B.pack [0x49, 0x49, 0xFF, 0x00, 0x00, 0x00]

tiffBEbs :: B.ByteString
tiffBEbs = B.pack [0x4D, 0x4D, 0x00, 0x00, 0x00, 0xFF]
