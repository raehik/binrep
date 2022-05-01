module Binrep.Example.Tiff where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int

import GHC.Generics ( Generic )
import Data.Data ( Typeable, Data )

import Data.ByteString qualified as B

type W8 = I 'U 'I1 'LE

brCfgNoSum :: BR.Cfg W8
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

data Tiff where
    Tiff :: TiffBody e -> Tiff
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
        c1 <- get @W8
        c2 <- get @W8
        case (c1, c2) of
          (0x49, 0x49) -> do
              body <- get @(TiffBody 'LE)
              return $ Tiff body
          (0x4d, 0x4d) -> do
              body <- get @(TiffBody 'BE)
              return $ Tiff body
          _ -> fail "bad TIFF header"

tiffLEbs :: B.ByteString
tiffLEbs = B.pack [0x49, 0x49, 0xFF, 0x00, 0x00, 0x00]

tiffBEbs :: B.ByteString
tiffBEbs = B.pack [0x4d, 0x4d, 0x00, 0x00, 0x00, 0xFF]
