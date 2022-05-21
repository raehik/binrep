module Binrep.Example.Tar where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.NullPadded
import Binrep.Type.AsciiNat

import GHC.Generics ( Generic )

import Data.Word ( Word8 )

import GHC.TypeNats

import Data.ByteString qualified as B

import FlatParse.Basic qualified as FP

type BS = B.ByteString

brCfgNoSum :: BR.Cfg (I 'U 'I1 'LE)
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

-- | The naturals in tars are sized octal ASCII digit strings that end with a
--   null byte (and may start with leading ASCII zeroes). The size includes the
--   terminating null, so you get @n-1@ digits. What a farce.
--
-- Don't use this constructor directly! The size must be checked to ensure it
-- fits.
newtype TarNat n = TarNat { getTarNat :: AsciiNat 8 }
    deriving stock (Generic, Show, Eq)

type instance CBLen (TarNat n) = n
instance KnownNat n => BLen (TarNat n)

-- | No need to check for underflow etc. as TarNat guarantees good sizing.
instance KnownNat n => Put (TarNat n) where
    put (TarNat an) = put pfxNulls <> put an <> put @Word8 0x00
      where
        pfxNulls = B.replicate (fromIntegral pfxNullCount) 0x30
        pfxNullCount = n - blen an - 1
        n = typeNatToBLen @n

instance KnownNat n => Get (TarNat n) where
    get = do
        an <- FP.isolate (fromIntegral (n - 1)) get
        get @Word8 >>= \case
          0x00 -> return $ TarNat an
          w    -> FP.err $ "TODO expected null byte, got " <> show w
      where
        n = typeNatToBLen @n

-- Partial header
data Tar = Tar
  { tarFileName :: NullPadded 100 BS
  , tarFileMode :: TarNat 8
  , tarFileUIDOwner :: TarNat 8
  , tarFileUIDGroup :: TarNat 8
  , tarFileFileSize :: TarNat 12
  , tarFileLastMod :: TarNat 12
  } deriving stock (Generic, Show, Eq)

instance BLen Tar where blen = blenGeneric brCfgNoSum
instance Put  Tar where put  = putGeneric  brCfgNoSum
instance Get  Tar where get  = getGeneric  brCfgNoSum
