module Binrep.Example.Tar where

import Binrep
import Binrep.Generic
import Binrep.Generic qualified as BR
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Predicate.NullPad
import Binrep.Type.AsciiNat
import Binrep.Type.Sized

import GHC.Generics ( Generic )
import Data.Data ( Typeable )

import Data.Word ( Word8 )

import GHC.TypeLits

import Data.ByteString qualified as B

type BS = B.ByteString

brCfgNoSum :: BR.Cfg (I 'U 'I1 'LE)
brCfgNoSum = BR.Cfg { BR.cSumTag = undefined }

-- | The naturals in tars are octal ASCII digit strings, that end with a null
--   byte. What a farce.
newtype TarNat n = TarNat { getTarNat :: Sized n (AsciiNat 8) }
    deriving stock (Generic, Typeable)
    deriving (Show, Eq) via Sized n (AsciiNat 8)

instance KnownNat n => BLen (TarNat n) where blen (TarNat n) = blen n + 1
instance Put (TarNat n) where put (TarNat n) = put n <> put @Word8 0x00
instance KnownNat n => Get (TarNat n) where
    get = do
        n <- get
        get @Word8 >>= \case
          0x00 -> return $ TarNat n
          w    -> fail $ "TODO expected null byte, got " <> show w

-- Partial header
data Tar = Tar
  { tarFileName :: NullPadded 100 BS
  , tarFileMode :: TarNat 8
  , tarFileUIDOwner :: TarNat 8
  , tarFileUIDGroup :: TarNat 8
  , tarFileFileSize :: TarNat 12
  , tarFileLastMod :: TarNat 12
  } deriving stock (Generic, Typeable, Show, Eq)

instance BLen Tar where blen = blenGeneric brCfgNoSum
instance Put  Tar where put  = putGeneric  brCfgNoSum
instance Get  Tar where get  = getGeneric  brCfgNoSum
