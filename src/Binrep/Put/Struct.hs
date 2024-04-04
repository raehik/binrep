{-# LANGUAGE UndecidableInstances #-} -- for @KnownNat (CBLen a)@ in head

module Binrep.Put.Struct where

import Bytezap.Struct qualified as Struct
import Bytezap.Struct.Generic qualified as Struct
import Control.Monad.ST ( RealWorld )
import Binrep.CBLen
import GHC.TypeLits ( KnownNat )
import GHC.Generics
import Data.ByteString qualified as B

import Binrep.Common.Via.Prim ( ViaPrim(..) )
import Raehik.Compat.Data.Primitive.Types ( Prim' )
import Data.Word
import Data.Int
import Binrep.Util.ByteOrder
import Data.Functor.Identity
import Raehik.Compat.Data.Primitive.Types.Endian ( ByteSwap )

import Binrep.Common.Class.TypeErrors ( ENoSum, ENoEmpty )
import GHC.TypeLits ( TypeError )
import Data.Void

type PutterC = Struct.Poke RealWorld

-- | constant size putter
class PutC a where putC :: a -> PutterC

runPutC :: forall a. (PutC a, KnownNat (CBLen a)) => a -> B.ByteString
runPutC = Struct.unsafeRunPokeBS (cblen @a) . putC

instance Struct.GPokeBase PutC where
    type GPokeBaseSt PutC   = RealWorld
    type GPokeBaseC  PutC a = PutC a
    gPokeBase = Struct.unPoke . putC
    type KnownSizeOf' PutC a = KnownNat (CBLen a)
    sizeOf' = cblenProxy#

-- | Serialize a term of the struct-like type @a@ via its 'Generic' instance.
putGenericStruct
    :: forall a
    .  ( Generic a, Struct.GPoke PutC (Rep a) )
    => a -> PutterC
putGenericStruct = Struct.Poke . Struct.gPoke @PutC . from

instance Prim' a => PutC (ViaPrim a) where
    putC = Struct.prim . unViaPrim
    {-# INLINE putC #-}

instance TypeError ENoEmpty => PutC Void where putC = undefined
instance TypeError ENoSum => PutC (Either a b) where putC = undefined

instance PutC a => PutC (Identity a) where putC = putC . runIdentity

instance PutC PutterC where putC = id

-- | Unit type serializes to nothing. How zen.
instance PutC () where
    {-# INLINE putC #-}
    putC () = Struct.emptyPoke

-- | Look weird? Yeah. But it's correct :)
instance (PutC l, KnownNat (CBLen l), PutC r) => PutC (l, r) where
    {-# INLINE putC #-}
    putC (l, r) = Struct.sequencePokes (putC l) (cblen @l) (putC r)

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via ViaPrim Word8 instance PutC Word8

-- | 8-bit (1-byte) words do not require byte order in order to precisely
--   define their representation.
deriving via ViaPrim  Int8 instance PutC  Int8

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via Word8 instance PutC (ByteOrdered end Word8)

-- | Byte order is irrelevant for 8-bit (1-byte) words.
deriving via  Int8 instance PutC (ByteOrdered end  Int8)

-- ByteSwap is required on opposite endian platforms, but we're not checking
-- here, so make sure to keep it on both.
deriving via ViaPrim (ByteOrdered 'LittleEndian a)
    instance (Prim' a, ByteSwap a) => PutC (ByteOrdered 'LittleEndian a)
deriving via ViaPrim (ByteOrdered    'BigEndian a)
    instance (Prim' a, ByteSwap a) => PutC (ByteOrdered    'BigEndian a)
