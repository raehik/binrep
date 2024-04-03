module Binrep.Put.Struct where

import Bytezap.Struct qualified as Struct
import Bytezap.Struct.Generic qualified as Struct
import Control.Monad.ST ( RealWorld )
import Raehik.Compat.Data.Primitive.Types ( Prim' )
import GHC.Generics
import Binrep.CBLen
import Binrep.Common.Via.Prim ( ViaPrim(..) )
import GHC.TypeLits ( KnownNat )

type PutterC = Struct.Poke RealWorld

-- | constant size putter
class PutC a where putC :: a -> PutterC

instance Struct.GPokeBase PutC where
    type GPokeBaseSt PutC   = RealWorld
    type GPokeBaseC  PutC a = PutC a
    gPokeBase = Struct.unPoke . putC
    type KnownSizeOf' PutC a = KnownNat (CBLen a)
    sizeOf' = reifyCBLenProxy#

-- | Serialize a term of the struct-like type @a@ via its 'Generic' instance.
putGenericStruct
    :: forall a
    .  ( Generic a, Struct.GPoke PutC (Rep a) )
    => a -> PutterC
putGenericStruct = Struct.Poke . Struct.gPoke @PutC . from

instance Prim' a => PutC (ViaPrim a) where
    putC = Struct.prim . unViaPrim
    {-# INLINE putC #-}
