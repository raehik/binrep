{-# OPTIONS_GHC -fno-warn-orphans #-}

module Binrep.Util.ByteOrder
  ( ByteOrder(..), ByteOrdered(..), type EndianSuffix
  , type LE, type BE, type Endian
  ) where

import Raehik.Compat.Data.Primitive.Types.Endian ( ByteOrdered(..) )
import GHC.ByteOrder ( ByteOrder(..) )
import Strongweak
import Data.Kind ( type Type )
import GHC.TypeLits ( type Symbol )

{- TODO
I should explain this better, because these strongweak instances effectively go
through two "layers" here: a, and ByteOrdered. That is, @'Weakened'
('ByteOrdered' end a) = 'Weakened' a@. This is good, because the newtype is
purely type info!! But it's also kinda weird? Like maybe I should rethink
instance design.

Perhaps I could provide a type for "coerce via" instances, which annotates
errors nicely (instead of just passing through)?
-}

deriving via (a :: Type) instance     Weaken a =>     Weaken (ByteOrdered end a)
deriving via (a :: Type) instance Strengthen a => Strengthen (ByteOrdered end a)

-- shorter names I originally used
type LE = 'LittleEndian
type BE =    'BigEndian
type Endian = ByteOrdered

type family EndianSuffix (end :: ByteOrder) :: Symbol where
    EndianSuffix 'LittleEndian = "LE"
    EndianSuffix    'BigEndian = "BE"
