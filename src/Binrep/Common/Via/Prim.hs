module Binrep.Common.Via.Prim where

-- | DerivingVia newtype for types which can borrow from 'Prim''.
newtype ViaPrim a = ViaPrim { unViaPrim :: a }
