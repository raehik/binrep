module Senserial.Sequential.Internal.Builder where

import Data.Kind ( type Type, type Constraint )

{- | Sequential builders.

A type may be used as a sequential builder if it is a 'Monoid' and has a class
for purely converting from compatible types.

This is an sort of "enumeration" type class, which enables selecting a class
to use in a generic instance @S1@ base case.
-}
class Monoid bld => SeqBuilder bld where
    -- | Builder type class.
    type SeqBuilderC bld :: Type -> Constraint

    -- | Serialize to the selected builder using its type class.
    --
    -- This should be set to the appropriate function in the builder type class.
    toSeqBuilder :: SeqBuilderC bld a => a -> bld
