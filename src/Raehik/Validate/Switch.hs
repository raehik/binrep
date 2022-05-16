{- | Definitions to help with data type validation.

The concept here is to define two views over a data type:

  * "validated", meaning any predicates implied by the types have been asserted
  * "unvalidated", meaning predicates are not yet unasserted -- and simpler
    (e.g. mathematically idealized) types are used instead

TODO

  * typeclass methods should be internal, user should use methods that reorder
    the typevars for better visible type application
-}

module Raehik.Validate.Switch where

import Refined hiding ( strengthen, Weaken(..) )

import Data.Vector.Sized ( Vector )
import Data.Kind ( Type )
import Data.Word
import Data.Int
import Data.Text ( Text )
import GHC.TypeNats

import Data.ByteString qualified as B

data Validation = Validated | Unvalidated

-- | Obtain the weak representation of the given type.
type family Weak (a :: Type) :: Type

{-
-- primitives
type instance Weak [a] = [a]
-}

-- machine integers
type instance Weak Word8  = Natural
type instance Weak Word16 = Natural
type instance Weak Word32 = Natural
type instance Weak Word64 = Natural
type instance Weak Int8   = Integer
type instance Weak Int16  = Integer
type instance Weak Int32  = Integer
type instance Weak Int64  = Integer

-- other
type instance Weak (Vector n a) = [a]
type instance Weak (Refined p a) = a

{-
-- base types
type instance Weak Text = Text
type instance Weak Char = Char
type instance Weak B.ByteString = B.ByteString
-}

type family Switch (v :: Validation) a :: Type where
    Switch 'Validated   a = a
    Switch 'Unvalidated a = Weak a
