{- | Definitions to help with data type validation.

The concept here is to define two views over a data type:

  * "validated", meaning any predicates implied by the types have been asserted
  * "unvalidated", meaning predicates are not yet unasserted -- and simpler
    (e.g. mathematically idealized) types are used instead

TODO

  * typeclass methods should be internal, user should use methods that reorder
    the typevars for better visible type application
-}

{-# LANGUAGE FunctionalDependencies #-}

module Raehik.Validate where

import Refined hiding ( strengthen, Weaken(..) )
import Util ( coerceBounded )

import Data.Vector.Sized ( Vector )
import Data.Vector.Sized qualified as V
import Data.Kind ( Type )
import Data.Word
import Data.Int
import Data.Text ( Text )
import GHC.TypeNats

import Data.Either.Combinators ( mapLeft )

import Data.ByteString qualified as B

data Validation = Validated | Unvalidated

type family Weak (a :: Type) :: Type

-- primitives
type instance Weak [a] = [a]

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

-- base types
type instance Weak Text = Text
type instance Weak Char = Char
type instance Weak B.ByteString = B.ByteString

type family Switch (v :: Validation) a :: Type where
    Switch 'Validated   a = a
    Switch 'Unvalidated a = Weak a

class Weaken s w | s -> w where
    weaken :: s -> w

instance Weaken Word8  Natural where weaken = fromIntegral
instance Weaken Word16 Natural where weaken = fromIntegral
instance Weaken Word32 Natural where weaken = fromIntegral
instance Weaken Word64 Natural where weaken = fromIntegral
instance Weaken Int8   Integer where weaken = fromIntegral
instance Weaken Int16  Integer where weaken = fromIntegral
instance Weaken Int32  Integer where weaken = fromIntegral
instance Weaken Int64  Integer where weaken = fromIntegral

instance Weaken (Vector n a) [a] where
    weaken = V.toList

instance Weaken (Refined p a) a where
    weaken = unrefine

class Strengthen w s | s -> w where
    strengthen :: w -> Either String s

instance Strengthen Natural Word8  where strengthen = coerceBounded
instance Strengthen Natural Word16 where strengthen = coerceBounded
instance Strengthen Natural Word32 where strengthen = coerceBounded
instance Strengthen Natural Word64 where strengthen = coerceBounded
instance Strengthen Integer Int8   where strengthen = coerceBounded
instance Strengthen Integer Int16  where strengthen = coerceBounded
instance Strengthen Integer Int32  where strengthen = coerceBounded
instance Strengthen Integer Int64  where strengthen = coerceBounded

instance KnownNat n => Strengthen [a] (Vector n a) where
    strengthen w = case V.fromList w of Nothing -> Left "TODO bad size vector"
                                        Just s  -> Right s

instance Predicate p a => Strengthen a (Refined p a) where
    strengthen = mapLeft show . refine

{-
instance {-# OVERLAPPABLE #-} Weaken     w w where weaken     = id
instance {-# OVERLAPPABLE #-} Strengthen s s where strengthen = Right
-}
