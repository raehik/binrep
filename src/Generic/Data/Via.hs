-- | Wrapper types for using with @DerivingVia@.
module Generic.Data.Via where

import GHC.Generics ( Generic )
import GHC.TypeLits ( ErrorMessage(Text) )

-- | Wrapper for using to derive instances via generics. Emit type error on
--   'Rec0' base case i.e. any non-empty constructor.
newtype NoRec0 a = NoRec0 { unNoRec0 :: a }
    deriving stock (Generic, Show)

type ENoRec0 =
    'Text "Cannot use generic function on NoRec0-wrapped type containing fields"

-- | Wrapper for using to derive instances via generics. Do nothing for 'Rec0'
--   base case i.e. every constructor field.
--
-- What "nothing" means precisely is dependent on the generic function. It
-- should be something unit-like, such as 'mempty' or 'empty'.
newtype EmptyRec0 a = EmptyRec0 { unEmptyRec0 :: a }
    deriving stock (Generic, Show)
