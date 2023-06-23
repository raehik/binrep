-- | Wrapper types for using with @DerivingVia@.

module Generic.Data.Function.Via where

import GHC.Generics ( Generic )
import GHC.TypeLits ( ErrorMessage(Text) )
import Data.Functor.Identity ( Identity(..) )

-- | Wrapper for using to derive instances via generics. Emit type error on
--   'Rec0' base case i.e. any non-empty constructor.
newtype NoRec0 a = NoRec0 { unNoRec0 :: a }
    deriving stock (Generic, Show)
    deriving (Functor, Applicative, Monad) via Identity

type ENoRec0 =
    'Text "Cannot use generic function on NoRec0-wrapped type containing fields"

-- | Wrapper for using to derive instances via generics. Do nothing for 'Rec0'
--   base case i.e. every constructor field.
--
-- "nothing" probably means 'mempty', but *may* be another unit-like.
--
-- TODO This might not be useful. It's not "special" like 'NoRec0', it's
-- basically tied to 'Monoid'. So it's useful for 'foldMap', but kind of
-- arbitrary when applied to 'traverse'.
newtype EmptyRec0 a = EmptyRec0 { unEmptyRec0 :: a }
    deriving stock (Generic, Show)
    deriving (Functor, Applicative, Monad) via Identity
