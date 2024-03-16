{- | "Thin" types which reference the parser input when gotten via 'Get'.

flatparse's @take@ family perform no copying-- instead, a bytestring is
manually constructed with the finalizer from the input bytestring. I'm not sure
I want this -- it sounds like a memory leak waiting to happen -- so I default to
copying to a new bytestring. This type allows recovering the efficient no-copy
behaviour.

TODO doing this the other way around would be simpler, and fit flatparse better.
All we need is such a class:

@
class Copy a where copy :: a -> a
instance Copy B.ByteString where copy = B.copy
@

But this just doesn't fly, because it would invert the behaviour.

-}

{-# LANGUAGE UndecidableInstances #-} -- for strongweak derivingvia

module Binrep.Type.Thin where

import Binrep

import FlatParse.Basic qualified as FP

import GHC.Generics ( Generic )
import Data.Data ( Data )
import GHC.Exts ( IsList )
import Data.String
import Control.DeepSeq
import Data.Functor.Identity
import Strongweak

import Data.ByteString qualified as B

newtype Thin a = Thin { unThin :: a }
    -- derive all instances that 'Data.ByteString.ByteString' has
    deriving stock (Generic, Data, Show, Read)
    deriving
      ( Eq, Ord, Semigroup, Monoid -- simple
      , NFData, IsString, IsList -- weird
      , BLen, Put -- binrep
      ) via a

    -- at the end of the day, we are the identity functor
    deriving (Weaken, Strengthen) via Identity a

instance Get (Thin B.ByteString) where get = Thin <$> FP.takeRest
