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

module Binrep.Type.Thin where

import Binrep

import FlatParse.Basic qualified as FP
import Bytezap qualified as BZ
import Bytezap.Bytes qualified as BZ

import GHC.Generics ( Generic )
import Data.Data ( Data )
import GHC.IsList
import Data.String
import Control.DeepSeq
import Data.Functor.Identity
import Strongweak

import Data.ByteString qualified as B

newtype Thin a = Thin { unThin :: a }
    deriving stock (Generic, Data, Show)
    deriving
      ( Eq, Ord, Semigroup, Monoid, Read
      , NFData, IsString, IsList
      , BLen, Put
      ) via a
    deriving (Weaken, Strengthen) via (Identity a)

-- TODO strongweak. might be a derivingvia wrapper to write.

instance Get (Thin B.ByteString) where get = Thin <$> FP.takeRest
instance Get (Thin BZ.Write) where
    get = fmap Thin $ fmap BZ.byteString $ FP.takeRest
