{- | Main end-user binrep module bundling most functionality.

Generics are bundled together in 'Binrep.Generic'.
-}

module Binrep
  ( module Binrep.BLen
  , module Binrep.Put
  , module Binrep.Get

  -- * Extras
  , blenViaPut
  ) where

import Binrep.BLen
import Binrep.Put
import Binrep.Get

-- | The length in bytes of a 'Put'-able type is the length of the serialized
--   term.
--
-- Do not use this in 'BLen' instances. It's intended as a proof, and
-- potentially for testing purposes. Calculating length in bytes shouldn't
-- involve serializing (it should be fast and use minimal memory).
blenViaPut :: Put a => a -> BLenT
blenViaPut = blen . runPut
