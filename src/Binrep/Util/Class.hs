module Binrep.Util.Class where

import GHC.TypeLits

-- | Common type error string for when you attempt to use a binrep instance at
--   an empty data type (e.g. 'Data.Void.Void', 'GHC.Generics.V1').
type ENoEmpty = 'Text "No binary representation for empty data type"

-- | Common type error string for when you attempt to use a binrep instance
--   at a sum data type
--   GHC is asked to derive a non-sum
--   instance, but the data type in question turns out to be a sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type ENoSum =
         'Text "No binary representation for unannotated sum data type"
    :$$: 'Text "Consider defining a custom data type"
    :<>: 'Text " and deriving a generic instance with explicit sum handling"
