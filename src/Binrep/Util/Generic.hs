module Binrep.Util.Generic where

import GHC.TypeLits

-- | Common type error string for when GHC is asked to derive a non-sum
--   instance, but the data type in question turns out to be a sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedSum =
    'Text "Cannot derive non-sum binary representation instance for sum data type"

-- | Common type error string for when GHC is asked to derive a sum instance,
--   but the data type in question turns out to be a non-sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedNonSum =
    'Text "Refusing to derive sum binary representation instance for non-sum data type"
