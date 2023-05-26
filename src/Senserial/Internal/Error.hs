-- | TODO 1 Nice type errors for generic instance derivation failures.

module Senserial.Internal.Error where

-- | TODO 2 Nice type errors for generic instance derivation failures.

import GHC.TypeLits ( ErrorMessage(Text) )

-- | Common type error string for when you attempt to use a senserial instance
--   at an empty data type (e.g. 'Data.Void.Void', 'GHC.Generics.V1').
type ENoEmpty = 'Text "Requested senserial instance disallows empty data type"

-- | Common type error string for when GHC is asked to derive a non-sum
--   instance, but the data type in question turns out to be a sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedSum =
    'Text "Cannot derive non-sum senserial instance for sum data type"

-- | Common type error string for when GHC is asked to derive a sum instance,
--   but the data type in question turns out to be a non-sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedNonSum =
    'Text "Refusing to derive sum senserial instance for non-sum data type"
