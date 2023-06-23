-- | Common descriptions for generic data type errors.

module Generic.Data.Function.Error where

import GHC.TypeLits ( ErrorMessage(Text) )

-- | Common type error string for when you attempt to use a generic instance
--   at an empty data type (e.g. 'Data.Void.Void', 'GHC.Generics.V1').
type ENoEmpty = 'Text "Requested generic instance disallows empty data type"

-- | Common type error string for when GHC is asked to derive a non-sum
--   instance, but the data type in question turns out to be a sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedSum =
    'Text "Cannot derive non-sum generic instance for sum data type"

-- | Common type error string for when GHC is asked to derive a sum instance,
--   but the data type in question turns out to be a non-sum data type.
--
-- No need to add the data type name here, since GHC's context includes the
-- surrounding instance declaration.
type EUnexpectedNonSum =
    'Text "Refusing to derive sum generic instance for non-sum data type"
