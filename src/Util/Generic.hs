{-# LANGUAGE AllowAmbiguousTypes #-}

module Util.Generic where

import GHC.Generics

-- | 'datatypeName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
datatypeName' :: forall d. Datatype d => String
datatypeName' = datatypeName @d undefined

-- | 'conName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
conName' :: forall c. Constructor c => String
conName' = conName @c undefined

-- | 'selName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
selName' :: forall s. Selector s => String
selName' = selName @s undefined

-- | Get the record name for a selector if present.
--
-- On the type level, a 'Maybe Symbol' is stored for record names. But the
-- reification is done using @fromMaybe ""@. So we have to inspect the resulting
-- string to determine whether the field uses record syntax or not. (Silly.)
selName'' :: forall s. Selector s => Maybe String
selName'' = case selName' @s of "" -> Nothing
                                s  -> Just s
