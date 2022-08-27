{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Generic.Internal where

import GHC.TypeLits
import GHC.Generics

data Cfg a = Cfg
  { cSumTag :: String -> a
  -- ^ How to turn a constructor name into a byte tag.

  , cSumTagEq :: a -> a -> Bool
  }

-- | Common type error string for when GHC attempts to derive an binrep instance
--   for a (the?) void datatype @V1@.
type GErrRefuseVoid =
    'Text "Refusing to derive binary representation for void datatype"

-- | 'conName' without the value (only used as a proxy). Lets us push our
--   'undefined's into one place.
conName' :: forall c. Constructor c => String
conName' = conName @c undefined
