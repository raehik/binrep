module Binrep.Generic.Internal where

import GHC.TypeLits
import Data.Text ( Text )

data Cfg a = Cfg
  { cSumTag :: String -> a
  -- ^ How to turn a constructor name into a byte tag.

  , cSumTagEq   :: a -> a -> Bool
  , cSumTagShow :: a -> Text
  }

-- | Common type error string for when GHC attempts to derive an binrep instance
--   for a (the?) void datatype @V1@.
type GErrRefuseVoid =
    'Text "Refusing to derive binary representation for void datatype"
