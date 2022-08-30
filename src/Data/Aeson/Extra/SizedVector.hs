{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Aeson.Extra.SizedVector where

import Data.Aeson
import Data.Vector.Generic.Sized.Internal qualified as VSI
import Data.Vector.Generic.Sized qualified as VS
import Data.Vector.Generic qualified as V
import GHC.TypeNats ( KnownNat )

instance ToJSON   (v a) => ToJSON   (VSI.Vector v n a) where
    toJSON     (VSI.Vector v) = toJSON     v
    toEncoding (VSI.Vector v) = toEncoding v
instance (FromJSON (v a), KnownNat n, V.Vector v a) => FromJSON (VSI.Vector v n a) where
    parseJSON j = do
        v <- parseJSON j
        case VS.toSized v of
          Nothing -> fail "TODO bad size"
          Just v' -> pure v'
