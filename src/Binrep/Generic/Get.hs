module Binrep.Generic.Get where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.Get
import Binrep.Generic.Internal

import Data.Serialize.Get qualified as Cereal
import Numeric ( showHex )
import Control.Applicative ( (<|>) )

getGeneric :: (Generic a, GGet (Rep a), Get w, Integral w, Show w) => Cfg w -> Cereal.Get a
getGeneric cfg = to <$> gget cfg

class GGet f where
    gget :: (Get w, Integral w, Show w) => Cfg w -> Cereal.Get (f a)

-- | Empty constructor.
instance GGet U1 where
    gget _ = return U1

-- | Field.
instance Get c => GGet (K1 i c) where
    gget _ = K1 <$> get

-- | Product type fields are consecutive.
instance (GGet l, GGet r) => GGet (l :*: r) where
    gget cfg = do l <- gget cfg
                  r <- gget cfg
                  return $ l :*: r

-- | Constructor sums are differentiated by a prefix tag.
instance GGetSum (l :+: r) => GGet (l :+: r) where
    gget cfg = do
        tag <- get
        case ggetsum cfg tag of
          Just parser -> parser
          Nothing -> fail $ "invalid sum type tag: "<>showHex tag ""

-- | Refuse to derive instance for void datatype.
instance TypeError GErrRefuseVoid => GGet V1 where
    gget = undefined

-- | Any datatype, constructor or record.
instance GGet f => GGet (M1 i d f) where
    gget cfg = M1 <$> gget cfg

--------------------------------------------------------------------------------

class GGetSum f where
    ggetsum :: (Get w, Integral w, Show w) => Cfg w -> w -> Maybe (Cereal.Get (f a))

instance (GGetSum l, GGetSum r) => GGetSum (l :+: r) where
    ggetsum cfg tag = l <|> r
      where
        l = fmap L1 <$> ggetsum cfg tag
        r = fmap R1 <$> ggetsum cfg tag

-- | Bad. Need to wrap this like SumFromString in Aeson.
instance (GGet r, Constructor c) => GGetSum (C1 c r) where
    ggetsum cfg tag
     | tag == (cSumTag cfg) (conName' @c) = Just $ gget cfg
     | otherwise = Nothing
