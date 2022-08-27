-- TODO remove Show instance requirement

{-# LANGUAGE UndecidableInstances #-} -- required for TypeError >:(

module Binrep.Generic.Get where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.Get
import Binrep.Generic.Internal

import FlatParse.Basic qualified as FP
import Control.Applicative ( (<|>) )

getGeneric :: (Generic a, GGet (Rep a), Get w, Show w) => Cfg w -> Getter a
getGeneric cfg = to <$> gget cfg

class GGet f where
    gget :: (Get w, Show w) => Cfg w -> Getter (f a)

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
        tag <- getE $ EGenericSumTag
        case ggetsum cfg tag of
          Just parser -> parser
          Nothing -> errE $ eGenericSumTagInvalid tag

getE :: Get a => (E -> EGeneric) -> Getter a
getE f = FP.cutting get (EGeneric $ f $ EBase EFail) (\e _ -> EGeneric $ f e)

errE :: EGeneric -> Getter a
errE = FP.err . EGeneric

-- | Refuse to derive instance for void datatype.
instance TypeError GErrRefuseVoid => GGet V1 where
    gget = undefined

-- | Any datatype, constructor or record.
instance GGet f => GGet (M1 i d f) where
    gget cfg = M1 <$> gget cfg

--------------------------------------------------------------------------------

class GGetSum f where
    ggetsum :: (Get w, Show w) => Cfg w -> w -> Maybe (Getter (f a))

instance (GGetSum l, GGetSum r) => GGetSum (l :+: r) where
    ggetsum cfg tag = l <|> r
      where
        l = fmap L1 <$> ggetsum cfg tag
        r = fmap R1 <$> ggetsum cfg tag

-- | Bad. Need to wrap this like SumFromString in Aeson.
instance (GGet r, Constructor c) => GGetSum (C1 c r) where
    ggetsum cfg tag
     | (cSumTagEq cfg) tag ((cSumTag cfg) (conName' @c)) = Just $ gget cfg
     | otherwise = Nothing
