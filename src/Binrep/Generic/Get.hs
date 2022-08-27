{-# LANGUAGE UndecidableInstances #-} -- required for TypeError >:(

module Binrep.Generic.Get where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.Get
import Binrep.Generic.Internal
import Util.Generic

import FlatParse.Basic qualified as FP
import Control.Applicative ( (<|>) )

import Numeric.Natural

getGeneric :: (Generic a, GGetD (Rep a), Get w) => Cfg w -> Getter a
getGeneric cfg = to <$> ggetD cfg

class GGetD f where
    ggetD :: Get w => Cfg w -> Getter (f a)

instance (GGetC f, Datatype d) => GGetD (D1 d f) where
    ggetD cfg = M1 <$> ggetC cfg (datatypeName' @d)

class GGetC f where
    ggetC :: Get w => Cfg w -> String -> Getter (f a)

-- | Refuse to derive instance for empty data types.
instance TypeError GErrRefuseVoid => GGetC V1 where
    ggetC = undefined

-- | TODO: Non-sum data types.
instance (GGetS f, Constructor c) => GGetC (C1 c f) where
    ggetC cfg dStr = (M1 . snd) <$> ggetS cfg dStr (conName' @c) 0

class GGetS f where
    ggetS :: Get w => Cfg w -> String -> String -> Natural -> Getter (Natural, (f a))

-- | The empty constructor trivially succeeds without parsing anything.
instance GGetS U1 where
    ggetS _ _ _ fIdx = pure (fIdx, U1)

instance (GGetS l, GGetS r) => GGetS (l :*: r) where
    ggetS cfg dStr cStr fIdx = do
        (fIdx',  l) <- ggetS cfg dStr cStr fIdx
        (fIdx'', r) <- ggetS cfg dStr cStr (fIdx'+1)
        pure (fIdx'', l :*: r)

instance (Get a, Selector s) => GGetS (S1 s (Rec0 a)) where
    ggetS _ dStr cStr fIdx = do
        a <- getEWrap $ EGeneric dStr . EGenericField cStr sStr fIdx
        pure (fIdx, M1 (K1 a))
      where
        sStr = selName'' @s

--------------------------------------------------------------------------------

-- | Constructor sums are differentiated by a prefix tag.
instance GGetCSum (l :+: r) => GGetC (l :+: r) where
    ggetC cfg dStr = do
        tag <- getEWrap $ EGeneric dStr . EGenericSum . EGenericSumTag
        case ggetCSum cfg dStr tag of
          Just parser -> parser
          Nothing -> do
            let tagPretty = cSumTagShow cfg $ tag
            FP.err $ EGeneric dStr $ EGenericSum $ EGenericSumTagNoMatch [] tagPretty

-- | TODO: Want to return an @Either [(String, Text)]@ indicating the
-- constructors and their expected tags tested, but needs fiddling (can't use
-- 'Alternative'). Pretty minor, but Aeson does it and it's nice.
class GGetCSum f where
    ggetCSum :: Get w => Cfg w -> String -> w -> Maybe (Getter (f a))

instance (GGetCSum l, GGetCSum r) => GGetCSum (l :+: r) where
    ggetCSum cfg dStr tag = l <|> r
      where
        l = fmap L1 <$> ggetCSum cfg dStr tag
        r = fmap R1 <$> ggetCSum cfg dStr tag

instance (GGetS f, Constructor c) => GGetCSum (C1 c f) where
    ggetCSum cfg dStr tag =
        let cStr = conName' @c
            cTag = (cSumTag cfg) cStr
        in  if   (cSumTagEq cfg) tag cTag
            then Just ((M1 . snd) <$> ggetS cfg dStr cStr 0)
            else Nothing
