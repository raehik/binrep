{-# LANGUAGE UndecidableInstances #-} -- required for TypeError >:(

module Binrep.BLen.Simple.Generic where

import GHC.Generics
import GHC.TypeLits ( TypeError )

import Binrep.BLen.Simple
import Binrep.Util.Class
import Binrep.Util.Generic
import Util.Generic

-- | Measure the byte length of a term of the sum type 'a' via its 'Generic'
--   instance.
--
-- You must provide a function to obtain the byte length for the prefix tag, via
-- inspecting the reified constructor names. This is regrettably inefficient.
-- Alas. Do write your own instance if you want better performance!
blenGenericSum :: (Generic a, GBLenDSum (Rep a)) => (String -> Int) -> a -> Int
blenGenericSum f = gblenDSum f . from

-- | Measure the byte length of a term of the non-sum type 'a' via its 'Generic'
--   instance.
blenGenericNonSum :: (Generic a, GBLenDNonSum (Rep a)) => a -> Int
blenGenericNonSum = gblenDNonSum . from

-- | Generic sum type measurer (data type/top level).
class GBLenDSum f where gblenDSum :: (String -> Int) -> f p -> Int

-- | Unwrap meta (data type/top level).
instance GBLenDSum f => GBLenDSum (D1 c f) where
    gblenDSum f (M1 a) = gblenDSum f a

-- | Measure a term of a sum data type.
instance GBLenCSum (l :+: r) => GBLenDSum (l :+: r) where
    gblenDSum = gblenCSum

-- | Refuse to derive a non-sum instance if we expected a sum data type.
instance TypeError EUnexpectedNonSum => GBLenDSum (C1 c f) where
    gblenDSum = undefined

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GBLenDSum V1 where
    gblenDSum = undefined

-- | Generic measurer (constructor sum level).
class GBLenCSum f where gblenCSum :: (String -> Int) -> f p -> Int

-- | Inspect constructor sum tree.
instance (GBLenCSum l, GBLenCSum r) => GBLenCSum (l :+: r) where
    gblenCSum f = \case L1 l -> gblenCSum f l
                        R1 r -> gblenCSum f r

-- | Sum lengths of constructor prefix tag and constructor contents.
instance (Constructor c, GBLenC f) => GBLenCSum (C1 c f) where
    gblenCSum f x = f (conName' @c) + gblenC (unM1 x)

-- | Generic measurer (constructor level).
class GBLenC f where gblenC :: f p -> Int

-- | Sum adjacent fields.
instance (GBLenC l, GBLenC r) => GBLenC (l :*: r) where
    gblenC (l :*: r) = gblenC l + gblenC r

-- | Measure a field using its existing 'BLen' instance.
instance BLen a => GBLenC (S1 c (Rec0 a)) where gblenC (M1 (K1 a)) = blen a

-- | Wow, look! Nothing!
instance GBLenC U1 where gblenC U1 = 0

-- | Generic non-sum type measurer (data type/top level).
class GBLenDNonSum f where gblenDNonSum :: f p -> Int

-- | Unwrap meta (data type/top level).
instance GBLenDNonSum f => GBLenDNonSum (D1 c f) where
    gblenDNonSum (M1 a) = gblenDNonSum a

-- | Refuse to derive a sum instance if we expected a non-sum data type.
instance TypeError EUnexpectedSum => GBLenDNonSum (l :+: r) where
    gblenDNonSum = undefined

-- | Measure the single constructor of a non-sum data type.
instance GBLenC f => GBLenDNonSum (C1 c f) where
    gblenDNonSum (M1 a) = gblenC a

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GBLenDNonSum V1 where
    gblenDNonSum = undefined
