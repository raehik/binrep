{-# LANGUAGE UndecidableInstances #-} -- required for TypeError >:(

{- Generic binrep serializers via bytezap.

In the name of explicitness, there are two generic serializers:

  * a non-sum type serializer, which only permits data types containing a single
    constructor
  * a sum type serializer, which only permits data types containing >1
    constructors

Neither serializer permits empty datatypes. Incorrect usage will result in a
descriptive type error.

Note that due to the design of 'Generic', instances for sum types will
inevitably have worse performance than hand-written instances. If you want the
best performance possible, write your own instance.

Note your type's 'Generic' instance _must_ be decorated with metadata. So only
use these with GHC's automatically derived 'Generic' instances.
-}

module Binrep.Put.Bytezap.Generic where

import GHC.Generics
import GHC.TypeError ( TypeError )
import Util.Generic ( conName' )
import Binrep.Util.Class
import Binrep.Util.Generic

import Bytezap ( Poke )
import Binrep.Put.Bytezap ( Put(..) )

-- | Serialize a term of the sum type @a@ via its 'Generic' instance.
--
-- You must provide a serializer for @a@'s constructors. This is regrettably
-- inefficient due to having to use 'String's. Alas. Do write your own instance
-- if you want better performance!
putGenericSum
    :: (Generic a, GPutDSum (Rep a)) => (String -> Poke) -> a -> Poke
putGenericSum f = gputDSum f . from

-- | Serialize a term of the non-sum type 'a' via its 'Generic' instance.
putGenericNonSum :: (Generic a, GPutDNonSum (Rep a)) => a -> Poke
putGenericNonSum = gputDNonSum . from

-- | Generic sum type serializer (data type/top level).
class GPutDSum f where gputDSum :: (String -> Poke) -> f p -> Poke

-- | Unwrap meta (data type/top level).
instance GPutDSum f => GPutDSum (D1 c f) where
    gputDSum f (M1 a) = gputDSum f a

-- | Serialize a term of a sum data type.
instance GPutCSum (l :+: r) => GPutDSum (l :+: r) where
    gputDSum = gputCSum

-- | Refuse to derive a non-sum instance if we expected a sum data type.
instance TypeError EUnexpectedNonSum => GPutDSum (C1 c f) where
    gputDSum = undefined

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GPutDSum V1 where
    gputDSum = undefined

-- | Generic putter (constructor sum level).
class GPutCSum f where gputCSum :: (String -> Poke) -> f p -> Poke

-- | Inspect constructor sum tree.
instance (GPutCSum l, GPutCSum r) => GPutCSum (l :+: r) where
    gputCSum f = \case L1 l -> gputCSum f l
                       R1 r -> gputCSum f r

-- | Serialize constructor prefix tag and constructor contents.
instance (Constructor c, GPutC f) => GPutCSum (C1 c f) where
    gputCSum putTag (M1 a) = putTag (conName' @c) <> gputC a

-- | Generic putter (constructor level).
class GPutC f where gputC :: f p -> Poke

-- | Serialize fields left to right.
instance (GPutC l, GPutC r) => GPutC (l :*: r) where
    gputC (l :*: r) = gputC l <> gputC r

-- | Serialize a field using its existing 'Put' instance.
instance Put a => GPutC (S1 c (Rec0 a)) where gputC (M1 (K1 a)) = put a

-- | Wow, look! Nothing!
instance GPutC U1 where gputC U1 = mempty

-- | Generic non-sum type serializer (data type/top level).
class GPutDNonSum f where gputDNonSum :: f p -> Poke

-- | Unwrap meta (data type/top level).
instance GPutDNonSum f => GPutDNonSum (D1 c f) where
    gputDNonSum (M1 a) = gputDNonSum a

-- | Refuse to derive a sum instance if we expected a non-sum data type.
instance TypeError EUnexpectedSum => GPutDNonSum (l :+: r) where
    gputDNonSum = undefined

-- | Serialize the single constructor of a non-sum data type.
instance GPutC f => GPutDNonSum (C1 c f) where
    gputDNonSum (M1 a) = gputC a

-- | Refuse to derive an instance for an empty data type.
instance TypeError ENoEmpty => GPutDNonSum V1 where
    gputDNonSum = undefined
