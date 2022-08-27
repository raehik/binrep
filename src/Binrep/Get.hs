{-# LANGUAGE FunctionalDependencies #-}

module Binrep.Get
  ( Getter, Get(..), runGet, runGetter
  , E(..), EBase(..), eBase, EGeneric(..), eGenericSumTagInvalid
  , GetWith(..), runGetWith
  ) where

import FlatParse.Basic qualified as FP

import Data.ByteString qualified as B

import GHC.Exts ( TYPE, type LiftedRep )

import Data.Word
import Data.Int
import Data.Void ( Void )

import GHC.Generics ( Generic )

import Data.Text ( Text )
import Binrep.Util ( tshow )

import Binrep.BLen ( BLenT )

type Getter a = FP.Parser E a

data E
  = EBase EBase
  | EGeneric EGeneric
    deriving stock (Eq, Show, Generic)

eBase :: EBase -> Getter a
eBase = FP.err . EBase

data EBase
  = ENoVoid
  | EFail
  | EEof

  | EExpectedByte Word8 Word8
  -- ^ expected first, got second

  | EOverlong BLenT BLenT
  -- ^ expected first, got second

  | EExpected B.ByteString B.ByteString
  -- ^ expected first, got second

  | EFailNamed String
  -- ^ known fail

  | EFailParse String B.ByteString Word8
  -- ^ parse fail (where you parse a larger object, then a smaller one in it)

    deriving stock (Eq, Show, Generic)

data EGeneric
  = EGenericSumTag E
  | EGenericSumTagInvalid Text
    -- ^ TODO need either a typevar and instance, or some flatparse code to
    -- recover the bytes parsed for the sum tag (and use a 'ByteString')
    -- for now I've apparently already shoved a Show in there soooo using that.
    -- but not ideal
    deriving stock (Eq, Show, Generic)

eGenericSumTagInvalid :: Show a => a -> EGeneric
eGenericSumTagInvalid = EGenericSumTagInvalid . tshow

class Get a where
    -- | Parse from binary.
    get :: Getter a

runGet :: Get a => B.ByteString -> Either E (a, B.ByteString)
runGet = runGetter get

runGetter :: Getter a -> B.ByteString -> Either E (a, B.ByteString)
runGetter g bs = case FP.runParser g bs of
                   FP.OK a bs' -> Right (a, bs')
                   FP.Fail     -> Left $ EBase EFail
                   FP.Err e    -> Left e

-- | Impossible to parse 'Void'.
instance Get Void where
    get = FP.err $ EBase ENoVoid

-- | Parse heterogeneous lists in order. No length indicator, so either fails or
--   succeeds by reaching EOF. Probably not what you usually want, but sometimes
--   used at the "top" of binary formats.
instance Get a => Get [a] where
    get = do as <- FP.many get
             FP.cut FP.eof $ EBase EEof
             return as

instance (Get a, Get b) => Get (a, b) where
    get = do
        a <- get
        b <- get
        return (a, b)

instance Get B.ByteString where
    get = FP.takeRestBs

instance Get Word8 where get = FP.anyWord8
instance Get  Int8 where get = FP.anyInt8

-- | A type that can be parsed from binary given some environment.
--
-- Making this levity polymorphic makes things pretty strange, but is useful.
-- See @Binrep.Example.FileTable@.
class GetWith (r :: TYPE rep) a | a -> r where
    -- | Parse from binary with the given environment.
    getWith :: r -> Getter a
    -- can no longer provide default implementation due to levity polymorphism
    --default getWith :: Get a => r -> Getter a
    --getWith _ = get

--deriving anyclass instance Get a => GetWith r [a]

-- Note that @r@ is not levity polymorphic, GHC forces it to be lifted. You
-- can't bind (LHS) a levity polymorphic value.
runGetWith
    :: GetWith (r :: TYPE LiftedRep) a
    => r -> B.ByteString -> Either E (a, B.ByteString)
runGetWith r bs = runGetter (getWith r) bs
