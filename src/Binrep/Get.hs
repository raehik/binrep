{-# LANGUAGE FunctionalDependencies #-}

module Binrep.Get
  ( Getter, Get(..), runGet, runGetter
  , E(..), EBase(..), EGeneric(..), EGenericSum(..)
  , eBase
  , getEWrap, getEBase
  , cutEBase
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

import Binrep.BLen ( BLenT )

import Numeric.Natural

type Getter a = FP.Parser E a

data E
  = EBase EBase

  | EGeneric String {- ^ datatype name -} EGeneric

    deriving stock (Eq, Show, Generic)

eBase :: EBase -> Getter a
eBase = FP.err . EBase

-- | TODO confirm correct operation (error combination)
getEWrap :: Get a => (E -> E) -> Getter a
getEWrap f = FP.cutting get (f $ EBase EFail) (\e _ -> f e)

getEBase :: Get a => EBase -> Getter a
getEBase = FP.cut get . EBase

cutEBase :: Getter a -> EBase -> Getter a
cutEBase f e = FP.cut f $ EBase e

data EBase
  = ENoVoid
  | EFail

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

  | ERanOut Natural
  -- ^ ran out of input, needed precisely @n@ bytes for this part (n > 0)

    deriving stock (Eq, Show, Generic)

data EGeneric
  = EGenericSum EGenericSum
  | EGenericField String (Maybe String) Natural E
    deriving stock (Eq, Show, Generic)

data EGenericSum
  = EGenericSumTag E
  | EGenericSumTagNoMatch [String] Text
    deriving stock (Eq, Show, Generic)

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
    get = eBase ENoVoid

-- | Parse heterogeneous lists in order. No length indicator, so either fails or
--   succeeds by reaching EOF. Probably not what you usually want, but sometimes
--   used at the "top" of binary formats.
instance Get a => Get [a] where
    get = go
      where
        go = do
            FP.withOption FP.eof (\() -> pure []) $ do
                a <- get
                as <- go
                pure $ a : as

instance (Get a, Get b) => Get (a, b) where
    get = do
        a <- get
        b <- get
        pure (a, b)

instance Get B.ByteString where
    get = FP.takeRestBs

instance Get Word8 where get = cutEBase FP.anyWord8 (ERanOut 1)
instance Get  Int8 where get = cutEBase FP.anyInt8  (ERanOut 1)

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
