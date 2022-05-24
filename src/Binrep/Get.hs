{-# LANGUAGE FunctionalDependencies #-}

module Binrep.Get
  ( Getter, Get(..), runGet, runGetter
  , GetWith(..), runGetWith
  ) where

import FlatParse.Basic qualified as FP
import FlatParse.Basic ( Parser )
import Data.ByteString qualified as B
import Data.Word
import Data.Int
import GHC.Exts

type Getter a = Parser String a

class Get a where
    -- | Parse from binary.
    get :: Getter a

runGet :: Get a => B.ByteString -> Either String (a, B.ByteString)
runGet = runGetter get

runGetter :: Getter a -> B.ByteString -> Either String (a, B.ByteString)
runGetter g bs = case FP.runParser g bs of
                   FP.OK a bs' -> Right (a, bs')
                   FP.Fail     -> Left "TODO fail"
                   FP.Err e    -> Left e

-- | Parse heterogeneous lists in order. No length indicator, so either fails or
--   succeeds by reaching EOF. Probably not what you usually want, but sometimes
--   used at the "top" of binary formats.
instance Get a => Get [a] where
    get = do as <- FP.many get
             FP.isEof >>= \case
               True  -> return as
               False -> error "TODO fail"

instance (Get a, Get b) => Get (a, b) where
    get = do
        a <- get
        b <- get
        return (a, b)

instance Get B.ByteString where
    get = FP.takeRest

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
    => r -> B.ByteString -> Either String (a, B.ByteString)
runGetWith r bs = runGetter (getWith r) bs
