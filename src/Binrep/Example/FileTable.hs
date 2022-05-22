module Binrep.Example.FileTable where

import Binrep
import Refined hiding ( Weaken )
import Refined.Unsafe
import Binrep.Type.Common ( Endianness(..) )
import Binrep.Type.Int
import Binrep.Type.LenPfx
import FlatParse.Basic qualified as FP
import Data.ByteString qualified as B
import Strongweak
import Strongweak.Generic
--import Data.Map ( Map )
import GHC.Generics ( Generic )
import GHC.Exts
import Data.Vector.Sized qualified as V
import Data.Word

type BS = B.ByteString

-- We're unable to put one invariant in the types: an entry can't be placed past
-- the maximum offset. The only way to check that is to lay out the table.
--
-- This needs to be a newtype, because @Table 'Strong a@ means the above
-- invariant has been checked. That means we can't use the strongweak generics.
newtype Table s a = Table { unTable :: SW s (LenPfx 'I1 'LE (Entry s a)) }

instance (Put a, BLen a) => Put (Table 'Strong a) where put = putFileTable

putFileTable :: (Put a, BLen a) => Table 'Strong a -> Builder
putFileTable (Table a@(LenPfx es)) =
    let es' = V.map prepEntry es
        osBase = V.sum $ V.map (\(l, _, _) -> l) es'
    in  case V.foldl go ((fromIntegral osBase) - 1, mempty, mempty) es' of
          (_, bh, bd) -> put (lenPfxSize a) <> bh <> bd
  where
    go :: (Word8, Builder, Builder) -> (BLenT, Word8 -> Builder, BS) -> (Word8, Builder, Builder)
    go (os, bh, bd) (_, eh, ed) = (os+fromIntegral (B.length ed), bh<>eh os, bd<>put ed)

prepEntry :: (Put a, BLen a) => Entry 'Strong a -> (BLenT, Word8 -> Builder, BS)
prepEntry (Entry nm bs) = (l, b, bs')
  where
    bs' = unrefine bs
    b os =
        put nm <> put os <> put (fromIntegral (B.length bs') :: Word8)
    l = blen nm + 1 + 1 + blen bs'

instance Get a => Get (Table 'Strong a) where get = getFileTable

getFileTable :: Get a => Getter (Table 'Strong a)
getFileTable = FP.withAddr# $ \addr# -> Table <$> getLenPfx (getWith addr#)

{-
This is certainly a weird type.

  * Can use regular strongweak generics
  * Has no 'Get' instance
  * Has a 'GetWith Addr#' instance
  * Has no 'Put' instance
  * Has no 'PutWith' instance

You can't serialize an 'Entry' by itself, because it serializes to two
artifacts, a header entry and the associated data. Now I see why Kaitai Struct
was having trouble with serializing this sort of type.
-}
data Entry s a = Entry
  { entryName :: a
  , entryData :: SW s (Refined (SizeLessThan (IMax 'U 'I1)) BS)
  } deriving stock (Generic)
deriving stock instance Show a => Show (Entry 'Weak   a)
deriving stock instance Eq   a => Eq   (Entry 'Weak   a)
deriving stock instance Show a => Show (Entry 'Strong a)
deriving stock instance Eq   a => Eq   (Entry 'Strong a)
instance Weaken     (Entry 'Strong a) (Entry 'Weak   a) where weaken     = weakenGeneric
instance Strengthen (Entry 'Weak   a) (Entry 'Strong a) where strengthen = strengthenGeneric

instance Get a => GetWith Addr# (Entry 'Strong a) where getWith = getEntry

getEntry :: Get a => Addr# -> Getter (Entry 'Strong a)
getEntry addr# = do
    name <- get
    dat  <- FP.withAnyWord8# $ \offset# -> FP.withAnyWord8# $ \len# ->
        FP.lenAtOffset# addr# (w8i# offset#) (w8i# len#)
    return $ Entry name (reallyUnsafeRefine dat)

w8i# :: Word8# -> Int#
w8i# w# = word2Int# (word8ToWord# w#)

exBs :: BS
exBs = B.pack
  [ 0x02
  , 0x30, 0x31, 0x32, 0x00
  , 12 -- <- offset!!
  , 0x01
  , 0x39, 0x38, 0x00
  , 13
  , 0x01
  , 0xFF
  , 0xF0
  ]
