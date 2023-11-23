{-# LANGUAGE UnboxedTuples #-}

module Bytezap where

import GHC.Exts
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B
import GHC.IO
import Data.Word

-- | TODO inner poke type
--
-- TODO can I change this to
--
-- @
-- Ptr Word8 -> IO (Ptr Word8)
-- @
--
-- without any performance loss? it's the same underneath newtypes and datas.
-- 'Ptr' is a data rather than a newtype, but IO is just a newtype.
--
-- I originally did this to beat ptr-poker, but idk. Now doubtful.
type Poke# = Addr# -> State# RealWorld -> (# State# RealWorld, Addr# #)

-- | Unboxed poke operation.
--
-- A newtype allows us a monoidal interface.
newtype Poke = Poke
  { -- | Write at an offset from an address and return the next offset.
    --
    -- The returned offset must be after the argument offset.
    --
    -- TODO I use that output order because it matches IO. Probs doesn't matter.
    unPoke :: Poke#
  }

-- | Construct a 'Poke'.
poke :: Poke# -> Poke
poke = Poke
{-# INLINE poke #-}

-- | Sequence two 'Poke's left-to-right.
instance Semigroup Poke where
    {-# INLINE (<>) #-}
    Poke l <> Poke r = Poke $ \addr# st# ->
        case l addr# st# of (# st'#, addr'# #) -> r addr'# st'#

-- | The empty 'Poke' simply returns its arguments.
instance Monoid Poke where
    {-# INLINE mempty #-}
    mempty = Poke $ \addr# st# -> (# st#, addr# #)

-- | Allocate a buffer of the given size and run a 'Poke' over it.
--
-- The 'Poke' must fill the buffer exactly. If it goes under, you should get
-- some random garbage at the end. If it goes over, your computer will probably
-- explode.
runPoke :: Int -> Poke -> B.ByteString
runPoke len = B.unsafeCreate len . wrapPoke
{-# INLINE runPoke #-}

wrapPoke :: Poke -> Ptr Word8 -> IO ()
wrapPoke (Poke p) (Ptr addr#) =
    IO (\st# -> case p addr# st# of (# l, _r #) -> (# l, () #))
{-# INLINE wrapPoke #-}
