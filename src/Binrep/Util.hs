{-# LANGUAGE AllowAmbiguousTypes #-}

module Binrep.Util where

-- tshow
import Data.Text qualified as Text
import Data.Text ( Text )

-- posIntToNat
import GHC.Exts ( Int(..), int2Word# )
import GHC.Num.Natural ( Natural(..) )

-- natVal''
import GHC.TypeNats ( KnownNat, natVal' )
import GHC.Exts ( proxy#, Proxy# )

tshow :: Show a => a -> Text
tshow = Text.pack . show

-- | Convert some 'Int' @i@ where @i >= 0@ to a 'Natural'.
--
-- This is intended for wrapping the output of 'length' functions.
--
-- underflows if you call it with a negative 'Int' :)
posIntToNat :: Int -> Natural
posIntToNat (I# i#) = NS (int2Word# i#)
{-# INLINE posIntToNat #-}

natVal'' :: forall a. KnownNat a => Natural
natVal'' = natVal' (proxy# :: Proxy# a)
{-# INLINE natVal'' #-}
