module Data.Store.Aeson where

import Data.Text qualified as Text
import Data.Text ( Text )
import Data.Char qualified as Char
import GHC.Exts
import Foreign
import Data.Store qualified as Store
import Data.Text.Array qualified as Text.A
import Data.Text.Internal ( Text(..) )

blenTextUtf8AesonEscapeAsciiChar :: Word8 -> Int
blenTextUtf8AesonEscapeAsciiChar w8
  | w8 == c2w '\\' = 2
  | w8 == c2w '\"' = 2
  | w8 >= 0x20 = 1
  | w8 == c2w '\n' = 2
  | w8 == c2w '\r'  = 2
  | w8 == c2w '\t'  = 2
  | otherwise = 6
{-# inline blenTextUtf8AesonEscapeAsciiChar #-}

blenTextUtf8AesonEscape :: Text -> Int
blenTextUtf8AesonEscape = Text.foldl' go 0
  where
    go i c =
        if   Char.isAscii c
        then i + blenTextUtf8AesonEscapeAsciiChar (c2w c)
        else i + utf8Length c
{-# inline blenTextUtf8AesonEscape #-}

-- from 'Data.Text.Internal.Encoding.Utf8.utf8Length' (text 2.0)
utf8Length :: Char -> Int
utf8Length (C# c) = I# ((1# +# geChar# c (chr# 0x80#)) +# (geChar# c (chr# 0x800#) +# geChar# c (chr# 0x10000#)))
{-# inline utf8Length #-}

-- | TODO convenience. should be no-op. can we trust it?
c2w :: Char -> Word8
c2w = fromIntegral . Char.ord
{-# inline c2w #-}

putTextUtf8AesonEscape :: Text -> Store.Poke ()
putTextUtf8AesonEscape = undefined
{-# inline putTextUtf8AesonEscape #-}

{-# inline encodeUtf8BuilderEscaped #-}
encodeUtf8BuilderEscaped
    :: (Ptr Word8 -> Word8 -> IO (Ptr Word8)) -> Text -> Ptr Word8 -> IO (Ptr Word8)
encodeUtf8BuilderEscaped f (Text arr os len) = asdf f arr (os+len) os

asdf
    :: (Ptr Word8 -> Word8 -> IO (Ptr Word8))
    -> Text.A.Array -> Int -> Int -> Ptr Word8 -> IO (Ptr Word8)
asdf pokeEscapeAscii arr iend = go
  where
    go i p
     | i >= iend = pure p
     | otherwise =
        let w8 = unsafeIndex' arr i
        in  if   w8 < 0x80
            then pokeEscapeAscii p w8 >>= go (i+1)
            else poke p w8 >> go (i+1) (p `plusPtr` 1)

unsafeIndex' :: Text.A.Array -> Int -> Word8
unsafeIndex' = Text.A.unsafeIndex -- error "text-2.0 only"

escapeAscii :: Ptr Word8 -> Word8 -> IO (Ptr Word8)
escapeAscii p w8
 | w8 == c2w '\\' = do
    poke p (c2w '\\')
    poke (p `plusPtr` 1) (c2w '\\')
    pure (p `plusPtr` 2)
 | w8 == c2w '\"' = do
    poke p (c2w '\\')
    poke (p `plusPtr` 1) (c2w '\"')
    pure (p `plusPtr` 2)
 | otherwise = do
    poke p w8
    pure (p `plusPtr` 1)
