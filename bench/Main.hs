{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Gauge

import Binrep
import Binrep.Generic
import Binrep.Type.NullTerminated
import Data.ByteString qualified as B
import Refined

import GHC.Generics ( Generic )
import Data.Word

{-
data X = X Word8 Word16 Word8
    deriving stock (Generic)

instance Put X where put = putGeneric c
instance Put' X where put' = put'Generic
instance BLen X where blen _ = 4
-}

data X3
    = X31 Word8
    | X32 Word8
    | X33 Word8 (NullTerminated B.ByteString) X3
    deriving stock (Generic)

instance BLen X3 where blen = blenGenericSum cDef
instance Put  X3 where put  = putGenericSum  cDef

x33 :: X3
x33 =
      X33 001 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X33 002 $$(refineTH "hi, cstring here")
    $ X33 003 $$(refineTH "hi, cstring here")
    $ X33 004 $$(refineTH "hi, cstring here")
    $ X33 005 $$(refineTH "hi, cstring here")
    $ X33 006 $$(refineTH "hi, cstring here")
    $ X33 007 $$(refineTH "hi, cstring here")
    $ X32 008

main :: IO ()
main = defaultMain
  [ bgroup "tiny"
    [ bench "put"  $ whnf Binrep.runPut               x33
    ]
  ]