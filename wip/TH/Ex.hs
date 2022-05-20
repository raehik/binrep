{-# LANGUAGE TemplateHaskell #-}

module Binrep.Wip.Ex where

import Binrep.Wip.TH
import Data.Word

data A = A Word8

x ''A
