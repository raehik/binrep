module Util where

import Type.Reflection ( typeRep, Typeable )

coerceBounded :: forall b n. (Integral b, Bounded b, Show b, Typeable b, Integral n, Show n, Typeable n) => n -> Either String b
coerceBounded n =
    if   n <= maxB && n >= minB then Right (fromIntegral n)
    else Left $ show (typeRep @n)<>" "<>show n<>" not well bounded for "<>show (typeRep @b)<>", require: "<>show minB<>" <= n <= "<>show maxB
  where
    maxB = fromIntegral @b @n maxBound
    minB = fromIntegral @b @n minBound
