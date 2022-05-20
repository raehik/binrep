{-# LANGUAGE TemplateHaskell #-}

module Binrep.Wip.TH where

import Language.Haskell.TH.Syntax
import Binrep

x :: Name -> Q [Dec]
x nm = do
    TyConI (DataD _ _ _ _ cs _) <- reify nm
    -- [InstanceD _ cxt ty' decs] <- reifyInstances ''BLen [ty]
    [d| instance BLen $ty where blen _ = 5 |]
 where ty = return $ ConT nm
