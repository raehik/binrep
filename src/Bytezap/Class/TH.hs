{-# LANGUAGE TemplateHaskell #-}

-- adapted from Data.Store.TH.Internal

module Bytezap.Class.TH where

{-

import Bytezap
import Bytezap.Class

makePut :: Name -> Q [Dec]
makePut name = do
    dt <- reifyDataType name
    let preds = map (AppT ConT ''Put . VarT) (dtTvs dt)
        argTy = appsT (ConT name) (map VarT (dtTvs dt))
    (:[]) <$> derivePut preds argTy (dtCons dt)

makePutInstance :: Cxt -> Type -> Exp -> Dec
makePutInstance cs ty putExpr =
    plainInstanceD
        cs
        (AppT (ConT ''Put) ty)
        [ ValD (VarP 'put) (NormalB putExpr) []
        ]

derivePut :: Cxt -> Type -> [DataCon] -> Q Dec
derivePut preds headTy cons0 =
    makePutInstance preds headTy <$> putExpr
  where
    cons :: [(Name, [(Name, Type)])]
    cons =
      [ ( dcName dc
        , [ (mkName ("c" ++ show ixc ++ "f" ++ show ixf), ty)
          | ixf <- ints
          | (_, ty) <- dcFields dc
          ]
         )
      | ixc <- ints
      | dc <- cons0
      ]
    (tagType, _, tagSize) =
        fromMaybe (error "Too many constructors") $
        find (\(_, maxN, _) -> maxN >= length cons) tagTypes
    tagTypes :: [(Name, Int, Int)]
    tagTypes =
        [ ('(), 1, 0)
        , (''Word8,  fromIntegral (maxBound :: Word8),  1)
        , (''Word16, fromIntegral (maxBound :: Word16), 2)
        , (''Word32, fromIntegral (maxBound :: Word32), 4)
        , (''Word64, fromIntegral (maxBound :: Word64), 8)
    fName ix = mkName ("f" ++ show ix)
    ints = [0..] :: [Int]
    fNames = map fName ints
    sizeNames = zipWith (\_ -> mkName . ("sz" ++) . show) cons ints
    tagName = mkName "tag"
    valName = mkName "val"
    sizeExpr
        -- Maximum size of GHC tuples
        | length cons <= 62 =
            caseE (tupE (concatMap (map sizeAtType . snd) cons))
                  (case cons of
                     -- Avoid overlapping matches when the case expression is ()
                     [] -> [matchConstSize]
                     [c] | null (snd c) -> [matchConstSize]
                     _ -> [matchConstSize, matchVarSize])
        | otherwise = varSizeExpr
      where
        sizeAtType :: (Name, Type) -> ExpQ
        sizeAtType (_, ty) = [| size :: Size $(return ty) |]
        matchConstSize :: MatchQ
        matchConstSize = do
            let sz0 = VarE (mkName "sz0")
                sizeDecls =
                    if null sizeNames
                        then [valD (varP (mkName "sz0")) (normalB [| 0 |]) []]
                        else zipWith constSizeDec sizeNames cons
            sameSizeExpr <-
                case sizeNames of
                    (_ : tailSizeNames) ->
                        foldl (\l r -> [| $(l) && $(r) |]) [| True |] $
                        map (\szn -> [| $(return sz0) == $(varE szn) |]) tailSizeNames
                    [] -> [| True |]
            result <- [| ConstSize (tagSize + $(return sz0)) |]
            match (tupP (map (\(n, _) -> conP 'ConstSize [varP n])
                             (concatMap snd cons)))
                  (guardedB [return (NormalG sameSizeExpr, result)])
                  sizeDecls
        constSizeDec :: Name -> (Name, [(Name, Type)]) -> DecQ
        constSizeDec szn (_, []) =
            valD (varP szn) (normalB [| 0 |]) []
        constSizeDec szn (_, fields) =
            valD (varP szn) body []
          where
            body = normalB $
                foldl1 (\l r -> [| $(l) + $(r) |]) $
                map (\(sizeName, _) -> varE sizeName) fields
        matchVarSize :: MatchQ
        matchVarSize = do
            match (tupP (map (\(n, _) -> varP n) (concatMap snd cons)))
                  (normalB varSizeExpr)
                  []
        varSizeExpr :: ExpQ
        varSizeExpr =
            [| VarSize $ \x -> tagSize + $(caseE [| x |] (map matchVar cons)) |]
        matchVar :: (Name, [(Name, Type)]) -> MatchQ
        matchVar (cname, []) =
            match (conP cname []) (normalB [| 0 |]) []
        matchVar (cname, fields) =
            match (conP cname (zipWith (\_ fn -> varP fn) fields fNames))
                  body
                  []
          where
            body = normalB $
                foldl1 (\l r -> [| $(l) + $(r) |])
                (zipWith (\(sizeName, _) fn -> [| getSizeWith $(varE sizeName) $(varE fn) |])
                         fields
                         fNames)
    -- Choose a tag size large enough for this constructor count.
    -- Expression used for the definition of peek.
    peekExpr = case cons of
        [] -> [| error ("Attempting to peek type with no constructors (" ++ $(lift (show headTy)) ++ ")") |]
        [con] -> peekCon con
        _ -> doE
            [ bindS (varP tagName) [| peek |]
            , noBindS (caseE (sigE (varE tagName) (conT tagType))
                      (map peekMatch (zip [0..] cons) ++ [peekErr]))
            ]
    peekMatch (ix, con) = match (litP (IntegerL ix)) (normalB (peekCon con)) []
    peekErr = match wildP (normalB
        [| peekException $ T.pack $ "Found invalid tag while peeking (" ++ $(lift (show headTy)) ++ ")" |]) []
    peekCon (cname, fields) =
        case fields of
            [] -> [| pure $(conE cname) |]
            _ -> doE $
                map (\(fn, _) -> bindS (varP fn) [| peek |]) fields ++
               [noBindS $ appE (varE 'return) $ appsE $ conE cname : map (\(fn, _) -> varE fn) fields]

-}
