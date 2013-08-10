module HaxParse.AST.TH where

import Language.Haskell.TH.Syntax

makeIsFns :: Name -> Q [Dec]
makeIsFns name = do inf <- reify name
                    case inf of TyConI (DataD _ _ _ recs _) -> fmap concat $ mapM makeIsFn recs
                                x -> error $ "Expected a datatype, got " ++ show x

makeIsFn :: Con -> Q [Dec]
makeIsFn (RecC con _) = do s <- sig
                           return [ SigD (mkName $ "is" ++ nameBase con) s
                                  , FunD (mkName $ "is" ++ nameBase con) [matchClause, noMatchClause] ]
    where sig = [t|$(return . ConT $ mkName "Event") -> Bool|]
          matchClause = Clause [RecP con []] (NormalB (ConE $ mkName "True")) []
          noMatchClause = Clause [WildP] (NormalB (ConE $ mkName "False")) []
makeIsFn (NormalC con _) = makeIsFn (RecC con [])
makeIsFn x = error $ "Unhandled variant " ++ show x
