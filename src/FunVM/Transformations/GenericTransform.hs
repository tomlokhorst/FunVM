module FunVM.Transformations.GenericTransform
  ( transform
  ) where

import Data.List

import FunVM.Core

transform
  :: (ValBind -> Bool)
  -> (ValBind -> ValBind)
  -> (ValBind -> ValBind -> ValBind)
  -> Module
  -> Module
transform applicable updateWorker updateWrapper (Module x is bgs) =
  Module x is (map (concatMap f) bgs)
  where
    nms = concatMap (map valBindId) bgs
    f :: ValBind -> [ValBind]
    f vb | applicable' && isWorker  = [updateWorker vb]
         | applicable' && isWrapper = [updateWrapper vb vb]
         | applicable' = let uWorker  = updateWorker (newWorker vb)
                             uWrapper = updateWrapper uWorker (newWrapper vb)
                         in [uWorker, uWrapper]
         | otherwise   = [vb]
      where
        nm = valBindId vb
        applicable' = applicable vb
        isWorker    = "_worker" `isSuffixOf` nm
        isWrapper   = nm ++ "_worker" `elem` nms

newWorker :: ValBind -> ValBind
newWorker (Bind (TermPat x t) v) = Bind (TermPat (x ++ "_worker") t) v
newWorker vb = vb

newWrapper :: ValBind -> ValBind
newWrapper (Bind (TermPat x t) v) =
  let workerNm    = x ++ "_worker"
      (bdss,ids)  = args (Val v)
      body        = foldl (@@) (Var workerNm) (map (map Var) ids)
      val         = case bdss of
                      (bd:bds) -> Lam bd (foldr (\ps e -> Val $ Lam ps e) body bds)
                      []       -> error "newWrapper: not a lambda"
  in Bind (TermPat x t) $ val
newWrapper vb = vb

args :: Expr -> ([[Bind]], [[Id]])
args (Val (Lam ps b)) = let (bds, ids) = args b
                        in (ps:bds, (map bindId ps):ids)
args (Let _ _ e)      = args e
args _                = ([], [])

