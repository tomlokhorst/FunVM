module FunVM.Transformations.WorkerWrapper.GenericTransform
  ( transform
  ) where

import Data.List
import Data.Maybe

import FunVM.Core

transform
  :: (ValBind -> Bool)
  -> (ValBind -> ValBind)
  -> (ValBind -> ValBind -> ValBind)
  -> Module
  -> Module
transform applicable updateWorker updateWrapper (Module x is bgs) =
  Module x is (map g bgs)
  where
    g :: Group -> Group
    g = map snd . reverse . foldl f []
    nms = concatMap (map valBindId) bgs

    -- Note: the ordering of workers and wrappers is important.
    -- Since this is a fold-left, workers must always apear before wrappers.
    -- This way, the worker can be found when calling the updateWrapper function
    f :: [(Id, ValBind)] -> ValBind -> [(Id, ValBind)]
    f env vb
      | applicable' && isWorker  = (nm, updateWorker vb) : env
      | applicable' && isWrapper = (nm, updateWrapper (fromJust $ lookup workerNm env) vb) : env
      | applicable'              = let uWorker  = updateWorker (newWorker vb)
                                       uWrapper = updateWrapper uWorker (newWrapper vb)
                                   in (workerNm, uWorker) : (nm, uWrapper) : env
      | otherwise                = (nm, vb) : env
      where
        nm          = valBindId vb
        workerNm    = nm ++ "_worker"
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

