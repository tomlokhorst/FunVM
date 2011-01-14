module FunVM.Transformations.InlineWrapperAtCallsites
  ( transform
  ) where

import Data.Maybe

import FunVM.Core

transform :: Module -> Module
transform (Module x is bgs) =
  Module x is (map (map fvb) bgs)
  where
    env      = concatMap (map (valBind (\b v -> (bindId b, v)))) bgs
    nms      = concatMap (map valBindId) bgs
    wrappers = filter (\nm -> nm ++ "_worker" `elem` nms) nms

    fvb :: ValBind -> ValBind
    fvb (Bind p v) = Bind p (fval v)

    fval :: Val -> Val
    fval (Lam bs e) = Lam bs (f e)
    fval (Delay e)  = Delay (f e)
    fval v          = v

    f :: Expr -> Expr
    f (Val v)        = Val (fval v)
    f (Var nm) | nm `elem` wrappers
                     = Val (fromJust $ lookup nm env)
    f (Var nm)       = Var nm
    f (App e1 e2)    = App (f e1) (f e2)
    f (Multi es)     = Multi (map f es)
    f (Force e)      = Force (f e)
    f (Let bs e1 e2) = Let bs e1 (f e2)
    f (LetRec vbs e) = LetRec vbs (f e)

