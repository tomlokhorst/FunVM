module FunVM.WorkerWrapperTransformations.InlineWrapperAtCallsites
  ( transform
  ) where

import Data.Maybe
import Data.Monoid hiding (Any)

import FunVM.Core
import FunVM.Transformations.SimplePartialEvaluator
import qualified FunVM.WorkerWrapperTransformations.DefinitionSiteArityRaising as DAR

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
    f e@(App _ _) | appVar e `elem` map Just wrappers
                     = let nm   = fromJust $ appVar e
                           wrkr = fromJust $ lookup nm env
                       in inline mempty (Val wrkr) (argss e) --Var (">> " ++ show (argss e) ++ " <<")
    f (App e1 e2)    = App (f e1) (f e2)
    f (Multi es)     = Multi (map f es)
    f (Force e)      = Force (f e)
    f (Let bs e1 e2) = Let bs e1 (f e2)
    f (LetRec vbs e) = LetRec vbs (f e)

    appVar :: Expr -> Maybe Id
    appVar (Val (Delay e))  = appVar e
    appVar (Var nm)         = Just nm
    appVar (App e1       _) = appVar e1
    appVar (Force e)        = appVar e
    appVar (Let _ _ e2)     = appVar e2
    appVar (LetRec _ e)     = appVar e
    appVar _                = Nothing

    argss :: Expr -> [[Expr]]
    argss (App e1@App{} e2) = argss e1 ++ argss e2
    argss (App _        e2) = argss e2
    argss (Multi es)        = [es]
    argss e                 = [[e]]

inline :: Env -> Expr -> [[Expr]] -> Expr
inline env e          []    = exprEval env e
inline env (Val (Lam bs e)) (args:argss) = inline (mkEnv bs args `mappend` env) e argss
inline _   e          _     = e

