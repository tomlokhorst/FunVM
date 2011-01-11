-- Local beta reduction

module FunVM.Transformations.SimplePartialEvaluator
  ( transform
  , bindEval
  , typeEval
  , valBindEval
  , valEval
  , exprEval
  , groupEval
  , moduleEval
  ) where

import Data.Function
import Data.List
import Data.Maybe

import FunVM.Core


type Env = [(Id, Expr)]

mnm :: String
mnm = "FunVM.Transformations.SimplePartialEvaluator"

transform :: Module -> Module
transform = moduleEval []

bindEval :: Env -> Bind -> Bind
bindEval env (TermPat x t) = TermPat x (typeEval env t)
bindEval _   (TypePat x k) = TypePat x k

typeEval :: Env -> Type -> Type
typeEval _   (Base b)    = Base b
typeEval env (TyVar nm)  = maybe (TyVar nm) unLit $ lookup nm env
  where
    unLit (Val (Lit (Type t))) = t
    unLit e                    = error $ mnm ++ ".typeEval: can't use `" ++ show e ++ "' as type"
typeEval env (Fun bs ts) = Fun (map (bindEval env) bs)
                               (map (typeEval $ map bindId bs `removeEnv` env) ts)
typeEval env (Lazy ts)   = Lazy (map (typeEval env) ts)
typeEval _   Any         = Any

valBindEval :: Env -> ValBind -> ValBind
valBindEval env (Bind b v) = Bind (bindEval env b) (valEval env v)

valEval :: Env -> Val -> Val
valEval _   (Lit l)    = Lit l
valEval env (Lam bs e) = Lam (map (bindEval env) bs)
                             (exprEval (map bindId bs `removeEnv` env) e)
valEval env (Delay e)  = Delay (exprEval env e)
valEval env (Prim s t) = Prim s (typeEval env t)

exprEval :: Env -> Expr -> Expr
exprEval env (Val v)        = Val (valEval env v)
exprEval env (Var nm)       = fromMaybe (Var nm) $ lookup nm env
exprEval env (App e1 e2)    = App (exprEval env e1) (exprEval env e2)
exprEval env (Multi es)     = Multi (map (exprEval env) es)
exprEval env (Force e)      = Force (exprEval env e)
exprEval env (Let bs e1 e2) = Let (map (bindEval env) bs)
                                  (exprEval env e1)
                                  (exprEval (map bindId bs `removeEnv` env) e2)
exprEval env (LetRec vbs e) = LetRec (groupEval env vbs)
                                     (exprEval env' e)
  where
    env' = map valBindId vbs `removeEnv` env

groupEval :: Env -> Group -> Group
groupEval env vbs = map (valBindEval env') vbs
  where
    env' = map valBindId vbs `removeEnv` env

moduleEval :: Env -> Module -> Module
moduleEval env (Module nm is bgs) = Module nm is (map (groupEval env) bgs)

removeEnv :: [Id] -> Env -> Env
removeEnv ids env = foldr (\x env' -> deleteBy ((==) `on` fst) (x, undefined) env') env ids

