{-# LANGUAGE DoRec #-}
module FunVM.Evaluator
  ( Values
  , Value (..)
  , Env
  , eval
  ) where

import Control.Monad.Error
import Data.Function
import Data.List

import FunVM.Core.Build
import FunVM.Core.Destructors
import FunVM.Core.Pretty ()
import FunVM.Core.Syntax

-- | A value is multiple Vals
type Values = [Value]

data Value
  = ValBase   Literal
  | ValFun    Env [Bind] Expr
  | ValThunk  Env Expr
  | ValPrim   String
  deriving (Eq, Show)

type Env = [(Id, Value)]
type Eval a = Either String a

val :: Env -> Val -> Value
val _   (Lit   l)    = ValBase l
val env (Lam   ps e) = ValFun (closureEnv env e) ps e
val env (Delay e)    = ValThunk (closureEnv env e) e
val _   (Prim  s _)  = ValPrim s

eval :: Env -> Expr -> Eval Values
eval env (Val v)            = return [val env v]
eval env (Var x)            = maybe (throwError $ "Variable '" ++ x ++ "' not in environment.")
                                    (return . return)
                                    (lookup x env)
eval env (App e1 e2)        = do
  f <- eval env e1
  case f of
    [ValPrim s] -> do
      args <- eval env e2
      primFun s args
    [ValFun env' bs body] -> do
      env'' <- evalArgs env bs e2
      eval (env'' `mergeEnvs` env') body
    x -> throwError $ "Can't apply to non-function `" ++ show x ++ "'."
eval env (Let ps e1 e2)  = do
  vs <- eval env e1
  let env' = zip (map bindId ps) vs
  eval (env' `mergeEnvs` env) e2
eval env (LetRec bs e)     = do
  rec env' <- mapM (valBind (evalValBind (env' `mergeEnvs` env))) bs
  eval env' e
eval env (Multi es)         = concat `fmap` mapM (eval env) es
eval env (Force e)          = do
  v <- eval env e
  case v of
    [ValThunk env' e'] -> eval env' e'
    x -> throwError $ "Can't force non-thunk `" ++ show x ++ "'."

evalArgs :: Env -> [Bind] -> Expr -> Eval Env
evalArgs x y z = f x y [z]
  where
    f :: Env -> [Bind] -> [Expr] -> Eval Env
    f _   _  []              = return []
    f env bs (Multi es1:es2) = f env bs (es1 ++ es2)
    f env bs (e:es) = do
      vs <- eval env e
      let (bs1, bs2) = splitAt (length vs) bs
          env' = zip (map bindId bs1) vs
      (env' ++) `fmap` f (env' ++ env) bs2 es

evalValBind :: Env -> Bind -> Val -> Eval (Id, Value)
evalValBind env b v = do
  let x = bindId b
  let v'= val env v
  return $ (x, v')

closureEnv :: Env -> Expr -> Env
closureEnv env e = intersectBy ((==) `on` fst) env (map (\x -> (x, undefined)) (fv e))

mergeEnvs :: Env -> Env -> Env
mergeEnvs env1 env2 = nubBy ((==) `on` fst) (env1 ++ env2)

primFun :: String -> Values -> Eval Values
primFun "primAddInt32" [ValBase (Integer x _), ValBase (Integer y _)] = intVal (x + y)
primFun "primSubInt32" [ValBase (Integer x _), ValBase (Integer y _)] = intVal (x - y)
primFun "primMulInt32" [ValBase (Integer x _), ValBase (Integer y _)] = intVal (x * y)
primFun "primEqInt32"  [ValBase (Integer x _), ValBase (Integer y _)] = intVal (if x == y then 1 else 0)
primFun "primOrInt32"  [ValBase (Integer x _), ValBase (Integer y _)] = intVal (if x /= 0 || y /= 0 then 1 else 0)
primFun "primIfInt32"  [ValBase (Integer p _), _, x, y]  = return $ if p == 0 then [y] else [x]
primFun s _  = throwError $ "Unknown Prim call `" ++ s ++ "'"

intVal :: Integer -> Eval Values
intVal x = return [ValBase $ Integer x int32]

