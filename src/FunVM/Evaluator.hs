{-# LANGUAGE DoRec #-}
module FunVM.Evaluator
  ( Value
  , Val (..)
  , Env
  , eval
  ) where

import Control.Monad
import Control.Monad.Error
import Data.Function
import Data.List

import FunVM.Pretty ()
import FunVM.Syntax
import FunVM.Types

-- | A value is multiple Vals
type Value = [Val]

data Val
  = ValBase   Literal
  | ValFun    Env [Pattern] Expr
  | ValThunk  Env Expr
  | ValFFI    String
  deriving (Eq, Show)

type Env = [(Id, Value)]

eval :: Env -> Expr -> Either String Value
eval _    (Lit x)           = return [ValBase x]
eval env  (Var x)           = maybe (throwError $ "Variable '" ++ x ++ "' not in environment.")
                                    return
                                    (lookup x env)
eval env (App e es)         = do
  f <- eval env e
  case f of
    [ValFFI s] -> do
      args <- mapM (eval env) es
      ffi s args
    [ValFun env' ps body] -> do
      args <- mapM (eval env) es
      let env'' = zip (map patId ps) args ++ env'
      eval env'' body
    x -> throwError $ "Can't apply to non-function `" ++ show x ++ "'."
eval env (Lam ps e)         = return [ValFun (closureEnv env e) ps e]
eval env (Let NonRec bs e)  = do
  env' <- foldM evalBind env bs
  eval env' e
eval env (Let Rec bs e)     = do
  rec env' <- concat `fmap` mapM (evalBind (nub $ env' ++ env)) bs
  eval env' e
eval env (Multi es)         = concat `fmap` mapM (eval env) es
eval env (Delay e)          = return [ValThunk (closureEnv env e) e]
eval env (Force e)          = do
  v <- eval env e
  case v of
    [ValThunk env' e'] -> eval env' e'
    x -> throwError $ "Can't force non-thunk `" ++ show x ++ "'."
eval _   (FFI s _)          = return [ValFFI s]

evalBind :: Env -> Bind -> Either String Env
evalBind env (Bind ps e) = do
  vs <- eval env e
  let env' = zip (map patId ps) (map return vs)
  return $ mergeEnvs env' env

patId :: Pattern -> Id
patId = pattern (curry fst) (curry fst)

closureEnv :: Env -> Expr -> Env
closureEnv env e = intersectBy ((==) `on` fst) env (map (\x -> (x, undefined)) (fv e))

mergeEnvs :: Env -> Env -> Env
mergeEnvs env1 env2 = nubBy ((==) `on` fst) (env1 ++ env2)

ffi :: String -> [Value] -> Either String Value
ffi "primAdd" [[ValBase (Int x _)], [ValBase (Int y _)]] = return [ValBase $ Int (x + y) int32]
ffi "primSub" [[ValBase (Int x _)], [ValBase (Int y _)]] = return [ValBase $ Int (x - y) int32]
ffi "primMul" [[ValBase (Int x _)], [ValBase (Int y _)]] = return [ValBase $ Int (x * y) int32]
ffi "primEq"  [[ValBase (Int x _)], [ValBase (Int y _)]] = return [ValBase $ Int (if x == y then 1 else 0) int32]
ffi "primOr"  [[ValBase (Int x _)], [ValBase (Int y _)]] = return [ValBase $ Int (if x /= 0 || y /= 0 then 1 else 0) int32]
ffi "primIf"  [[ValBase (Int p _)], [_], x, y]  = return $ if p == 0 then y else x
ffi s _  = throwError $ "Unknown FFI call `" ++ s ++ "'"

