module FunVM.Syntax
  ( Id
  , Module (..)
  , Bind (..)
  , Pat (..)
  , Val (..)
  , Expr (..)
  , Literal (..)
  , fv
  ) where

import Data.List
import FunVM.Types

-- | Identifier for variables and other names
type Id = String

-- | Module definition
data Module
  = Module Id [Bind]
  deriving Eq

-- Value binding
data Bind
  = Bind Pat Expr
  deriving Eq

-- | Patterns in lambdas, lets or top level bindings
data Pat
  = TermPat  Id  Type
  | TypePat  Id  Kind
  deriving Eq

-- | Values in Weak Head Normal Form
data Val
  = Lit    Literal 
  | Lam    [Pat]  Expr
  | Delay  Expr
  deriving Eq

-- | Main expression data type
data Expr
  = Val     Val
  | Var     Id
  | App     Expr    Expr
  | Multi   [Expr]
  | Force   Expr
  | Let     [Pat]   Expr  Expr
  | LetRec  [Bind]  Expr
  | FFI     String  Type
  deriving Eq

-- | Literal integers, characters or strings
data Literal
  = Integer  Integer  Type
  | Char     Char
  | String   String
  | Type     Type
  deriving Eq

-- | Free value variables in expression
fv :: Expr -> [Id]
fv (Val (Lit     _))    = []
fv (Val (Lam     ps e)) = fv e \\ map patId ps
fv (Val (Delay   e))    = fv e
fv (Var     x)          = [x]
fv (App     e1 e2)      = fv e1 `union` fv e2
fv (Force   e)          = fv e
fv (Multi   es)         = concatMap fv es
fv (Let     ps e1 e2)   = (fv e2 \\ map patId ps) `union` fv e1
fv (LetRec  bs e)       = (foldr union (fv e) (map (fv . bindExpr) bs))
                            \\ map (patId . bindPat) bs
fv (FFI     _ _)        = []

-- Small helper functions

patId :: Pat -> Id
patId (TermPat x _) = x
patId (TypePat x _) = x

bindPat :: Bind -> Pat
bindPat (Bind p _) = p

bindExpr :: Bind -> Expr
bindExpr (Bind _ e) = e

