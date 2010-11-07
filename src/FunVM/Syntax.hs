module FunVM.Syntax
  ( Id
  , TyVar
  , Types
  , Type (..)
  , Base (..)
  , Kind (..)
  , Bind (..)
  , ValBind (..)
  , Val (..)
  , Literal (..)
  , Expr (..)
  , Group
  , Module (..)
  , fv
  ) where

import Data.List

-- | Identifier for variables and other names
type Id = String

-- | Type Variable
type TyVar = String

-- | List of types
type Types = [Type]

-- | Kind of a type
data Kind
  = Star
  deriving Eq

-- | Binding patterns in lambdas, lets or top level bindings
data Bind
  = TermPat  Id  Type
  | TypePat  Id  Kind
  deriving Eq

-- | Single type
data Type
  = Base    Base
  | TyVar   TyVar
  | Fun     [Bind]  Types
  | Lazy    Types
  deriving Eq

-- | Built-in types
data Base
  = Int { bitSize :: !Int }
  | Float32
  | Double64
  | Character
  | Utf16String
  deriving Eq

-- Value binding
data ValBind
  = Bind Bind Val
  deriving Eq

-- | Values in Weak Head Normal Form
data Val
  = Lit    Literal 
  | Lam    [Bind]  Expr
  | Delay  Expr
  | Prim   String  Type
  deriving Eq

-- | Main expression data type
data Expr
  = Val     Val
  | Var     Id
  | App     Expr       Expr
  | Multi   [Expr]
  | Force   Expr
  | Let     [Bind]     Expr  Expr
  | LetRec  [ValBind]  Expr
  deriving Eq

-- | Literal integers, characters or strings
data Literal
  = Integer  !Integer  !Type
  | Char     !Char
  | String   !String
  | Type     !Type
  deriving Eq

-- | Group of mutually dependent bindings
type Group = [ValBind]

-- | Module definition
data Module
  = Module
      { name    :: Id
      , imports :: [Id]
      , bgroups :: [Group]
      }
  deriving Eq

-- | Free value variables in expression
fv :: Expr -> [Id]
fv (Val (Lit     _))    = []
fv (Val (Lam     ps e)) = fv e \\ map bindId ps
fv (Val (Delay   e))    = fv e
fv (Val (Prim    _ _))  = []
fv (Var     x)          = [x]
fv (App     e1 e2)      = fv e1 `union` fv e2
fv (Force   e)          = fv e
fv (Multi   es)         = concatMap fv es
fv (Let     ps e1 e2)   = (fv e2 \\ map bindId ps) `union` fv e1
fv (LetRec  bs e)       =
  (foldr union (fv e) (map (val (const []) (const fv) fv (\_ _ -> []) . valBindVal) bs))
    \\ map (bindId . valBindBind) bs

-- Small helper functions

bindId :: Bind -> Id
bindId (TermPat x _) = x
bindId (TypePat x _) = x

valBindBind :: ValBind -> Bind
valBindBind (Bind p _) = p

valBindVal :: ValBind -> Val
valBindVal (Bind _ v) = v

val
  :: (Literal -> a)
  -> ([Bind] -> Expr -> a)
  -> (Expr -> a)
  -> (String -> Type -> a)
  -> Val
  -> a
val f _ _ _ (Lit l)    = f l
val _ f _ _ (Lam ps e) = f ps e
val _ _ f _ (Delay e)  = f e
val _ _ _ f (Prim x t) = f x t

