module FunVM.Syntax where

import Data.List
import FunVM.Types

-- | Identifier for variables and other names
type Id = String

-- | Module definition
data Module
  = Module Id [Bind]
  deriving Eq

-- | Should be named `module', but that's not allowed
modul' :: (Id -> [Bind] -> a) -> Module -> a
modul' f (Module x bs) = f x bs

-- Value bindings
data Bind
  = Bind [Pattern] Expr
  deriving Eq

bind :: ([Pattern] -> Expr -> a) -> Bind -> a
bind f (Bind ps e) = f ps e

-- | Patterns in lambdas, lets or top level bindings
data Pattern
  = ValPattern   Id  Type
  | TypePattern  Id  Kind
  deriving Eq

pattern :: (Id -> Type -> a) 
             -> (Id -> Kind -> a)
             -> Pattern
             -> a
pattern f _ (ValPattern x t)  = f x t
pattern _ g (TypePattern x k) = g x k


-- | Main expression data type
data Expr
  = Var     Id
  | Lit     Literal
  | App     Expr        [Expr]
  | Lam     [Pattern]   Expr
  | Let     LetType     [Bind]      Expr
  | Multi   [Expr]
  | Delay   Expr
  | Force   Expr
  | FFI     String      Type
  deriving Eq

-- | Literal integers, characters or strings
data Literal
  = Int     Integer  Type
  | Char    Char
  | String  String
  | Type    Type
  deriving Eq

literal :: (Integer -> Type -> a)
             -> (Char -> a)
             -> (String -> a)
             -> (Type -> a)
             -> Literal
             -> a
literal f _ _ _ (Int x t)  = f x t
literal _ f _ _ (Char x)   = f x
literal _ _ f _ (String x) = f x
literal _ _ _ f (Type x)   = f x

-- | Type of let binding
data LetType
  = NonRec
  | Rec
  deriving Eq

letType :: a -> a -> LetType -> a
letType x _ NonRec = x
letType _ y Rec    = y

-- | Free value variables in expression
fv :: Expr -> [Id]
fv (Var     x)       = [x]
fv (Lit     _)       = []
fv (App     e  es)   = nub $ concat (fv e : (map fv es))
fv (Lam     ps e)    = fv e \\ concatMap patvar ps
fv (Let NonRec bs e) = let bds []      = []
                           bds (b:bs') = (bds bs' \\ bind (\ps _ -> concatMap patvar ps) b)
                                             ++ bind (\_ e' -> fv e') b
                       in nub $ (fv e \\ concatMap (bind (\ps _ -> concatMap patvar ps)) bs)
                                 ++ bds bs
fv (Let Rec  bs e)   = (nub $ concat [fv e ++ concatMap (bind (\_ e' -> fv e')) bs])
                        \\ concatMap (bind (\ps _ -> concatMap patvar ps)) bs
fv (Delay   e)       = fv e
fv (Force   e)       = fv e
fv (Multi   es)      = concatMap fv es
fv (FFI     _ _)     = []

patvar :: Pattern -> [Id]
patvar = pattern (\v _ -> [v]) (\_ _ -> [])

