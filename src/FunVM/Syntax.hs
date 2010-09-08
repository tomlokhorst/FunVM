module FunVM.Syntax where

import Data.List
import qualified FunVM.Types as T

-- | Identifier for variables and other names
type Id = String

-- | Module definition
data Module
  = Module Id [Bind]
  deriving (Eq, Show)

-- | Should be named `module', but that's not allowed
modul' :: (Id -> [Bind] -> a) -> Module -> a
modul' f (Module x bs) = f x bs

-- Value bindings
data Bind
  = Bind [Pattern] Expr
  deriving (Eq, Show)

bind :: ([Pattern] -> Expr -> a) -> Bind -> a
bind f (Bind ps e) = f ps e

-- | Patterns in lambdas, lets or top level bindings
data Pattern
  = ValPattern   Id  T.Type
  | TypePattern  Id  T.Kind
  deriving (Eq, Show)

pattern :: (Id -> T.Type -> a) 
             -> (Id -> T.Kind -> a)
             -> Pattern
             -> a
pattern f _ (ValPattern x t)  = f x t
pattern _ g (TypePattern x k) = g x k


-- | Main expression data type
data Expr
  = Var     Id
  | Lit     Literal     T.Type
  | App     Expr        [Expr]
  | Lam     [Pattern]   Expr
  | Let     LetType     [Bind]      Expr
  | Multi   [Expr]
  | Delay   Expr
  | Force   Expr
  | FFI     String      T.Type
  deriving (Eq, Show)

-- | Literal integers, characters or strings
data Literal
  = Int     Integer
  | Char    Char
  | String  String
  deriving (Eq, Show)

literal :: (Integer -> a)
             -> (Char -> a)
             -> (String -> a)
             -> Literal
             -> a
literal f _ _ (Int x)    = f x
literal _ g _ (Char c)   = g c
literal _ _ h (String s) = h s

-- | Type of let binding
data LetType
  = NonRec
  | Rec
  deriving (Eq, Show)

letType :: a -> a -> LetType -> a
letType x _ NonRec = x
letType _ y Rec    = y

-- | Free value variables in expression
fv :: Expr -> [Id]
fv (Var     x)       = [x]
fv (Lit     _ _)     = []
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

