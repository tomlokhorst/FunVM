{-# OPTIONS_GHC -fno-warn-orphans #-} 
{-# LANGUAGE StandaloneDeriving #-}

-- This module should be rewritten to use PP combinators!

module FunVM.Core.Pretty
  (pretty) where

import FunVM.Core.Syntax

import Data.List

class Pretty a where
  pr :: ShowS -> a -> ShowS

pretty :: (Pretty a) => a -> String
pretty x = pr id x ""

instance Pretty Kind where
  pr _ Star = s "*"

instance Pretty Bind where
  pr pre (TermPat "_" t) = pr pre t
  pr pre (TermPat x   t) = s x . s " : " . pr pre t
  pr pre (TypePat x   t) = s x . s " :: " . pr pre t

instance Pretty Type where
  pr pre (Base b)    = pr pre b
  pr pre (Fun ps rs) = tuple (map (pr pre) ps)
                        . s " -> " . tuple (map (pr pre) rs)
  pr pre (Lazy [t])  = s "{" . pr pre t . s "}"
  pr pre (Lazy ts)   = s "{" . inter (s ", ") (map (pr pre) ts) . s "}"
  pr _   (TyVar x)   = s x
  pr _   (Any)       = s "_"

instance Pretty Base where
  pr _ (Int x)     = s ("int" ++ show x)
  pr _ Float32     = s "float32"
  pr _ Double64    = s "double64"
  pr _ Character   = s "char"
  pr _ Utf16String = s "string"

instance Pretty ValBind where
  pr pre (Bind (TermPat x (Fun _ rts)) (Lam ps e)) =
      s "fun " . s x . sp . tuple (map (pr pre) ps) . s " -> " . tuple (map (pr pre) rts) . nl
    . s " = " . pr (indent 3 pre) e
  pr pre (Bind p e) =
      pre . pr pre p . nl
    . pre . s "  = " . pr ((indent 1 pre) . s "   ") e

instance Pretty Val where
  pr pre (Lit l)                = pr pre l
  pr pre (Lam ps e@(Var {}))    = s "\\" . tuple (map (pr pre) ps)
                                   . s " -> " . pr pre e
  pr pre (Lam ps e@(Val Lam{})) = s "\\" . tuple (map (pr pre) ps)
                                   . s " -> " . pr pre e
  pr pre (Lam ps e)             = s "\\" . tuple (map (pr pre) ps)
                                   . s " ->" . nl . pre . s "  " . pr (indent 2 pre) e
  pr pre (Delay (Multi es))     = s "{" . commas (map (pr (indent 1 pre)) es) . s "}"
  pr pre (Delay e)              = s "{" . pr (indent 1 pre) e . s "}"
  pr pre (Prim x t)             = s "primitive " . s x . s " : " . pr pre t

instance Pretty Expr where
  pr pre (Val v)             = pr pre v
  pr _   (Var x)             = s x
  pr pre (App f@(App {}) a)  = pr pre f . sp . pr pre a
  pr pre (App f@(Var {}) a)  = pr pre f . sp . pr pre a
  pr pre (App f a)           = paren (pr pre f) . nl . pre . sp . sp . pr (indent 2 pre) a
  pr pre (Multi es)          = tuple (map (pr pre) es)
  pr pre (Force e)           = s "|" . pr (indent 1 pre) e . s "|"
  pr pre l@(Let{})           = s "let " . nl . f l
    where
      f (Let ps e1 e2) = indent 2 pre . tuple (map (pr $ indent 2 pre) ps) . nl
                           . pre . s "    = " . pr (indent 7 pre) e1 . nl
                           . f e2
      f e              = pre . s "in " . pr (indent 3 pre) e
  pr pre (LetRec bs e)      = s "letrec " . nl
                                . inter nl (map (pr (indent 2 pre)) bs) . nl
                                . pre . s "in " . pr (indent 3 pre) e

instance Pretty Literal where
  pr pre (Integer x t)  = s "(" . shows x . s " : " . pr pre t . s ")"
  pr _   (Char c)       = shows c
  pr _   (String x)     = shows x
  pr pre (Type t)       = pr pre t

instance Pretty Module where
  pr pre (Module x is bss) =
      s "module " . s x . nl
    . inter nl (map (\i -> s "import " . s i) is)
    . nl
    . inter (nl . nl) (map (pr pre) (concat bss))

-- Helper functions, to pretty print
sp, nl :: ShowS
sp    = (" " ++)
nl    = ('\n' :)

indent :: Int -> ShowS -> ShowS
indent x pre = pre . (replicate x ' ' ++)

tuple :: [ShowS] -> ShowS
tuple [x] = x
tuple xs  = paren (commas xs)

paren :: ShowS -> ShowS
paren ss  = s "(" . ss . s ")"

commas :: [ShowS] -> ShowS
commas = inter (s ", ")

inter :: ShowS -> [ShowS] -> ShowS
inter x xs = foldr (.) id $ intersperse x xs

s :: String -> ShowS
s = showString


-- Show instances for all data type
instance Show Kind where
  show = pretty

instance Show Bind where
  show = pretty

instance Show Type where
  show = pretty

instance Show Base where
  show = pretty

instance Show Literal where
  show = pretty

instance Show ValBind where
  show = pretty

instance Show Expr where
  show = pretty

instance Show Module where
  show = pretty

-- deriving instance Show Kind
-- deriving instance Show Bind
-- deriving instance Show Type
-- deriving instance Show Base
-- deriving instance Show ValBind
-- deriving instance Show Val
-- deriving instance Show Expr
-- deriving instance Show Literal
-- deriving instance Show Module

