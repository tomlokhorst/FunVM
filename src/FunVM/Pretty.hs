{-# OPTIONS_GHC -fno-warn-orphans #-} 
module FunVM.Pretty
  (pretty) where

import FunVM.Syntax
import FunVM.Types

import Data.List

class Pretty a where
  pr :: ShowS -> a -> ShowS

pretty :: (Pretty a) => a -> String
pretty x = pr id x ""

instance Pretty Module where
  pr pre (Module x bs) =
      s "module " . s x . nl . nl
    . inter nl (map (pr pre) bs)

instance Pretty Bind where
  pr pre (Bind ps e) =
      pre . tuple (map (pr pre) ps) . nl
    . s " = " . pr (pre . s "   ") e . nl

instance Pretty Pattern where
  pr pre (ValPattern  x t) = s x . s " : " . pr pre t
  pr pre (TypePattern x t) = s x . s " : " . pr pre t

instance Pretty Expr where
  pr _   (Var x) = s x
  pr pre (Lit l t) = s "(" . pr pre l . s " : " . pr pre t . s ")"
  pr pre (App f@(App {}) as) = pr pre f . sp . tuple (map (pr pre) as)
  pr pre (App f@(Var {}) as) = pr pre f . sp . tuple (map (pr pre) as)
  pr pre (App f as)          = paren (pr pre f) . nl
                                . pre . sp . tuple (map (pr pre) as)
  pr pre (Lam ps e)          = s "\\" . tuple (map (pr pre) ps)
                                . s " -> " . pr pre e
  pr pre (Let t bs e)        = s "let " . pr pre t . nl
                                . inter nl (map (pr (pre . s "  ")) bs)
                                . s " in" . nl
                                . pr pre e
  pr pre (Multi es)          = tuple (map (pr pre) es)
  pr pre (Delay (Multi es))  = s "{" . commas (map (pr pre) es) . s "}"
  pr pre (Delay e)           = s "{" . pr pre e . s "}"
  pr pre (Force e)           = s "|" . pr pre e . s "|"
  pr pre (FFI x t)           = s "foreign " . shows x . s " : " . pr pre t

instance Pretty Literal where
  pr _ (Int x)    = shows x
  pr _ (Char c)   = shows c
  pr _ (String x) = shows x

instance Pretty LetType where
  pr _ NonRec = id
  pr _ Rec    = s " rec"

instance Pretty Type where
  pr pre (Base b)    = pr pre b
  pr pre (Fun ps rs) = tuple (map (pr pre) ps)
                        . s " -> " . tuple (map (pr pre) rs)
  pr pre (Lazy [t])  = s "{" . pr pre t . s "}"
  pr pre (Lazy ts)   = s "{" . inter (s ", ") (map (pr pre) ts) . s "}"
  pr pre (Quant x k) = s x . s " :: " . pr pre k
  pr _   (TyVar x)   = s x
  pr _   (Any)       = s "_"

instance Pretty Base where
  pr _ Float32     = s "float32"
  pr _ Double64    = s "double64"
  pr _ Int1        = s "int1"
  pr _ Int8        = s "int8"
  pr _ Int32       = s "int32"
  pr _ Int64       = s "int64"
  pr _ Character   = s "char"
  pr _ Utf16String = s "string"

instance Pretty Kind where
  pr _ Star = s "*"

-- Helper functions, to pretty print
sp, nl :: ShowS
sp    = (" " ++)
nl    = ('\n' :)

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
instance Show Module where
  show = pretty

instance Show Bind where
  show = pretty

instance Show Pattern where
  show = pretty

instance Show Expr where
  show = pretty

instance Show Literal where
  show = pretty

instance Show LetType where
  show = pretty

instance Show Type where
  show = pretty

instance Show Base where
  show = pretty

instance Show Kind where
  show = pretty

