module FunVM.Destructors
  ( modul'
  , bind
  , pat
  , literal
  ) where

import FunVM.Syntax
import FunVM.Types

-- | Should be named `module', but that's not allowed
modul' :: (Id -> [Group] -> a) -> Module -> a
modul' f (Module x bss) = f x bss

bind :: (Pat -> Expr -> a) -> Bind -> a
bind f (Bind p e) = f p e

pat :: (Id -> Type -> a) 
         -> (Id -> Kind -> a)
         -> Pat
         -> a
pat f _ (TermPat x t)  = f x t
pat _ g (TypePat x k) = g x k

literal :: (Integer -> Type -> a)
             -> (Char -> a)
             -> (String -> a)
             -> (Type -> a)
             -> Literal
             -> a
literal f _ _ _ (Integer x t)  = f x t
literal _ f _ _ (Char x)       = f x
literal _ _ f _ (String x)     = f x
literal _ _ _ f (Type x)       = f x

