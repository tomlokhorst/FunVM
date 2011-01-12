module FunVM.Core.Destructors
  ( modul'
  , valBind
  , valBindBind
  , valBindVal
  , valBindId
  , bind
  , bindId
  , literal
  ) where

import FunVM.Core.Syntax

-- | Should be named `module', but that's not allowed
modul' :: (Id -> [Id] -> [Group] -> a) -> Module -> a
modul' f (Module x is bss) = f x is bss

valBind :: (Bind -> Val -> a) -> ValBind -> a
valBind f (Bind p v) = f p v

valBindBind :: ValBind -> Bind
valBindBind = valBind (curry fst)

valBindVal :: ValBind -> Val
valBindVal = valBind (curry snd)

valBindId :: ValBind -> Id
valBindId = bindId . valBindBind

bind :: (Id -> Type -> a) -> (Id -> Kind -> a) -> Bind -> a
bind f _ (TermPat x t)  = f x t
bind _ g (TypePat x k)  = g x k

bindId :: Bind -> Id
bindId = bind const const

literal :: (Integer -> Int -> a)
             -> (Char -> a)
             -> (String -> a)
             -> (Type -> a)
             -> Literal
             -> a
literal f _ _ _ (Integer x bs)  = f x bs
literal _ f _ _ (Char x)        = f x
literal _ _ f _ (String x)      = f x
literal _ _ _ f (Type x)        = f x

