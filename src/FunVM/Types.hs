module FunVM.Types
  ( Name
  , Type (..)
  , Base (..)
  , fvt
  ) where

import FunVM.Core

data Type
  = Base  Base
  | Fun   Type Type
  | Sum   Type Type
  | Node  [Type]
  | Lazy  Type
  | Tuple [Type]
  | Var   Name
  | Any
  deriving (Eq, Show)

data Base
  = Float32
  | Double64
  | Int1
  | Int8
  | Int32
  | Int64
  deriving (Eq, Show)


fvt :: Type -> [Name]
fvt (Base _)     = []
fvt (Fun at rt)  = fvt at ++ fvt rt
fvt (Var nm)     = [nm]
fvt (Sum t1 t2)  = fvt t1 ++ fvt t2
fvt (Node ts)    = concatMap fvt ts
fvt (Lazy t)     = fvt t
fvt (Tuple ts)   = concatMap fvt ts
fvt (Any)        = []

