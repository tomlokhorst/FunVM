module FunVM.Types
  ( TyVar
  , Types
  , Type (..)
  , Base (..)
  , Kind (..)
  ) where

-- | Type Variable
type TyVar = String

-- | List of types
type Types = [Type]

-- | Single type
data Type
  = Base    Base
  | Fun     Types  Types
  | Lazy    Types
  | Quant   TyVar   Kind
  | TyVar   TyVar
  | Any
  deriving Eq

-- | Built-in types
data Base
  = Int Int
  | Float32
  | Double64
  | Character
  | Utf16String
  deriving Eq

-- | Kind of a type
data Kind
  = Star
  deriving Eq

