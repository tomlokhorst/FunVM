module FunVM.Types
  ( TyVar
  , Types
  , Type (..)
  , Base (..)
  , Kind (..)
  , int32
  , char
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
  = Float32
  | Double64
  | Int1
  | Int8
  | Int32
  | Int64
  | Character
  | Utf16String
  deriving Eq

-- | Kind of a type
data Kind
  = Star
  deriving Eq

-- | Convenience functions

int32 :: Type
int32 = Base Int32

char :: Type
char = Base Character

