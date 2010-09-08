module FunVM.Types where

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | Kind of a type
data Kind
  = Star
  deriving (Eq, Show)

