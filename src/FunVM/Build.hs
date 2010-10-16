module FunVM.Build
  ( int32
  , character
  , int
  ) where

import FunVM.Syntax
import FunVM.Types


-- Type convenience functions

int32 :: Type
int32 = Base $ Int 32

character :: Type
character = Base Character

-- Expression convenience functions

int :: Integer -> Expr
int x = Val (Lit $ Integer x int32)

