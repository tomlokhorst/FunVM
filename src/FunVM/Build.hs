module FunVM.Build
  ( (~>)
  , (@@)
  , params
  , int32
  , character
  , int
  ) where

import FunVM.Syntax


-- Type convenience functions

infixr 5 ~>

(~>) :: [Bind] -> Types -> Type
(~>) = Fun

(@@) :: Expr -> [Expr] -> Expr
e1 @@ e2 = e1 `App` Multi e2

params :: [Type] -> [Bind]
params = map (TermPat "_")

int32 :: Type
int32 = Base $ Int 32

character :: Type
character = Base Character

-- Expression convenience functions

int :: Integer -> Expr
int x = Val (Lit $ Integer x int32)

