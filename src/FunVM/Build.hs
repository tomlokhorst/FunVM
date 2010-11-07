module FunVM.Build
  ( params
  , int32
  , character
  , (@@)
  , ($$)
  , int
  , lets
  , fun
  ) where

import FunVM.Syntax


-- Type convenience functions

params :: [Type] -> [Bind]
params = map (TermPat "_")

int32 :: Type
int32 = Base $ Int 32

character :: Type
character = Base Character

-- Expression convenience functions

(@@) :: Expr -> [Expr] -> Expr
e1 @@ [e2] = e1 `App` e2
e1 @@ e2  = e1 `App` Multi e2

($$) :: Expr -> Expr -> Expr
e1 $$ e2 = e1 `App` Val (Delay e2)

int :: Integer -> Val
int x = Lit $ Integer x int32

lets :: [([Bind], Expr)] -> Expr -> Expr
lets xs expr = foldr (uncurry Let) expr xs

fun :: Id -> [Bind] -> [Type] -> Expr -> ([Bind], Expr)
fun x bs rts e  = ([TermPat x $ map f bs `Fun` rts], Val $ Lam bs e)
  where
    f :: Bind -> Bind
    f (TermPat _ t) = TermPat "_" t
    f t             = t

