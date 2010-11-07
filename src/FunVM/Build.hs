module FunVM.Build
  ( params
  , int32
  , character
  , (@@)
  , ($$)
  , int
  , lets
  , letrec
  , fn
  , fun
  ) where

import Control.Arrow

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
lets xs e = foldr (uncurry Let) e xs

letrec :: [(Bind, Val)] -> Expr -> Expr
letrec xs e = LetRec (map (uncurry Bind) xs) e

fn :: Id -> [Bind] -> [Type] -> Expr -> ([Bind], Expr)
fn x bs rts = first return . second Val . fun x bs rts

fun :: Id -> [Bind] -> [Type] -> Expr -> (Bind, Val)
fun x bs rts e  = (TermPat x $ map f bs `Fun` rts, Lam bs e)
  where
    f :: Bind -> Bind
    f (TermPat _ t) = TermPat "_" t
    f t             = t


