module FunVM.Build
  ( params
  , int32
  , character
  , (@@)
  , ($$)
  , int
  , char
  , ty
  , lam
  , delay
  , prim
  , lets
  , letrec
  , fn
  , fun
  , modul
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

int :: Integer -> Expr
int x = Val . Lit $ Integer x int32

char :: Char -> Expr
char = Val . Lit . Char

ty :: Type -> Expr
ty = Val . Lit . Type

lam :: [Bind] -> Expr -> Expr
lam bs = Val . Lam bs

delay :: Expr -> Expr
delay = Val . Delay

prim :: String -> Type -> Expr
prim s = Val . Prim s

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

modul :: Id -> [Id] -> [(Bind, Val)] -> Module
modul nm is xs = Module nm is [map (uncurry Bind) xs]

