module FunVM.Syntax
  ( Module (..)
  , Expr (..)
  , Decl (..)
  , Tag (..)
  , fve
  , fvd
  , names
  ) where

import FunVM.Core

import Data.List

data Module
  = Module Name [Decl]
  deriving (Eq, Show)

data Decl
  = Decl       Name    Expr
  | TupleDecl  [Name]  Expr
  deriving (Eq, Show)

data Expr
  = ConInt  Int
  | Var     Name
  | Lambda  Name    Expr
  | App     Expr    Expr
  | LetRec  [Decl]  Expr
  | Node    Tag     [Expr]
  | Field   Name    Int
  | Case    Name    [(Tag, Expr)]
  | Delay   Expr
  | Force   Expr
  | Tuple   [Expr]
  deriving (Eq, Show)

data Tag
  = Tag String
  deriving (Eq, Show)


fve :: Expr -> [Name]
fve (ConInt _)      = []
fve (Var    nm)     = [nm]
fve (Lambda nm e)   = fve e \\ [nm]
fve (App    e1 e2)  = nub $ fve e1 ++ fve e2
fve (LetRec ds e)   = nub $ fvd ds ++ (fve e \\ names ds)
fve (Node   _  es)  = nub $ concatMap fve es
fve (Field  nm _)   = [nm]
fve (Case   nm css) = nub $ nm : concatMap (\(_, e) -> fve e) css
fve (Delay  e)      = fve e
fve (Force  e)      = fve e
fve (Tuple  es)     = nub $ concatMap fve es


fvd :: [Decl] -> [Name]
fvd ds = concatMap f ds
  where
    f (Decl _ e)      = fve e \\ nms
    f (TupleDecl _ e) = fve e \\ nms
    nms = names ds

names :: [Decl] -> [Name]
names ds = concatMap f ds
  where
    f (Decl nm _)       = [nm]
    f (TupleDecl nms _) = nms


-- -----------------------------------
-- Test junk
-- -----------------------------------

testSet :: [Expr]
testSet =
  [ ConInt 3
  , Var "x"
  , Lambda "x" (Var "x")
  , Lambda "x" (Var "y")
  , Lambda "x" (Lambda "y" (Var "y"))
  , Lambda "x" (Lambda "y" (Var "x"))
  , Lambda "x" (Lambda "y" (Var "z"))
  , App (Lambda "x" (Var "z")) (Lambda "z" (Var "z"))
  , App (Lambda "x" (Var "z")) (Lambda "y" (Var "z"))
  , LetRec [Decl "x" (ConInt 3)] (ConInt 2)
  , LetRec [Decl "x" (ConInt 3)] (Var "z")
  , LetRec [Decl "x" (Var "y")] (Var "x")
  , LetRec [Decl "x" (Var "y"), Decl "y" (ConInt 3)] (Var "x")
  , LetRec [Decl "x" (Var "y"), Decl "y" (Var "z")] (ConInt 5)
  , Node (Tag "Cons") [ConInt 3, Var "x"]
  , Case "xs" [(Tag "Nil", Var "y"), (Tag "Cons", App (Field "xs" 1) (Var "z"))]
  , Tuple [Var "x", Var "y", Var "x"]
  ]

test =
 let l   = maximum (map (length . show) testSet)
     f p = let s = show p in s ++ (replicate (l - length s) ' ')
 in mapM_ (\e -> putStrLn $ f e ++ "   " ++ show (fve e)) testSet

