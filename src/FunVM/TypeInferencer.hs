{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}

module FunVM.TypeInferencer
  ( typeInfer
  ) where

import FunVM.Core
import qualified FunVM.Syntax as S
import FunVM.Types

import Control.Applicative
import Data.Bool.Extras
import Data.Function
import Data.List
import Data.Ord
import Debug.Trace

data Error
  = NamesNotIntroduced { nms :: [Name] }
  | NamesDupIntroduced { nms :: [Name] }
  | UnifyClash
      { tyInferred :: Type
      , tyExpected :: Type
      }
  | ErrorWrapper
      { typeOuter  :: Type
      , innerError :: Error
      }
  | Err String
  deriving (Show)

type TypeEnv       = [(Name, Type)]
type Errors        = [Error]
type Constraints   = [(Name, Type)]
type Substitutions = [(Name, Type)]
type UniqueVars    = [Name]

-- ConInt  Int
-- Var     Name
-- Lambda  Name    Expr
-- App     Expr    Expr
-- LetRec  [Decl]  Expr
-- Node    Tag     [Expr]
-- Field   Name    Int
-- Case    Name    [(Tag, Expr)]
-- Delay   Expr
-- Force   Expr
-- Tuple   [Expr]

-- Decl       Name    Expr
-- TupleDecl  [Name]  Expr

uniqueVars :: UniqueVars
uniqueVars = map (return) ['a'..'z'] ++ concatMap (\s -> map (\c -> s ++ [c]) ['a'..'z']) uniqueVars

freshVars :: UniqueVars
freshVars = map ("_"++) uniqueVars

-- Based on Algorithm W as described in on page 303 of "Principles of Program Analysis" by Nielsen, Nielsen and Hankin
typeInfer :: TypeEnv -> UniqueVars -> S.Expr -> ((Type, Constraints, Errors), UniqueVars)
typeInfer _   uvs (S.ConInt _)    = (res (Base Int32), uvs)
typeInfer env uvs (S.Var nm)      = (maybe (err [NamesNotIntroduced [nm]]) res (lookup nm env), uvs)
typeInfer env uvs (S.Lambda nm e) =
  let (v:uvs1) = uvs
      at       = Var (v ++ "_lam")
      ((rt, cns, errs), uvs2) = typeInfer ((nm, at):env) uvs1 e
  in ((Fun (subst cns at) rt, cns, errs), uvs2)
typeInfer env uvs (S.App e1 e2)   =
  let ((t1, cns1, errs1), uvs1) = typeInfer env uvs e1
      ((t2, cns2, errs2), uvs2) = typeInfer env uvs1 e2
      (v:uvs3) = uvs2
      at       = Var (v ++ "_app")
      (_, cns3, errs3) = unify (subst cns2 t1) (Fun t2 at)
  in ((subst cns3 at, cns3 ++ cns2 ++ cns1, errs3 ++ errs2 ++ errs1), uvs3)
typeInfer env uvs (S.LetRec ds e) =
  let ((env1, cns1, errs1), uvs1) = typeInferDecls env uvs ds
      ((t, cns2, errs2), uvs2)    = typeInfer env1 uvs1 e
  in ((t, cns2 ++ cns1, errs2 ++ errs1), uvs2)

typeInferDecls :: TypeEnv -> UniqueVars -> [S.Decl] -> ((TypeEnv, Constraints, Errors), UniqueVars)
typeInferDecls env uvs ds =
  let nms  = S.names ds
      dubs = nms \\ nub nms
  in if not (null dubs)
     then (errDecls [NamesDupIntroduced dubs], uvs)
     else let bnds  = zip nms (map (\v -> Var (v ++ "_decl")) uvs)
              uvs1  = drop (length nms) uvs
              (tces, uvs2) = fld (bnds ++ env) uvs1 ds
              ts    = concatMap (\(t, _, _) -> [t]) tces
              cns   = concatMap (\(_, cs, _) -> cs) tces
              errs  = concatMap (\(_, _, es) -> es) tces
              (sbs, errs') = solveConstraints cns
              bnds2 = zip nms (map (subst sbs) ts)
          in ((bnds2 ++ env, sbs, errs'), uvs2)
  where
    fld env uvs []     = ([], uvs)
    fld env uvs (d:ds) = let (tce@(t, cns1, errs1), uvs1)  = f env uvs d
                             (tces, uvs2) = fld env uvs1 ds
                         in (tce:tces, uvs2)
    f env uvs (S.Decl nm e) =
      let Just et                   = lookup nm env
          ((t1, cns1, errs1), uvs1) = typeInfer env uvs e
          (t, cns2, errs2)          = unify t1 (subst cns1 et)
          cns                       = cns2 ++ cns1
          errs                      = errs2 ++ errs1
      in ((t, cns, errs), uvs1)


solveConstraints :: Constraints -> (Substitutions, Errors)
solveConstraints cns' = let (sbs, errs) = loop (cns', [])
                        in (map (\(n, t) -> (n, subst sbs t)) sbs, errs)
  where
    solve gcns errs  = foldr (\(x, y) (x', y') -> (x ++ x', y ++ y')) ([], errs) $ map solveGroup gcns
    loop (cns, errs) = let gcns = groupCns cns
                       in if length gcns == length cns
                          then (cns, errs)
                          else loop (solve gcns errs)
    groupCns cns = groupBy ((==) `on` fst) $ sortBy (comparing fst) cns

    solveGroup :: Constraints -> (Substitutions, Errors)
    solveGroup cns = let (nm, _)        = head cns
                         (t, sbs, errs) = foldr f (Any, [], []) cns
                      in ((nm, t):sbs, errs)

    f :: (Name, Type) -> (Type, Substitutions, Errors) -> (Type, Substitutions, Errors)
    f (nm, t1) (t2, sbs1, errs1) = let (t, sbs2, errs2) = unify t1 t2
                                   in (t, sbs2 ++ sbs1, errs2 ++ errs1)

-- Note: first Type is inferred type, second Type is expected
-- Eg: (App (ConInt 3) (ConFloat 2.1))  =>  unify Int (Float -> a)
unify :: Type -> Type -> (Type, Constraints, Errors)
unify Any               t2                = res t2
unify t1                Any               = res t1
unify t1@(Base b1)      t2@(Base b2)
  | b1 == b2  = res t1
  | otherwise = err [UnifyClash t1 t2]
unify    (Fun at1 rt1)     (Fun at2 rt2)  =
  let (at, cns1, errs1) = unify at1 at2
      (rt, cns2, errs2) = unify (subst cns1 rt1) (subst cns1 rt2)
  in (Fun at rt, cns2 ++ cns1, errs2 ++ errs1)
-- Note: order is important `unify t1 (Var nm2)` comes _before_ `unify (Var nm1) t2`
unify t1                t2@(Var nm2)      =
  if nm2 `elem` fvt t1
  then err [UnifyClash t1 t2]
  else con nm2 t1
unify t1@(Var nm1)      t2                =
  if nm1 `elem` fvt t2
  then err [UnifyClash t1 t2]
  else con nm1 t2
unify t1                t2                = err [UnifyClash t1 t2]

res :: Type -> (Type, Constraints, Errors)
res t = (t, [], [])

con :: Name -> Type -> (Type, Constraints, Errors)
con nm t = (t, [(nm, t)], [])

err :: Errors -> (Type, Constraints, Errors)
err es = (Any, [], es)

errDecls :: Errors -> (TypeEnv, Constraints, Errors)
errDecls es = ([], [], es)

subst :: Substitutions -> Type -> Type
subst _   t@(Base _)     = t
subst sbs   (Fun at rt)  = Fun (subst sbs at) (subst sbs rt)
subst sbs t@(Var nm)     = maybe t id (lookup nm sbs)
subst sbs   (Sum t1 t2)  = Sum (subst sbs t1) (subst sbs t2)
subst sbs   (Node ts)    = Node (map (subst sbs) ts)
subst sbs   (Lazy t)     = Lazy (subst sbs t)
subst sbs   (Tuple ts)   = Tuple (map (subst sbs) ts)
subst _     (Any)        = Any


-- -----------------------------------
-- Test junk
-- -----------------------------------

tiSet :: [(TypeEnv, S.Expr)]
tiSet =
  [ ([], S.ConInt 3)
  --, ([], S.Lambda "x" (S.ConInt 2))
  --, ([], S.Lambda "x" (S.Var "x"))
  --, ([], S.App (S.Lambda "x" (S.Var "x")) (S.ConInt 4))
  --, ([], S.App (S.Lambda "x" (S.Var "x")) (S.Lambda "y" (S.Var "y")))
  --, ([], S.Var "x")
  --, ([], S.App (S.Lambda "x" (S.Var "x")) (S.Var "y"))
  --, ([], S.App (S.Lambda "x" (S.Var "z")) (S.Var "y"))
  --, ([], S.App (S.ConInt 1) (S.ConInt 2))
  --, ([], S.LetRec [ S.Decl "x" (S.Var "y")
  --                , S.Decl "y" (S.ConInt 3)
  --                , S.Decl "z" (S.Var "y")
  --                ] (S.ConInt 2))
  --, ([], S.LetRec [ S.Decl "id" (S.Lambda "x" (S.Var "x"))
  --                , S.Decl "cn" (S.Lambda "x" (S.ConInt 4))
  --                , S.Decl "tw" (S.Lambda "x" (S.Lambda "y" (S.ConInt 0)))
  --                ] (S.Var "tw" `S.App` (S.App (S.Var "id") (S.ConInt 2)) `S.App` (S.App (S.Var "id") (S.Var "cn"))))
  , ([], S.LetRec [ S.Decl "z" (S.App (S.Var "id") (S.Var "y"))
                  , S.Decl "y" (S.ConInt 3)
                  , S.Decl "id" (S.Lambda "y" (S.Var "y"))
                  ] (S.Var "z"))
  --, ( [("id", Fun (Var "a1") (Var "a1")), ("x", Base Int32)]
  --  , S.App (S.Var "id") (S.App (S.Var "id") (S.App (S.Var "id") (S.Var "x"))))
  --, ([], S.LetRec [ S.Decl "z" (S.App (S.Var "id") (S.ConInt 3))
  --                , S.Decl "id" (S.Lambda "y" (S.Var "y"))
  --                ] (S.Var "id"))
  --, ([], S.LetRec [ S.Decl "undefined" (S.Var "undefined")
  --                , S.Decl "id" (S.Lambda "y" (S.Var "y"))
  --                ] (S.Var "id"))
  , ([], S.LetRec [ S.Decl "x" (S.App (S.Var "id") (S.ConInt 3))
                  , S.Decl "y" (S.App (S.Var "id") (S.Lambda "y" (S.Var "y")))
                  , S.Decl "id"  (S.Lambda "x" (S.Var "x"))
                  ] (S.ConInt 0))
  ]

ti :: IO ()
ti = let l   = maximum (map (length . show . snd) tiSet)
         f (_, p) = let s = show p in s ++ (replicate (l - length s) ' ')
         nice ((t, cs, errs), _) = if null errs then show t ++ "    " ++ show cs else show errs
     in mapM_ (\p@(g, e) -> putStrLn $ f p ++ "    " ++ nice (typeInfer g freshVars e)) tiSet

