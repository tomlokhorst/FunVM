module FunVM.JsCompiler
  ( compile
  ) where

import Data.List

import FunVM.Core.Destructors
import FunVM.Core.Syntax

data Js = Js { unJs :: String }

instance Show Js where
  show (Js s) = s

cv :: Val -> Js
cv (Lit (Integer x _)) = Js $ show x
cv (Lit (Char c))      = Js $ show c
cv (Lit (String s))    = Js $ show s
cv (Lit (Type _))      = Js $ "undefined" --typeErasure
cv (Lam bs e)          = Js $ "function (" ++ intercalate ", " (map (varName . bindId) bs) ++ "){ return " ++ unJs (ce e) ++ "}"
cv (Delay e)           = Js $ "(function () {var val = null;"
                                ++ "return function(){ return val == null ? val = (" ++ unJs (ce e) ++") : val }"
                                ++ "})()"
cv (Prim s _)          = Js $ s

ce :: Expr -> Js
ce (Val v)        = cv v
ce (Var x)        = Js $ varName x
ce (App e1 e2)    = Js $ "(" ++ unJs (ce e1) ++ ").apply(null, [].concat(" ++ unJs (ce e2) ++ "))"
ce (Multi es)     = Js $ "[].concat(" ++ intercalate ").concat(" (map (unJs . ce) es) ++ ")"
ce (Force e)      = Js $ "(" ++ unJs (ce e) ++ ")()"
ce e@(Let{})      = clets e
ce e@(LetRec{})   = clets e

clets :: Expr -> Js
clets e = Js $ "(function () {\n" ++ concatMap unJs vs ++ "return " ++ unJs (ce b) ++ "})()"
 where
   (vs, b) = f e
   f (Let bs e1 e2)  = let (vs', b') = f e2
                       in (cbind bs e1 : vs', b')
   f (LetRec vbs e') = let (vs', b') = f e'
                       in (map cvalbind vbs ++ vs', b')
   f e' = ([], e')

cbind :: [Bind] -> Expr -> Js
cbind bs e = Js $
     "var " ++ bigName ++ " = " ++ unJs (ce e) ++ ";\n"
  ++ concatMap (\(x, nm) -> "var " ++ varName nm ++ " = " ++ bigName ++ "[" ++ show x ++ "];") (zip [0 :: Int ..] $ map bindId bs)
  where
    bigName = concatMap (varName . bindId) bs

cvalbind :: ValBind -> Js
cvalbind = valBind (\b v -> Js $ "var " ++ varName (bindId b) ++ " = " ++ unJs (cv v) ++ ";\n")

compile :: Module -> [Js]
compile (Module _ _ bgs) = map cvalbind $ concat bgs

-- Small helper functions

bindId :: Bind -> Id
bindId (TermPat x _) = x
bindId (TypePat x _) = x

typeErasure :: Js
typeErasure = error "Type Erasure hasn't taken place."

varName :: String -> String
varName nm = if nm `elem` reserved then '$' : nm else nm

reserved :: [String]
reserved = ["if", "const"]

