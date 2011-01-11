module FunVM.WorkerWrapperTransformations.DefinitionSiteArityRaising
  ( transform
  ) where

import FunVM.Core
import qualified FunVM.WorkerWrapperTransformations.GenericTransform as GT

transform :: Module -> Module
transform = GT.transform applicable updateWorker updateWrapper

applicable :: ValBind -> Bool
applicable (Bind _ v) = moreThanOne (nrArgs (Val v))
  where
    moreThanOne []  = False
    moreThanOne [_] = False
    moreThanOne _   = True

nrArgs :: Expr -> [Int]
nrArgs (Val (Lam bs e)) = length bs : nrArgs e
nrArgs (Let _ _ e)      = nrArgs e
nrArgs _                = []

updateWorker :: ValBind -> ValBind
updateWorker (Bind (TermPat x t@Fun{}) l@Lam{}) =
  let (pts, rts, ps, b) = f t (Val l)
  in Bind (TermPat x (Fun pts rts)) (Lam ps b)
  where
    f :: Type -> Expr -> ([Bind], [Type], [Bind], Expr)
    f (Fun pbs [rt]) (Val (Lam ps b)) = let (pbs', rts', ps', b') = f rt b
                                        in (pbs ++ pbs', rts', ps ++ ps', b')
    f t' (Let bs e1 e2) = let (pbs, rts, ps, b) = f t' e2
                          in ([], [pbs `Fun` rts], [], Let bs e1 (Val $ Lam ps b))
    f t' e = ([], [t'], [], e)
updateWorker vb = vb

updateWrapper :: ValBind -> ValBind -> ValBind
updateWrapper (Bind (TermPat _ t) _) (Bind p@(TermPat x _) (Lam ps' body)) = Bind p (Lam ps' (f body))
  where
    wNm  = x ++ "_worker"
    mwNm = Just $ wNm

    fval :: Val -> Val
    fval (Lam bs e) = Lam bs (f e)
    fval (Delay e)  = Delay (f e)
    fval v          = v

    f :: Expr -> Expr
    f (Val v)             = Val (fval v)
    f (Var nm)            = Var nm
    f e@(App e1 _)
      | appVar e1 == mwNm = foldl (@@) (Var wNm) (jn (shape t) (args e))
                            
    f (App e1 e2)         = App (f e1) (f e2)
    f (Multi es)          = Multi (map f es)
    f (Force e)           = Force (f e)
    f (Let bs e1 e2)      = Let bs e1 (f e2)
    f (LetRec vbs e)      = LetRec vbs (f e)

    appVar :: Expr -> Maybe Id
    appVar (Val (Delay e))  = appVar e
    appVar (App (Var nm) _) = Just nm
    appVar (App e1       _) = appVar e1
    appVar (Force e)        = appVar e
    appVar (Let _ _ e2)     = appVar e2
    appVar (LetRec _ e)     = appVar e
    appVar _ = Nothing

    shape :: Type -> [Int]
    shape (Fun bs [t']) = length bs : shape t'
    shape (Fun bs ts)   = [length bs, length ts]
    shape _             = []

    args :: Expr -> [Expr]
    args (App e1@App{} e2) = args e1 ++ args e2
    args (App _        e2) = args e2
    args (Multi es)        = es
    args e                 = [e]

    jn []     _  = []
    jn (n:ns) xs = let (ys, zs) = splitAt n xs
                   in ys : jn ns zs
updateWrapper _ vb = vb

