module Test where

import FunVM.Build
import FunVM.Pretty ()
import FunVM.Syntax

-- Test module


arith :: Type
arith = params [int32, int32] ~> [int32]

primIf :: Expr
primIf = Val $ FFI "primIf"
  ((params [int32] ++ [TypePat "c" Star] ++ params [Lazy [TyVar "c"], Lazy [TyVar "c"]])
      ~> [Lazy [TyVar "c"]])

test :: Module
test =
  Module "Prelude" []
    [ [ Bind (TermPat "add" arith)
             (FFI "primAddInt32" arith)
      ]
    , [ Bind (TermPat "sub" arith)
             (FFI "primSubInt32" arith)
      ]
    , [ Bind (TermPat "mul" arith)
             (FFI "primMulInt32" arith)
      ]
    , [ Bind (TermPat "if" ((params [int32]
                               ++ [TypePat "a" Star]
                               ++ params [Lazy [TyVar "a"], Lazy [TyVar "a"]]
                            ) ~> [int32]))
             (Lam [ TermPat "p" int32
                  , TypePat "a" Star
                  , TermPat "x" (Lazy [TyVar "a"])
                  , TermPat "y" (Lazy [TyVar "a"])
                  ]
                  (Force $ primIf @@ [Var "p", Var "a", Var "x", Var "y"]))
      ]
    , [ Bind (TermPat "const'" (([TypePat "a" Star, TypePat "b" Star]
                                    ++ params [TyVar "a", Lazy [TyVar "b"]]
                                ) ~> [TyVar "a"]))
             (Lam [ TypePat "a" Star
                  , TypePat "b" Star
                  , TermPat "x" (TyVar "a")
                  , TermPat "y" (Lazy [TyVar "b"])
                  ]
                  (Var "x"))
      ]
    , [ Bind (TermPat "const" ([TypePat "a" Star, TypePat "b" Star]
                                  ~> [params [Lazy [TyVar "a"]]
                                        ~> [params [Lazy [TyVar "b"]]
                                              ~> [TyVar "a"]]]))
             (Lam [ TypePat "a" Star
                  , TypePat "b" Star
                  ]
                  (Val $ Lam [TermPat "x" (Lazy [TyVar "a"])]
                             (Val $ Lam [TermPat "y" (Lazy [TyVar "b"])]
                                        (Force $ Var "x"))))
      , Bind (TermPat "x" (Lazy [int32]))
             (Delay $ Var "const" @@ [ Val $ Lit $ Type int32
                                     , Val $ Lit $ Type character
                                     ]
                                  @@ [Val $ int 3]
                                  @@ [Val $ Lit (Char 'c')])
      , Bind (TermPat "y" (Lazy [int32]))
             (Delay $ Val (Lam [TermPat "c" (Lazy [character])] (Var "x"))
                        $$ Val (Lit $ Char 'd'))
      , Bind (TermPat "main" (Lazy [int32]))
             (Delay $ Val $ int 42)
      ]
    ]

main :: IO ()
main = print test

