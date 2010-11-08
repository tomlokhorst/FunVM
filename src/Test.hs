module Main where

import FunVM.Build
import FunVM.Evaluator
import FunVM.Pretty ()
import FunVM.Syntax

-- Test module


arith :: Type
arith = params [int32, int32] `Fun` [int32]

primIf :: Expr
primIf = prim "primIfInt32"
              ((params [int32] ++ [TypePat "c" Star] ++ params [Lazy [TyVar "c"], Lazy [TyVar "c"]])
                  `Fun` [Lazy [TyVar "c"]])

test :: Module
test =
  modul "Prelude" []
    [ ( TermPat "add" arith
      , Prim "primAddInt32" arith
      )
    , ( TermPat "sub" arith
      , Prim "primSubInt32" arith
      )
    , ( TermPat "mul" arith
      , Prim "primMulInt32" arith
      )
    , fun "if" [ TermPat "p" int32
               , TypePat "a" Star
               , TermPat "x" (Lazy [TyVar "a"])
               , TermPat "y" (Lazy [TyVar "a"])
               ]
               [int32]
               (Force $ primIf @@ [Var "p", Var "a", Var "x", Var "y"])
    , ( TermPat "if" ((params [int32]
                        ++ [TypePat "a" Star]
                        ++ params [Lazy [TyVar "a"], Lazy [TyVar "a"]]
                     ) `Fun` [int32])
      , Lam [ TermPat "p" int32
            , TypePat "a" Star
            , TermPat "x" (Lazy [TyVar "a"])
            , TermPat "y" (Lazy [TyVar "a"])
            ]
            (Force $ primIf @@ [Var "p", Var "a", Var "x", Var "y"])
      )
    , ( TermPat "const'" (([TypePat "a" Star, TypePat "b" Star]
                             ++ params [TyVar "a", Lazy [TyVar "b"]]
                         ) `Fun` [TyVar "a"])
      , Lam [ TypePat "a" Star
            , TypePat "b" Star
            , TermPat "x" (TyVar "a")
            , TermPat "y" (Lazy [TyVar "b"])
            ]
            (Var "x")
      )
    , ( TermPat "const" ([TypePat "a" Star, TypePat "b" Star]
                              `Fun` [params [Lazy [TyVar "a"]]
                                      `Fun` [params [Lazy [TyVar "b"]]
                                              `Fun` [TyVar "a"]]])
      ,  Lam [ TypePat "a" Star
             , TypePat "b" Star
             ]
             (lam [TermPat "x" (Lazy [TyVar "a"])]
                  (lam [TermPat "y" (Lazy [TyVar "b"])]
                       (Force $ Var "x")))
      )
    , ( TermPat "x" (Lazy [int32])
      , Delay $ Var "const" @@ [ ty int32
                               , ty character
                               ]
                            @@ [int 3]
                            @@ [char 'c']
      )
    , ( TermPat "y" (Lazy [int32])
      , Delay $ Val (Lam [TermPat "c" (Lazy [character])] (Var "x"))
                  $$ Val (Lit $ Char 'd')
      )
    , ( TermPat "main" (Lazy [int32])
      , Delay $ int 42
      )
    ]

test2 :: Expr
test2 = lets [ ( [TermPat "add" arith]
               , (Val $ Prim "primAddInt32" arith)
               )
             , ( [TermPat "sub" arith]
               , (Val $ Prim "primSubInt32" arith)
               )
             , ( [TermPat "add3" $ params [int32, int32, int32] `Fun` [int32]]
               , (Val $ Lam [TermPat "x" int32, TermPat "y" int32, TermPat "z" int32]
                            (Var "add" @@ [Var "add" @@ [Var "x", Var "y"], Var "z"]))
               )
             , ( [TermPat "bar" $ params [int32] `Fun` [int32, int32]]
               , (Val $ Lam [TermPat "x" int32]
                            (Multi [ Var "add" @@ [Var "x", int 1]
                                   , Var "sub" @@ [Var "x", int 1]
                                   ]))
               )
             ]
             (Multi [ Var "add3" @@ [int 1, int 2, int 3]
                    , Var "add3" @@ [Multi [int 1, int 2], int 3]
                    , Var "add3" @@ [Var "bar" @@ [int 2], int 2]
                    ])

test3 :: Expr
test3 = lets [ fn "const" [ TypePat "a" Star
                          , TypePat "b" Star
                          , TermPat "x" $ Lazy [TyVar "a"]
                          , TermPat "y" $ Lazy [TyVar "b"]
                          ]
                          [TyVar "a"]
                          (Force $ Var "x")
             , ( [TermPat "add" arith]
               , (Val $ Prim "primAddInt32" arith)
               )
             ]
             (Var "const" @@ [ ty int32
                             , ty int32
                             , delay (Var "add" @@ [int 2, int 3])
                             , delay (Var "add" @@ [int 4, int 5])
                             ])

fib :: Expr
fib = lets [ ( [TermPat "sub" arith]
             , (Val $ Prim "primSubInt32" arith)
             )
           , ( [TermPat "add" arith]
             , (Val $ Prim "primAddInt32" arith)
             )
           , ( [TermPat "eq" arith]
             , (Val $ Prim "primEqInt32" arith)
             )
           , ( [TermPat "or" arith]
             , (Val $ Prim "primOrInt32" arith)
             )
           , fn "if" [ TermPat "p" int32
                     , TypePat "a" Star
                     , TermPat "x" (Lazy [TyVar "a"])
                     , TermPat "y" (Lazy [TyVar "a"])
                     ]
                     [TyVar "a"]
                     (Force $ primIf @@ [Var "p", Var "a", Var "x", Var "y"])
           ]
           (letrec
              [ fun "fib"
                  [ TermPat "n" $ int32]
                  [int32]
                  (Var "if"
                      @@ [ Var "or" @@ [ Var "eq" @@ [Var "n", int 0]
                                       , Var "eq" @@ [Var "n", int 1]
                                       ]
                         , ty int32
                         , delay (Var "n")
                         , delay
                             (Var "add"
                                @@ [ Var "fib" @@ [Var "sub" @@ [Var "n", int 1]]
                                   , Var "fib" @@ [Var "sub" @@ [Var "n", int 2]]
                                   ]
                             )
                         ]
                  )
              , fun "fibL"
                  [TermPat "n" $ Lazy [int32]]
                  [int32]
                  (Var "if"
                      @@ [ Var "or" @@ [ Var "eq" @@ [Force $ Var "n", int 0]
                                       , Var "eq" @@ [Force $ Var "n", int 1]
                                       ]
                         , ty int32
                         , delay (Force $ Var "n")
                         , delay
                             (Var "add"
                                @@ [ Var "fibL" @@ [delay $ Var "sub" @@ [Force $ Var "n", int 1]]
                                   , Var "fibL" @@ [delay $ Var "sub" @@ [Force $ Var "n", int 2]]
                                   ]
                             )
                         ]
                  )
              ]
             -- (Var "fib" @@ [int 20])
              (Var "fibL" @@ [delay $ int 20])
           )

main :: IO ()
main = print (eval [] fib)

