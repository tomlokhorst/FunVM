module Test where

import FunVM.Pretty ()
import FunVM.Syntax
import FunVM.Types

-- Test module

test :: Module
test =
  Module "Prelude"
    [ Bind [ ValPattern "add" ([Base Int32, Base Int32] `Fun` [Base Int32]) ]
           (FFI "primAddInt32" ([Base Int32, Base Int32] `Fun` [Base Int32]))
    , Bind [ ValPattern "sub" ([Base Int32, Base Int32] `Fun` [Base Int32])
           , ValPattern "mul" ([Base Int32, Base Int32] `Fun` [Base Int32])
           ]
           (Multi
              [ (FFI "primAddInt32" ([Base Int32, Base Int32] `Fun` [Base Int32]))
              , (FFI "primAddInt32" ([Base Int32, Base Int32] `Fun` [Base Int32]))
              ])
    , Bind [ ValPattern "if"
               ([Base Int32, Quant "a" Star , Lazy [TyVar "a"], Lazy [TyVar "a"]]
                  `Fun` [TyVar "a"])
           ]
           (Lam [ ValPattern  "p" (Base Int32)
                , TypePattern "b" Star
                , ValPattern  "x" (Lazy [TyVar "b"])
                , ValPattern  "y" (Lazy [TyVar "b"])
                ]
                (Force ((FFI "primIf" ([ Base Int32
                                       , Quant "c" Star
                                       , Lazy [TyVar "c"]
                                       , Lazy [TyVar "c"]
                                       ] `Fun` [Lazy [TyVar "c"]])
                        ) `App` [Var "p", Var "b", Var "x", Var "y"])))
    , Bind [ ValPattern "const'"
               ([Quant "a" Star, Quant "b" Star, TyVar "a", Lazy [TyVar "a"]]
                 `Fun` [TyVar "a"]) 
           ]
           (Lam [ TypePattern "c" Star
                , TypePattern "d" Star
                , ValPattern "x" (TyVar "c")
                , ValPattern "y" (Lazy [TyVar "d"])
                ]
                (Var "x"))
    , Bind [ ValPattern "const"
               ([Quant "a" Star, Quant "b" Star]
                 `Fun` [[TyVar "a"]
                        `Fun` [[Lazy [TyVar "a"]]
                               `Fun` [TyVar "a"]]]) 
           ]
           (Lam [ TypePattern "c" Star
                , TypePattern "d" Star
                ]
                (Lam [ValPattern "x" (TyVar "c")]
                     (Lam [ValPattern "y" (Lazy [TyVar "d"])]
                          (Var "x"))))
    , Bind [ ValPattern "x" (Base Int32) ]
           (Var "const" `App` [ Lit $ Type (Base Int32)
                              , Lit $ Type (Base Character)
                              ]
                        `App` [Lit (Int 3 $ Base Int32)]
                        `App` [Lit (Char 'c')])
    , Bind [ ValPattern "y" (Base Int32) ]
           (Lam [ValPattern "x" (Base Character)] (Var "x")
             `App` [Lit (Char 'c')])
    , Bind [ ValPattern "main" (Base Int32) ]
       (Lit (Int 42 $ Base Int32))
    ]

main :: IO ()
main = print test

