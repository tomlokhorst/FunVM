module Test where

import FunVM.Pretty ()
import FunVM.Syntax
import FunVM.Types

-- Test module

test :: Module
test =
  Module "Prelude"
    [ Bind [ ValPattern "add" ([int32, int32] `Fun` [int32]) ]
           (FFI "primAddInt32" ([int32, int32] `Fun` [int32]))
    , Bind [ ValPattern "sub" ([int32, int32] `Fun` [int32])
           , ValPattern "mul" ([int32, int32] `Fun` [int32])
           ]
           (Multi
              [ (FFI "primAddInt32" ([int32, int32] `Fun` [int32]))
              , (FFI "primAddInt32" ([int32, int32] `Fun` [int32]))
              ])
    , Bind [ ValPattern "if"
               ([int32, Quant "a" Star , Lazy [TyVar "a"], Lazy [TyVar "a"]]
                  `Fun` [TyVar "a"])
           ]
           (Lam [ ValPattern  "p" int32
                , TypePattern "b" Star
                , ValPattern  "x" (Lazy [TyVar "b"])
                , ValPattern  "y" (Lazy [TyVar "b"])
                ]
                (Force ((FFI "primIf" ([ int32
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
    , Bind [ ValPattern "x" int32 ]
           (Var "const" `App` [ Lit $ Type int32
                              , Lit $ Type (Base Character)
                              ]
                        `App` [Lit (Int 3 $ int32)]
                        `App` [Lit (Char 'c')])
    , Bind [ ValPattern "y" int32 ]
           (Lam [ValPattern "x" (Base Character)] (Var "x")
             `App` [Lit (Char 'c')])
    , Bind [ ValPattern "main" int32 ]
       (Lit (Int 42 $ int32))
    ]

main :: IO ()
main = print test

