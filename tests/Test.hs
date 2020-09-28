{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Test.Tasty
import Common
import Data.List (isInfixOf)
import Ezlang.Types
import Ezlang.Eval
import qualified Test.Tasty.QuickCheck as QC
import           System.FilePath
import           GHC.IO.Encoding

main :: IO ()
main = setLocaleEncoding utf8 >> runTests 
  [ ezlang_eval
  , ezlang_exn 
  , ezlang_repl 
  ]

-------------------------------------------------------------------------------
-- | ezlang-Repl
-------------------------------------------------------------------------------
ezlang_repl :: Score -> TestTree
ezlang_repl sc = testGroup "Repl"
 [ bc "tests/input/sh0.cmd"  5 "repl-0" 
 , bc "tests/input/sh1.cmd"  5 "repl-1" 
 , bc "tests/input/sh2.cmd" 10 "repl-2" 
 , bc "tests/input/sh3.cmd"  5 "repl-3" 
 , bc "tests/input/sh4.cmd"  5 "repl-4" 
 , bc "tests/input/sh5.cmd" 10 "repl-5" 
 , bc "tests/input/sh6.cmd" 15 "repl-6" 
 ]
 where 
  bc f pts name = binTest sc (BinCmd "stack run --allow-different-user" f (f <.> "out") pts name)

-------------------------------------------------------------------------------
-- | ezlang-Exceptions
-------------------------------------------------------------------------------


ezlang_exn :: Score -> TestTree
ezlang_exn sc = testGroup "Exceptions" 
  [ scoreTest ( ezlangExec, ex_1_2         , ResInt 3, 5, "exn - 1")
  , scoreTest ( ezlangExec, ex_t1_2        , ResInt 1, 5, "exn - 2")
  , scoreTest ( ezlangExec, ex_1_t2        , ResInt 2, 5, "exn - 3")
  , scoreTest ( ezlangExec, ex_t1_t2       , ResInt 1, 5, "exn - 4")
  , scoreTest ( ezlangExec, ex_t12         , ResInt 3, 5, "exn - 5")
  , scoreTest ( ezlangExec, ex_tt12        , ResInt 2, 5, "exn - 6")

  , scoreTest ( ezlangExec, exTry ex_1_2   , ResInt 3 , 5, "exn - 1")
  , scoreTest ( ezlangExec, exTry ex_t1_2  , ResInt 11, 5, "exn - 2")
  , scoreTest ( ezlangExec, exTry ex_1_t2  , ResInt 12, 5, "exn - 3")
  , scoreTest ( ezlangExec, exTry ex_t1_t2 , ResInt 11, 5, "exn - 4")
  , scoreTest ( ezlangExec, exTry ex_t12   , ResInt 13, 5, "exn - 5")
  , scoreTest ( ezlangExec, exTry ex_tt12  , ResInt 12, 5, "exn - 6")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest = scoreTest_ sc
    ezlangExec  = eval []
    ex_1_2    = ExBinary Plus (ExInt 1) (ExInt 2)
    ex_t1_2   = ExBinary Plus (ExThrow (ExInt 1)) (ExInt 2)
    ex_1_t2   = ExBinary Plus (ExInt 1) (ExThrow (ExInt 2)) 
    ex_t1_t2  = ExBinary Plus (ExThrow (ExInt 1)) (ExThrow (ExInt 2)) 
    ex_t12    = ExThrow (ExBinary Plus (ExInt 1)  (ExInt 2))
    ex_tt12   = ExThrow (ExBinary Plus (ExInt 1)  (ExThrow (ExInt 2)))
    exTry e   = ExTry e "z" (ExBinary Plus "z" (ExInt 10))


-------------------------------------------------------------------------------
-- | ezlang-Eval
-------------------------------------------------------------------------------
ezlang_eval :: Score -> TestTree
ezlang_eval sc = testGroup "Eval"
  [ scoreTest ( eval env1
              , ExVariable "c1"
              , ResInt 1
              , 1
              , "1a - c1")
  , scoreTest ( eval env1
              , ExBinary Multiply
                  ( ExBinary Minus (ExInt 20) "c1" )
                  ( ExBinary Plus  "c2" "c3")
              , ResInt 95
              , 3
              , "1a - (20-c1)*(c2+c3)")
  , failTest  ( eval env2
              , ExBinary Plus "bt" "c3"
              , "type error"
              , 1
              , "1b - True + 3")
  , failTest  ( eval env2
              , ExBinary Or "bt" "c3"
              , "type error"
              , 1
              , "1b - bt||c3")
  , scoreTest ( eval env2
              , ExIf (ExBinary LessThan (ExInt 3) (ExInt 3))
                (ExInt 2) (ExInt 4)
              , ResInt 4
              , 1
              , "1b - if (3 < 3) then 2 else 4")
  , scoreTest ( eval []
              , ExLet "x" (ExInt 4) "x"
              , ResInt 4
              , 1
              , "1c - let x = 4 in x")
  , scoreTest ( eval []
              , ExFuncApp (ExLambda "x" (ExBinary Multiply "x" "x")) (ExInt 5)
              , ResInt 25
              , 2
              , "1d - (\\x -> x*x) 5")
  , scoreTest ( eval []
              , (ExLet "fac"
                  (ExLambda "n"
                    (ExIf (ExBinary Equals "n" (ExInt 0))
                      (ExInt 1)
                      (ExBinary Multiply "n"
                        (ExFuncApp "fac"
                          (ExBinary Minus "n" (ExInt 1))))))
                  (ExFuncApp "fac" (ExInt 10)))
              , ResInt 3628800
              , 1
              , "1e - let fac = (\\ n -> if n == 0 then 1 else n * fac (n - 1)) in fac 10")
  , scoreTest ( eval prelude
              , (ExFuncApp "head" (ExBinary Cons (ExInt 1) ExNull))
              , ResInt 1
              , 1
              , "1f - head (1 , nil)")
  , scoreTest ( eval prelude
              , (ExFuncApp "tail" (ExBinary Cons (ExInt 1) ExNull))
              , ResNull
              , 1
              , "1f - tail (1 , nil)")
  , scoreTest ( parse
              , "True"
              , ExBool True
              , 1
              , "2a - \"true\"")
  , scoreTest ( parse
              , "123\n\t"
              , ExInt 123
              , 1
              , "2a - \" 894\\n\\t\"")
  , scoreTest ( parse
              , "x"
              , ExVariable "x"
              , 1
              , "2a - \"Z\"")
  , scoreTest ( parse
              , "let x = 5 in x"
              , ExLet "x" (ExInt 5) (ExVariable "x")
              , 1
              , "2b - let x = 5 in x")
  , scoreTest ( parse
              , "\\x -> 5"
              , ExLambda "x" (ExInt 5)
              , 1
              , "2b - \\x -> 5")
  , scoreTest ( parse
              , "if a then b else c"
              , ExIf (ExVariable "a") (ExVariable "b") (ExVariable "c")
              , 1
              , "2b - if a then b else c")
  , scoreTest ( parse
              , "x + 2"
              ,  ExBinary Plus (ExVariable "x") (ExInt 2)
              , 1
              , "2c - x+2")
  , scoreTest ( parse
              , "x <= 2"
              , ExBinary LessThanOrEquals (ExVariable "x") (ExInt 2)
              , 1
              , "2c - x<=2")
  , scoreTest ( parse
              , "x && 2"
              , ExBinary And (ExVariable "x") (ExInt 2)
              , 1
              , "2c - x&&2")
  , scoreTest ( parse
              , "1 + ( 2 * ( 3 ))"
              , ExBinary Plus (ExInt 1) (ExBinary Multiply (ExInt 2) (ExInt 3))
              , 1
              , "2d - 1+(2*(3))")
  , scoreTest ( parse
              , "f x"
              , ExFuncApp (ExVariable "f") (ExVariable "x")
              , 1
              , "2d - f x")
  , scoreTest ( parse
              , "1+a&&b||c+d*e-f-g x"
              , ExBinary Or (ExBinary And
                                   (ExBinary Plus (ExInt 1) (ExVariable "a"))
                                   (ExVariable "b"))
                (ExBinary Minus (ExBinary Minus
                                       (ExBinary Plus (ExVariable "c")
                                        (ExBinary Multiply (ExVariable "d")
                                         (ExVariable "e")))
                                        (ExVariable "f"))
                  (ExFuncApp (ExVariable "g") (ExVariable "x")))
              , 1
              , "2e - 1+a&&b||c+d*e-f-g x")

  , scoreTest ( parse
              , "e : f"
              , ExBinary Cons (ExVariable "e") (ExVariable "f")
              , 1
              , "2f - e:f")

  , fileTest  ( "tests/input/t1.hs"
              , ResInt 45
              , 1 )
  , fileTest  ( "tests/input/t2.hs"
              , ResInt 0
              , 1 )
  , fileTest  ( "tests/input/t3.hs"
              , ResInt 2
              , 1 )
  , fileTestE ( "tests/input/t4.hs"
              , "bound"
              , 1 )
  , fileTest  ( "tests/input/t5.hs"
              , ResInt 6
              , 1 )
  , fileTest  ( "tests/input/t6.hs"
              , ResInt 102
              , 2 )
  , fileTest  ( "tests/input/t8.hs"
              , ResInt 55
              , 2 )
  , fileTest  ( "tests/input/t9.hs"
              , ResInt 3628800
              , 2 )
  , fileTest  ( "tests/input/t10.hs"
              , ResInt 110
              , 2 )
  , fileTest  ( "tests/input/t11.hs"
              , ResInt 55
              , 2 )
  , fileTest  ( "tests/input/t12.hs"
              , ResInt 3628800
              , 2 )
  , fileTest  ( "tests/input/t13.hs"
              , ResInt 80
              , 2 )
  , fileTest  ( "tests/input/t14.hs"
              , resultList [ResInt 1, ResInt 6, ResInt 7, ResInt 8]
              , 2 )
  , fileTest  ( "tests/input/t15.hs"
              , ResBool False
              , 2 )
  , fileTest  ( "tests/input/t16.hs"
              , resultList [ResInt 2, ResInt 3, ResInt 4, ResInt 5]
              , 3 )
  , fileTest  ( "tests/input/t17.hs"
              , ResInt 10
              , 3 )
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest = scoreTest_ sc
    failTest  = failTest_ sc
    fileTest  = fileTest_ sc
    fileTestE = fileTestE_ sc
    parse     = Ezlang.Eval.parse

scoreTest_ :: (Show b, Eq b) => Score -> (a -> b, a, b, Int, String) -> TestTree
scoreTest_ sc (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)

failTest_ :: (Show b, Eq b) => Score -> (a -> b, a, String, Int, String) -> TestTree
failTest_ sc (f, x, err, n, msg) = scoreTest' sc (expectError err (return . f), x, True, n, msg)

fileTest_ sc (f, r, n)  = scoreTest' sc (execFile, f, r, n, "file: " ++ f)
fileTestE_ sc (f, e, n) = scoreTest' sc (expectError e execFile, f, True, n, "file: " ++ f)


expectError :: (Show b) => String -> (a -> IO b) -> a -> IO Bool
expectError err f x = do { r <- f x; print r; return False }
                      `catch`
                      (return . isInfixOf err . errMsg)

env1 :: Environment
env1 =
  [ ("c0", ResInt 0)
  , ("c1", ResInt 1)
  , ("c2", ResInt 2)
  , ("c3", ResInt 3)
  , ("c0", ResInt 4)
  , ("c1", ResInt 5)
  ]

env2 :: Environment
env2 = env1 ++
  [ ("bt", ResBool True)
  , ("bf", ResBool False)
  ]

