{-# LANGUAGE OverloadedStrings #-}

module Ezlang.Eval
  ( defsFile 
  , execFile, execString, execEnvString, execExpr
  , eval, lookupId, prelude
  , parse
  , env0
  )
  where

import           System.Directory (doesFileExist)
import           Control.Exception (throw, catch)
import           Ezlang.Types
import qualified Ezlang.Parser as Parser
--------------------------------------------------------------------------------
defsFile :: FilePath -> IO [(Id, Expression)]
--------------------------------------------------------------------------------
defsFile f = safeRead f >>= return . Parser.parseDefs

--------------------------------------------------------------------------------
execFile :: FilePath -> IO Result
--------------------------------------------------------------------------------
execFile f = (safeRead f >>= execString) `catch` exitError

safeRead :: FilePath -> IO String
safeRead f = do
  b <- doesFileExist f
  if b then readFile f 
       else throw (Error ("unknown file: " ++ f))

--------------------------------------------------------------------------------
execString :: String -> IO Result
--------------------------------------------------------------------------------
execString s = execExpr (parse s) `catch` exitError

--------------------------------------------------------------------------------
execEnvString :: Environment -> String -> IO Result
--------------------------------------------------------------------------------
execEnvString env s = return (eval env (parse s)) `catch` exitError

--------------------------------------------------------------------------------
execExpr :: Expression -> IO Result
--------------------------------------------------------------------------------
execExpr e = return (eval prelude e) `catch` exitError

--------------------------------------------------------------------------------
-- | `parse s` returns the Expression representation of the String s
--
--------------------------------------------------------------------------------
parse :: String -> Expression
--------------------------------------------------------------------------------
parse = Parser.parseExpr

exitError :: Error -> IO Result
exitError (Error msg) = return (ResErr msg)

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
eval :: Environment -> Expression -> Result
--------------------------------------------------------------------------------
eval env e = case evalE env e of 
  Left exn  -> exn
  Right val -> val


--------------------------------------------------------------------------------
evalE :: Environment -> Expression -> Either Result Result
--------------------------------------------------------------------------------
evalE _ (ExInt int) = Right (ResInt int)
evalE _ (ExBool b ) = Right (ResBool b)
evalE _ (ExNull) = Right (ResNull)
evalE env (ExVariable id)       = case (lookupId id env) of
                              (ResErr s) -> Left (ResErr s)
                              x -> Right x



evalE env (ExBinary op e1 e2) = case (evalE env e1) of
                                Left x -> Left (x)
                                Right x -> case (evalE env e2) of 
                                          Left x -> Left (x)
                                          Right y -> Right (evalBinExp op x y) 
                                  

evalE env (ExIf ifte s1 s2) = case (evalE env ifte) of
                            Right (ResBool True) -> (evalE env s1)
                            Right (ResBool False) -> (evalE env s2) 
                            Left (x) -> Left (x)
                            
evalE env (ExLet tmp e1 e2) = 
    let v1 = evalE env e1 in 
      case (v1) of
        Left (e) -> Left e
        Right (_) -> evalE env' e2
          where 
            env' = ((tmp, eval env' e1) : env)



evalE env (ExLambda id expr) = Right (ResClosure env id expr)

evalE env (ExFuncApp func args) = 
    case (evalE env func) of 
      Right (ResClosure frozenEnv x body) -> evalE env' body
        where 
          Right v = evalE env args
          env' = (x,v) : frozenEnv
      Right (ResPrimitive y) -> Right (y res)
          where
            Right res = (evalE env args)
      _ -> throw (Error "func app error")


evalE env (ExThrow e)       = Left (eval env e) 
evalE env (ExTry e1 x e2) = case (evalE env e1) of
                            Right r -> Right r
                            Left l -> evalE ((x, l) : env) e2



headF :: Result -> Result
headF (ResPair fst _) = fst
headF (ResNull) = throw (Error "empty list error")
headF _ = throw (Error "not list type")

tailF :: Result -> Result
tailF (ResPair _ ResNull) = ResNull
tailF (ResPair _ snd) = snd
tailF (ResNull) = throw (Error "empty list error") 
tailF _ = throw (Error "not list type")


--------------------------------------------------------------------------------
evalBinExp :: BinaryOperator -> Result -> Result -> Result
--------------------------------------------------------------------------------

-- Evaluate the following operations:
-- (+) Plus
-- (-) Minus
-- (*) Multiply
-- (/) Divide 
-- 
evalBinExp Plus (ResInt v1) (ResInt v2) = ResInt (v1 + v2)
evalBinExp Minus (ResInt v1) (ResInt v2) = ResInt (v1 - v2)
evalBinExp Multiply (ResInt v1) (ResInt v2) = ResInt (v1 * v2)
evalBinExp Divide (ResInt v1) (ResInt _) = ResInt (v1)
evalBinExp Equals (ResInt v1) (ResInt v2) = ResBool (v1 == v2)
evalBinExp Equals (ResPair (ResInt v1) y1) (ResPair (ResInt v2) y2) = evalBinExp And (evalBinExp Equals (ResInt v1) (ResInt v2)) (evalBinExp Equals y1 y2)
evalBinExp Equals (ResNull) (ResNull)               = ResBool (True)
evalBinExp Equals (ResPair _ _) (ResNull) = ResBool (False)
evalBinExp Equals (ResNull) (ResPair _ _) = ResBool (False)
evalBinExp Equals (ResBool v1) (ResBool v2) = ResBool (v1 == v2)
evalBinExp NotEquals (ResInt v1) (ResInt v2) = ResBool (v1 /= v2)
evalBinExp NotEquals (ResBool v1) (ResBool v2) = ResBool (v1 /= v2)
evalBinExp LessThan (ResInt v1) (ResInt v2) = ResBool (v1 < v2)
evalBinExp LessThanOrEquals (ResInt v1) (ResInt v2) = ResBool (v1 <= v2)
evalBinExp And (ResBool v1) (ResBool v2) = ResBool (v1 && v2)
evalBinExp Or (ResBool v1) (ResBool v2) = ResBool (v1 || v2)
evalBinExp Cons v1 (ResPair e1 e2)       = ResPair v1 (ResPair e1 e2)  
evalBinExp Cons v1 (ResNull)              = ResPair v1 (ResNull)
evalBinExp _ _ _ = throw (Error "type error: binaryoperation")
 
 

--------------------------------------------------------------------------------
-- | Unit tests for `throw`
--------------------------------------------------------------------------------

-- 1 + 2                 ==> 3
-- (throw 1) + 2         ==> 1
-- 1 + (throw 2)         ==> 2
-- (throw 1) + (throw 2) ==> 1
-- throw (1 + 2)         ==> 3
-- throw (1 + (throw 2)) ==> 2


--------------------------------------------------------------------------------
lookupId :: Id -> Environment -> Result
--------------------------------------------------------------------------------
lookupId o env = case lookup o env of 
    Just val -> val
    Nothing -> throw (Error ("unbound variable: " ++ o))



prelude :: Environment
prelude =
  [ ("head", ResPrimitive(headF)), ("tail", ResPrimitive(tailF))]

env0 :: Environment
env0 =  [ ("z1", ResInt 0)
        , ("x" , ResInt 1)
        , ("y" , ResInt 2)
        , ("z" , ResInt 3)
        , ("z1", ResInt 4)
        ]

--------------------------------------------------------------------------------
