{-# LANGUAGE OverloadedStrings #-}

module Ezlang.Types where

import           GHC.Exts( IsString(..) )
import           Text.Printf (printf)

import           Control.Exception
import           Data.Typeable
import qualified Data.List as L

data Error = Error {errMsg :: String}
             deriving (Show, Typeable)

instance Exception Error

--- BinaryOperators
data BinaryOperator
  = Plus
  | Minus
  | Multiply
  | Divide
  | Equals
  | NotEquals
  | LessThan
  | LessThanOrEquals
  | And
  | Or
  | Cons
  deriving (Eq, Ord, Show)

type Id = String

instance IsString Expression where
  fromString = ExVariable


--- What is an Expression? Anything that can be evaluated in the interpreter.
data Expression
  = ExInt  Int                                           
  | ExBool Bool                                             
  | ExNull                                                  
  | ExVariable Id                                                
  | ExBinary BinaryOperator Expression Expression      
  | ExIf  Expression Expression  Expression      
  | ExLet Id   Expression  Expression      
  | ExFuncApp Expression Expression            
  | ExLambda Id   Expression             
  | ExTry Expression Id    Expression           
  | ExThrow Expression                          
  deriving (Eq, Show)

--- What is a result? Anything that can be returned in the interpreter.
data Result
  = ResInt  Int
  | ResBool Bool
  | ResClosure   Environment Id Expression
  | ResNull
  | ResPair Result Result
  | ResErr  String
  | ResPrimitive (Result -> Result)

--- The Environment is just a mapping of "Strings" to Result. 
type Environment = [(Id, Result)]

--- Override the Result Equality: What does it mean to be Equal?
instance Eq Result where
    (ResInt x1)         == (ResInt x2)     = x1 == x2
    (ResBool x1)        == (ResBool x2)    = x1 == x2
    ResNull             == ResNull         = True
    (ResPair x1 y1)     == (ResPair x2 y2) = x1 == x2 && y1 == y2
    _                   == _               = False

instance Show Result where
  show = resultToString


binaryOperatorToString :: BinaryOperator -> String
binaryOperatorToString Plus  = "+"
binaryOperatorToString Minus = "-"
binaryOperatorToString Multiply   = "*"
binaryOperatorToString Divide   = "/"
binaryOperatorToString Equals    = "="
binaryOperatorToString NotEquals   = "!="
binaryOperatorToString LessThan    = "<"
binaryOperatorToString LessThanOrEquals    = "<="
binaryOperatorToString And   = "&&"
binaryOperatorToString Or    = "||"
binaryOperatorToString Cons  = ":"

resultToString :: Result -> String
resultToString (ResInt i)        = printf "%d" i
resultToString (ResBool b)       = printf "%s" (show b)
resultToString (ResClosure env x v) = printf "<<%s, \\%s -> %s>>" (envString env) x (show v)
resultToString (ResPair v w)     = printf "(%s : %s)" (show v) (show w)
resultToString (ResErr s)        = printf "ERROR: %s" s
resultToString ResNull            = "[]"
resultToString (ResPrimitive _)       = "<<primitive-function>>"

envString :: Environment -> String
envString env = printf "{ %s }" (L.intercalate ", " bs)
  where
    bs        = [ x ++ " := " ++ show v | (x, v) <- env]

expressionToString :: Expression -> String
expressionToString (ExInt i)       = printf "%d" i
expressionToString (ExBool b)      = printf "%s" (show b)
expressionToString (ExVariable x)       = x
expressionToString (ExBinary o e1 e2) = printf "(%s %s %s)" (show e1) (show o) (show e2)
expressionToString (ExIf c t e)    = printf "if %s then %s else %s" (show c) (show t) (show e)
expressionToString (ExLet x e e')  = printf "let %s = %s in \n %s" x (show e) (show e')
expressionToString (ExFuncApp e1 e2)   = printf "(%s %s)" (show e1) (show e2)
expressionToString (ExLambda x e)     = printf "\\%s -> %s" x (show e)
expressionToString ExNull           = "[]"
expressionToString (ExTry e1 x e2) = printf "try (%s) handle %s => %s" (show e1) x (show e2)
expressionToString (ExThrow e)       = printf "(throw %s)" (show e)


------------------------------------------------------------------------------
class Ezlang a where
  expr  :: a -> Expression
  result :: a -> Result

instance Ezlang Int where
  expr  = ExInt
  result = ResInt

instance Ezlang Bool where
  expr  = ExBool
  result = ResBool

exprList :: [Expression] -> Expression
exprList = foldr (ExBinary Cons) ExNull

resultList :: [Result] -> Result
resultList = foldr ResPair ResNull
