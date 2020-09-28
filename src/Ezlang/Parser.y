{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Ezlang.Parser (
    parseExpr
  , parseDefs
  , parseTokens
  ) where

import Ezlang.Lexer
import Ezlang.Types
import Control.Monad.Except
import Control.Exception

}

-- Entry point
%name top

-- Lexer structure
%tokentype { Token }

-- Parser monad
%monad { Except String } { (>>=) } { return }
%error { parseError }

-- Token Names
%token
    let   { LET _    }
    true  { TRUE _   }
    false { FALSE _  }
    in    { IN _     }
    if    { IF _     }
    then  { THEN _   }
    else  { ELSE _   }
    TNUM  { NUM _ $$ }
    ID    { ID _ $$  }
    '\\'  { LAM _    }
    '->'  { ARROW _  }
    '='   { EQB _    }
    '+'   { PLUS _   }
    '-'   { MINUS _  }
    '*'   { MUL _    }
    '&&'  { AND _    }
    '||'  { OR  _    }
    '=='  { EQL _    }
    '/='  { NEQ _    }
    '<'   { LESS _   }
    '<='  { LEQ _    }
    ':'   { COLON _  }
    '('   { LPAREN _ }
    ')'   { RPAREN _ }
    '['   { LBRAC _  }
    ']'   { RBRAC _  }
    ','   { COMMA _  }


-- Operators
%right in
%right if then else
%left '||' 
%left '&&'
%right ':' '->'
%nonassoc '=' '==' '/=' '<' '<=' 
%left '+' '-'
%left '*'
%%

Top  : Def                        { [$1] } 
     | Def ',' Top                { $1 : $3 }
     | Expr                       { [("", $1) ]}

Def  : ID '=' Expr                 { ($1, $3) }


Args : ID                          { [$1] }
     | ID Args                      { $1 : $2 }

Lists : TNUM                           { ExBinary Cons (ExInt $1) ExNull }
      | TNUM ',' Lists                 { ExBinary Cons (ExInt $1) $3 }
      | TNUM ':' Lists                 { ExBinary Cons (ExInt $1) $3 }
      | ID   ',' Lists                 { ExBinary Cons (ExVariable $1) $3}  
      | ID   ':' Lists                 { ExBinary Cons (ExVariable $1) $3}  
      | ID                             { ExBinary Cons (ExVariable $1) ExNull }  
      | Expr                           { ExBinary Cons $1 ExNull}
      | Expr ',' Lists                 { ExBinary Cons $1 $3 }
      


Appl : ID                                 { ExVariable $1}
     | TNUM                                {ExInt $1 }
     | '(' Expr ')'                         { $2 }
     | Appl TNUM                          { ExFuncApp $1 (ExInt $2) }
     | Appl ID                           { ExFuncApp $1 (ExVariable $2) }
     | Appl  '(' Expr ')'                 { ExFuncApp $1 $3}
     | Appl  '[' Lists ']'                 { ExFuncApp $1 $3}
     | Appl  '[' ']'                 { ExFuncApp $1 ExNull}

Expr : TNUM                        { ExInt $1 }
     | true                        { ExBool True }
     | false                       { ExBool False }
     | ID                          { ExVariable $1}
     | let ID '=' Expr in Expr     { ExLet $2 $4 $6}
     | let ID Args '=' Expr in Expr  { ExLet $2 (mkLam $3 $5) $7}
     | '\\' ID '->' Expr           { ExLambda $2 $4}
     | if Expr then Expr else Expr { ExIf $2 $4 $6}

     | Appl                             { $1 }
     | Expr '+' Expr               { ExBinary Plus $1 $3}
     | Expr '*' Expr               { ExBinary Multiply $1 $3}
     | Expr '-' Expr               { ExBinary Minus $1 $3}
     | Expr '<' Expr              { ExBinary LessThan $1 $3}
     | Expr '&&' Expr              { ExBinary And $1 $3}
     | Expr '||' Expr              { ExBinary Or $1 $3}
     | Expr '==' Expr              { ExBinary Equals $1 $3}
     | Expr '<=' Expr              { ExBinary LessThanOrEquals $1 $3}
     | Expr '/=' Expr              { ExBinary NotEquals $1 $3}
     | '[' ']'                     { ExNull }
     | '[' Lists ']'               { $2 }
     | Expr ':' Expr               { ExBinary Cons $1 $3}
     | '(' Expr ')'                { $2 }


{
mkLam :: [Id] -> Expression -> Expression
mkLam []     e = e
mkLam (x:xs) e = ExLambda x (mkLam xs e)

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError []     = throwError "Unexpected end of Input"

parseExpr :: String -> Expression
parseExpr s = case parseDefs' s of
                Left msg         -> throw (Error ("parse error:" ++ msg))
                Right ((_,e):_)  -> e

parseDefs :: String -> [(Id, Expression)]
parseDefs s = case parseDefs' s of 
                Left msg -> throw (Error ("parse error:" ++ msg))
                Right e  -> e
                
parseDefs' input = runExcept $ do
   tokenStream <- scanTokens input
   top tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . scanTokens


}
