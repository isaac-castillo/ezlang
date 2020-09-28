-- | This module has various "utilities" that you can use to build a REPL. 

module Ezlang.Repl where

import           Control.Exception
import           System.Exit
import           System.IO
import qualified Data.List as L
import qualified Data.Char as Char
import           Ezlang.Types 
import           Ezlang.Eval  

--------------------------------------------------------------------------------
welcome :: String
--------------------------------------------------------------------------------
welcome = unlines
  [ "------------------------------------------------------------"
  , "-------- The EZLANG Interpreter V.1.3.3.7 ------------------"
  , "------------------------------------------------------------"
  ]

--------------------------------------------------------------------------------
putStrFlush :: String -> IO ()
--------------------------------------------------------------------------------
putStrFlush str = do 
  putStr str
  hFlush stdout

--------------------------------------------------------------------------------
doQuit :: IO a 
--------------------------------------------------------------------------------
doQuit = do 
  putStrLn "Goodbye." 
  exitWith ExitSuccess 

--------------------------------------------------------------------------------
doEval :: Environment -> String -> IO ()
--------------------------------------------------------------------------------
doEval env s = (print =<< execEnvString env s) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doUnknown :: IO () 
--------------------------------------------------------------------------------
doUnknown = putStrLn "I'm sorry Dave, I'm sorry I can't do that..."

--------------------------------------------------------------------------------
doRun :: FilePath -> IO ()
--------------------------------------------------------------------------------
doRun f = (print =<< execFile f) `catch` (putStrLn . errMsg)

--------------------------------------------------------------------------------
doLoad :: FilePath -> IO Environment
--------------------------------------------------------------------------------
doLoad f = (defsEnv =<< defsFile f) `catch` exitEnv

exitEnv :: Error -> IO Environment
exitEnv err = putStrLn (errMsg err) >> return prelude 


--------------------------------------------------------------------------------
-- HINT: You may want to implement `defsEnv` and then use `doLoad`
--------------------------------------------------------------------------------
defsEnv :: [(Id, Expression)] -> IO Environment
--------------------------------------------------------------------------------
defsEnv xes =  return (makeEnv prelude xes)
                where 
                  makeEnv env [] = reverse env
                  makeEnv env (x:xs) = makeEnv env' xs
                    where env' = (fst x, (eval env (snd x))) : env
                  
--------------------------------------------------------------------------------
-- | A Datatype Commands for the shell -----------------------------------------
--------------------------------------------------------------------------------

data Cmd 
  = Eval String   
  | Run  FilePath 
  | Load FilePath 
  | Quit          
  | Unknown       
  deriving (Show)

strCmd :: String -> Cmd
strCmd str | (L.isPrefixOf pfxLoad (chomp 1 str)) = Load (chomp 5 str)
           | (L.isPrefixOf pfxRun (chomp 1 str)) = Run (chomp 4 str)
           | (L.isPrefixOf pfxQuit (chomp 1 str)) = Quit
           | otherwise = Eval str 

-- HINT: You may want to use the below functions and `L.isPrefixOf`, `chomp`, `pfxRun`, 

chomp :: Int -> String -> String
chomp n s = dropWhile Char.isSpace (drop n s)

pfxRun, pfxLoad, pfxQuit :: String 
pfxRun  = "run"
pfxLoad = "load"
pfxQuit = "quit"

