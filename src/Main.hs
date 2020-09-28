import Ezlang.Types
import Ezlang.Eval
import           Ezlang.Repl
import           GHC.IO.Encoding

loop :: Int -> Environment -> IO ()
loop i env = do
        putStrFlush ("λ [" ++ show i ++ "] ")
        command <- getLine  
         
        case (strCmd command )of

          Quit     -> do 
                       doQuit
                       loop (i+1) env
          Run str  -> do
                       doRun str
                       loop (i+1) env
          Eval str -> do 
                       doEval env str
                       loop (i+1) env
          Load str -> do 
                        a <- (doLoad str)
                        let env' = setEnv [] a
                        let res = getEnv env'
                        putStrFlush ("definitions: " ++ reverse (chomp 1 (reverse res))  ++ "\n")
                        loop (i+1) env'
        
getEnv :: Environment -> String
getEnv ((x,_):xs) = (x ++ " ") ++ getEnv xs
getEnv []         = ""

setEnv :: Environment-> Environment -> Environment
setEnv curr new = new ++ curr

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStrLn welcome
  loop 0 prelude