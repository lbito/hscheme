import System.Environment
import Parser
import LispVal
import LispError
import Eval
import Repl
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"
