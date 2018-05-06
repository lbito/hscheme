import System.Environment
import Parser
import LispVal
import LispError
import Eval
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val


main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
