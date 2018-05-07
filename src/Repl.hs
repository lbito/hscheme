module Repl where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except
import System.IO
import LispVal
import Eval
import Env
import Parser

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
     Left err -> throwError $ Parser err
     Right val -> return val

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do result <- prompt
                               if pred result
                                 then return ()
                                 else action result >> until_ pred prompt action

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
