import System.Environment
import Parser
import LispVal
import Eval

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
