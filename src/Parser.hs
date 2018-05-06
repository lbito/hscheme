module Parser where


import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Ratio

import LispVal

--TODO add complex number parsing
--TODO add vector parsing

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"nrt"
                  return $ case x of
                    '\\' -> x
                    '\"' -> x
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space") 
                      <|> do { x <- anyChar; notFollowedBy alphaNum ; return [x] }
                    return $ Character $ case value of
                      "space" -> ' '
                      "newline" -> '\n'
                      otherwise -> (value !! 0)

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (escapedChars <|> noneOf "\"\\")
                 char '"'
                 return (String x)

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ Atom atom

parseBool :: Parser LispVal
parseBool = do first <- try $ char '#'
               val <- try $ oneOf ("tf")
               return $ case val of
                 't' -> Bool True
                 'f' -> Bool False

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number . fst $ readHex x !! 0

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number . fst $ readOct x !! 0

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dec x)

parseLiteralDecimal :: Parser LispVal
parseLiteralDecimal = many1 digit >>= return . Number . read

parseDecimal :: Parser LispVal
parseDecimal = do try $ string "#d"
                  x <- many1 digit
                  (return . Number . read) x

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head$readFloat (x++"."++y))

parseNumber :: Parser LispVal
parseNumber = parseLiteralDecimal <|> parseHex <|> parseDecimal <|> parseOct <|> parseBin

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseList :: Parser LispVal
parseList = (sepBy parseExpr spaces) >>= \x -> (return . List) x

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseRatio
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> try parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val
