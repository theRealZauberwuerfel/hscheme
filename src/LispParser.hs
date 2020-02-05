module LispParser where

import Control.Monad ( (>>), liftM, return )
import Control.Monad.Except ( throwError )

import Data.Bool     ( Bool(..) )
import Data.Char     ( Char(..) )
import Data.Either   ( Either(..) )
import Data.Function ( ($), (.) )
import Data.List     ( (++) )
import Data.String   ( String )

import Text.ParserCombinators.Parsec (
      (<|>)
    , Parser(..)
    , char
    , digit
    , endBy
    , letter
    , many
    , many1
    , noneOf
    , oneOf
    , parse
    , sepBy
    , skipMany1
    , space
    , try )
import Text.Read ( read )

import LispTypes ( LispError(..), LispVal(..), ThrowsError )

symbol :: Parser Char
symbol =  oneOf "!$%&|*+-/:<=>?@^_~#"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
    Left  err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

spaces :: Parser ()
spaces =  skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = [first] ++ rest
    return $ case atom of
               "#t" -> Bool True
               "#f" -> Bool False
               otherwise -> Atom atom

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\'' <|> char '`'
    x <- parseExpr
    return $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x
