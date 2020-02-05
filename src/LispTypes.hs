-- Our basic types, also found in traditional lisps.

module LispTypes where

import Control.Monad.Except ( ExceptT(..) )

import Data.Bool     ( Bool(..) )
import Data.Either   ( Either(..) )
import Data.Function ( (.) )
import Data.IORef    ( IORef(..) )
import Data.List     ( (++), map, unwords )
import Data.Maybe    ( Maybe(..) )
import Data.String   ( String )

import GHC.Integer ( Integer(..) )

import System.IO ( Handle(..), IO(..) )

import Text.Show ( Show(..), show )
import Text.ParserCombinators.Parsec ( ParseError(..) )


unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

-- Our cool type to get a *mutable* environment.
-- It is needed here, but Environment.hs needs LispVal,
-- we would have cyclic imports.
type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO
type ThrowsError = Either LispError

-- What about quasiquoting?
-- Maybe use vectors instead of [LispVal].
data LispVal = Atom String
             | List [LispVal]
          -- Don't get too heroic
          -- | Dependent LispVal
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
          -- | Comment [LispVal]
             | Port Handle
          -- Maybe one day ... *seufz*
          -- OR we use this project to show how to communicate
          -- at runtime with CL to have macroexpansion for free!
          -- Of course ZeroMQ is as ever the holy grail.
          -- And if that works use the expansion code in Scheme.
          -- | Macro [LispVal]
          -- | Quote [LispVal]
          -- | QuasiQuote [LispVal]
          -- That would be nice!
          -- | Lazy [LispVal]
          -- | Recursive [LispVal]
          -- | Typed [LispVal]
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Func { params  :: [String]
                    , vararg  :: (Maybe String)
                    , body    :: [LispVal]
                    , closure :: Env }

instance Show LispVal where
    show (Atom name) = name
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head
                                      ++ " . " ++ show tail ++ ")"
    show (Number contents) = show contents
    show (String contents) = "\"" ++ contents ++ "\""
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (Port _) = "<IO port>"
    show (PrimitiveFunc _) = "<primitive>"
    show (IOFunc _) = "<IO primitive"
    show (Func { params = args, vararg  = varargs
               , body   = body, closure = env
               }) =
        "(lambda (" ++ unwords (map show args) ++
          (case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg) ++ ") ...)"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) =
           "Expected "
        ++ show expected
        ++ " args: found values "
        ++ unwordsList found
    show (TypeMismatch expected found) =
           "Invalid type: expected "
        ++ expected ++ ", found "
        ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr
