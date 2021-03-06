{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wunused-imports #-}
module EvalApply where

import Control.Monad        ( (>>=), liftM, mapM, return )
import Control.Monad.Except ( throwError )
import Control.Monad.Trans  ( liftIO )

import Data.Bool     ( (&&), Bool(..) )
import Data.Eq       ( (/=), (==) )
import Data.Function ( ($), (.) )
import Data.List     ( drop, last, length, map, zip )
import Data.Maybe    ( Maybe(..) )
import Data.String   ( String )

import Environment ( bindVars, defineVar, getVar, liftThrows, setVar )

import GHC.Real ( toInteger )

import LispParser ( readExprList )
import LispTypes  ( Env, IOThrowsError, LispError(..), LispVal(..) )

import System.IO ( readFile )

import Text.Show ( show )


makeFunc varargs env params body = return $
    Func (map show params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . show

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>=
    liftThrows . readExprList

-- |
-- eval: Steve Russell's universal function
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    case result of
      Bool False -> eval env alt
      otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
      then throwError $ NumArgs (num params) args
      else (liftIO $ bindVars closure $ zip params args) >>=
          bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
          Nothing -> return env
