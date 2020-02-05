-- Is needed to have `forall`.
{-# LANGUAGE ExistentialQuantification #-}

module Unpack (

      Unpacker(..)
    , equal
    , unpackEquals
    , unpackNum
    , unpackStr
    , unpackBool

    ) where

import Classic ( eqv )

import Control.Monad ( liftM, mapM, return )
import Control.Monad.Except ( throwError )

import Data.Bool     ( (||), Bool(..) )
import Data.Eq       ( Eq(..) )
import Data.Function ( ($) )
import Data.List     ( (!!), null, or )
import Data.String   ( String )
import Data.Tuple    ( fst )

import GHC.Integer ( Integer )

import LispTypes ( LispError(..), LispVal(..), ThrowsError )

import Text.Read ( reads )
import Text.Show ( show )


data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n
    in  if null parsed
          then throwError $ TypeMismatch "number" $ String n
          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                          [ AnyUnpacker unpackNum
                          , AnyUnpacker unpackStr
                          , AnyUnpacker unpackBool ]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList
