module Primitives where

import Control.Monad ( (>>=), mapM, return )
import Control.Monad.Except ( throwError )
import Classic ( car, cdr, cons, eqv )
import Data.Function ( ($), flip )
import Data.List ( (!!), (++), foldl1, map )
import Environment ( bindVars, nullEnv )
import GHC.Integer ( Integer )
import IOPrimitives ( ioPrimitives )
import LispTypes ( Env, LispError(..), LispVal(..), ThrowsError(..) )
import System.IO ( IO(..) )
import Unpack ( equal, unpackBool, unpackNum, unpackStr )


primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=
    (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                  ++ map (makeFunc PrimitiveFunc)
                         primitives)
  where makeFunc constructor (var, func) = (var, constructor func)

-- Make HashTable?
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("expt", numericBinop (^))
             , ("mod",  numericBinop mod)
             , ("quot", numericBinop quot)
             , ("rem",  numericBinop rem)

             , ("=",  numBoolBinop (==))
             , ("<",  numBoolBinop (<))
             , (">",  numBoolBinop (>))
             , ("/=", numBoolBinop (/=))
             , (">=", numBoolBinop (>=))
             , ("<=", numBoolBinop (<=))

             , ("&&", boolBoolBinop (&&))
             , ("||", boolBoolBinop (||))

             , ("string=?",  strBoolBinop (==))
             , ("string<?",  strBoolBinop (<))
             , ("string>?",  strBoolBinop (>))
             , ("string<=?", strBoolBinop (<=))
             , ("string>=?", strBoolBinop (>=))

             , ("car",    car)
             , ("cdr",    cdr)
             , ("cons",   cons)
             , ("eq?",    eqv)
             , ("eqv?",   eqv)
             , ("equal?", equal) ]

boolBinop :: (LispVal -> ThrowsError a)
          -> (a -> a -> Bool) -> [LispVal]
          -> ThrowsError LispVal
boolBinop unpacker op args =
    if length args /= 2
      then throwError $ NumArgs 2 args
      else do
          left <- unpacker $ args !! 0
          right <- unpacker $ args !! 1
          return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
