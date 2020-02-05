module HSchemeRepl ( runOne, runRepl ) where

import Control.Monad ( (>>), (>>=), Monad(..), liftM, return )

import Data.Bool     ( Bool(..) )
import Data.Eq       ( (==) )
import Data.Function ( ($), (.), flip )
import Data.List     ( (!!), drop, map )
import Data.String   ( String )

import Environment ( bindVars, liftThrows, runIOThrows )

import EvalApply ( eval )

import LispParser ( readExpr )

import Primitives ( primitiveBindings )

-- | Choose which one suits better.
-- import System.Console.Haskeline
-- import System.Console.Repline
import System.IO ( IO(..), getLine, hFlush, hPutStrLn, putStr, stderr, stdout )

import Text.Show ( show )

import LispTypes ( Env, LispVal(..) )

-- type Repl a = InputT IO a

runRepl :: IO ()
runRepl = primitiveBindings >>=
    until_ (== "quit") (readPrompt "λ>>> ") . evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>=
        flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env
        (List [Atom "load", String (args !! 0)])) >>=
        hPutStrLn stderr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show
    $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
      then return ()
      else action result >> until_ pred prompt action

{-
runRepl' :: Repl ()
runRepl' = do
    uinput <- getInputLine "Repl> "
    case uinput of
      Nothing -> outputStrLn "Bye."
      Just input -> (liftIO $ putStrLn input) >> runRepl'
-}
