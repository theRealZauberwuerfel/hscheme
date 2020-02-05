module Main ( main ) where

import Data.Function ( ($) )
import Data.List ( null )

import HSchemeRepl ( runOne, runRepl )

import System.Environment ( getArgs )
import System.IO ( IO(..) )


main :: IO ()
main = do
    args <- getArgs
    if null args
      -- then runInputT defaultSettings runRepl
      then runRepl
      else runOne $ args
