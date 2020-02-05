module IOPrimitives where

import Control.Monad ( liftM )
import Control.Monad.Trans ( liftIO )

import EvalApply
import Environment ( liftThrows )

import LispParser ( readExpr )

import LispTypes ( IOThrowsError, LispVal(..) )

import System.IO ( IOMode(..), hClose, hGetLine, hPrint, openFile, stdin, stdout )


ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [ ("apply",             applyProc)
               , ("open-input-file",   makePort ReadMode)
               , ("open-output-file",  makePort WriteMode)
               , ("close-input-port",  closePort)
               , ("close-output-port", closePort)
               , ("read",              readProc)
               , ("write",             writeProc)
               , ("read-contents",     readContents)
               , ("read-all",          readAll) ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $
    openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>=
    liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >>
    (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO
                               $ readFile filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
