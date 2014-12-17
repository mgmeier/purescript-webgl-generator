-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  GPL-2
--
-- Maintainer  :  jnf@arcor.de
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main where


import IDL.AST
import IDL.Parser
import IDL.Printer

import System.Console.GetOpt (usageInfo)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Text.Parsec as PP (runParser)
import qualified Text.Parsec.Error as PP
       (errorMessages, messageString)
import Debug.Trace (trace)
import Text.PrettyPrint (render)

main :: IO ()
main = do
    args <- getArgs
    fp <- case args of
            [] ->  do
                putStrLn $ "Generator requires input idl file"
                exitSuccess
            (hd:_) -> return (hd)
    string <- readFile fp
    case PP.runParser idlParser () "" string of
        Left error -> mapM_ (putStrLn . PP.messageString) (PP.errorMessages error)
        Right idl -> putStr (render (ppPureScriptFFI idl))

