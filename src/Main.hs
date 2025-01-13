module Main where

import Types
import ConvertDMN
import PrintProg
import PrintProgJavascript
import Prettyprinter
import System.Environment ( getArgs, unsetEnv )
import System.IO (readFile, writeFile)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.Process (callCommand)
import Text.XML.HXT.Core hiding (Schema)
import FromMD
import TypeChecking
import qualified Data.Text.IO as T
import qualified Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            content <- readFile inputFile
            putStrLn content
            
            putStrLn ""

            let (parsedDRD, varMap) = parseMDToDMN content
            print parsedDRD
            putStrLn "Final var map: "
            print varMap
            putStrLn ""

            -- type checking
            case typeCheck parsedDRD varMap of
                Left errors -> do
                    putStrLn "Error occurred during type checking:"
                    putStrLn errors

                -- convert to IR
                Right checkedDRD -> do
                    putStrLn "Type checking passed."
                    putStrLn ""
                    
                    let convertedDRD = convertDRD checkedDRD
                    print convertedDRD
                    putStrLn ""

                    -- translate to python
                    putStrLn "python transpilation"
                    (print . (<>) line . showProg) convertedDRD
                    putStrLn ""

                    -- translate to javascript
                    putStrLn "javascript transpilation"
                    (print . (<>) line . showProgJs) convertedDRD
                    putStrLn ""

        _ -> putStrLn "Please enter as: stack run <input-file>"
