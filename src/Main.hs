module Main where

import Types
import ConvertDMN
import PrintProg
import PrintProgJavascript
import Prettyprinter
import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import System.Directory (setCurrentDirectory, getCurrentDirectory)
import System.Process (callCommand)
import System.Environment (unsetEnv)
import Text.XML.HXT.Core hiding (Schema)
import FromMD
import TypeChecking
import TranslateToSimala
import RenderPretty (render)
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

                    -- translate to simala ast
                    putStrLn "simala ast"
                    let simalaDMN = translateToSimala convertedDRD
                    print simalaDMN
                    putStrLn ""

                    -- translate to simala
                    putStrLn "simala transpilation"
                    let simalaProg = render simalaDMN
                    T.putStrLn $ render simalaDMN
                    putStrLn ""

                    setCurrentDirectory "/root/cclaw/dmn/simala"

                    T.writeFile "try.simala" simalaProg
                    unsetEnv "GHC_PACKAGE_PATH"
                    callCommand "cabal run simala -- try.simala"

        _ -> putStrLn "Please enter as: stack run <input-file>"
