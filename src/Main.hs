module Main where

import Types
import ConvertDMN
import PrintProg
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
import Base.Pretty
import qualified Data.Text.IO as T

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile] -> do
            content <- readFile inputFile
            putStrLn content
            
            putStrLn ""

            let parsedDRD = parseMDToDMN content
            print parsedDRD
            putStrLn ""

            -- let convertedDRD = convertDRD parsedDRD
            -- print convertedDRD
            -- putStrLn ""

            -- putStrLn "python ver"
            -- (print . (<>) line . showProg) convertedDRD
            -- putStrLn ""

            -- putStrLn "simala ver"
            -- let simalaDMN = translateToSimala convertedDRD
            -- print simalaDMN

            -- type checking
            case typeCheck parsedDRD of
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
                    putStrLn "python ver"
                    (print . (<>) line . showProg) convertedDRD
                    putStrLn ""

                    -- translate to simala ast
                    putStrLn "simala ast"
                    let simalaDMN = translateToSimala convertedDRD
                    print simalaDMN
                    putStrLn ""

                    -- translate to simala
                    putStrLn "simala ver"
                    let simalaProg = render simalaDMN
                    T.putStrLn $ render simalaDMN

                    setCurrentDirectory "/root/cclaw/dmn/simala"

                    T.writeFile "try.simala" simalaProg
                    unsetEnv "GHC_PACKAGE_PATH"
                    callCommand "cabal run simala -- try.simala"

        _ -> putStrLn "Please enter as: stack run <input-file>"
