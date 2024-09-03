module Main where

import Types
import ConvertDMN
import PrintProg
import Prettyprinter
import System.Environment (getArgs)
import System.IO (readFile)
import Text.XML.HXT.Core hiding (Schema)
import FromMD
import TypeChecking
import TranslateToSimala

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

                    -- translate to simala
                    putStrLn "simala ver"
                    let simalaDMN = translateToSimala convertedDRD
                    print simalaDMN
        _ -> putStrLn "Please enter as: stack run <input-file>"

    -- let markdownTable = "|U|Mark (input, number)|Result (output, string)|\n\
    --                     \|---|---|---|\n\
    --                     \|1|>=50|\"Pass\"|\n\
    --                     \|2|<50|\"Fail\"|"
    
    -- content <- readFile "input.md"
    -- let markdownTable = content
    -- putStrLn markdownTable

    -- -- parsing markdown to types structure 
    -- let parsedDecision = parseMDToDMN markdownTable
    -- print "Parsed Decision:"
    -- print parsedDecision
    -- putStrLn ""

    -- -- type checking
    -- case typeCheck parsedDecision of
    --     Left errors -> do
    --         putStrLn "Error occured during type checking:"
    --         putStrLn errors
    --     -- convert to IR
    --     Right checkedDecision -> do
    --         putStrLn "Type checking passed."
    --         let convertedDecision = convertDecision checkedDecision
    --         print convertedDecision
    --         putStrLn ""
    --         -- translate to python
    --         print "python ver"
    --         (print . (<>) line . showProg) convertedDecision

    -- print "simala ver"
    -- (print . (<>) line . showProgSimala) convertedRules
