module Main where

import Types
import ConvertDMN
import PrintProg
import PrintProgSimala
import Prettyprinter
import System.IO (readFile)
import Text.XML.HXT.Core hiding (Schema)
import FromMD
import TypeChecking

main :: IO ()
main = do
    -- let markdownTable = "|U|Mark (input, number)|Result (output, string)|\n\
    --                     \|---|---|---|\n\
    --                     \|1|>=50|\"Pass\"|\n\
    --                     \|2|<50|\"Fail\"|"
    content <- readFile "input.md"
    let markdownTable = content
    putStrLn markdownTable

    -- parsing markdown to types structure 
    let parsedDecision = parseMDToDMN markdownTable
    print "Parsed Decision:"
    print parsedDecision
    putStrLn ""

    -- type checking
    case typeCheck parsedDecision of
        Left errors -> do
            putStrLn "Error occured during type checking:"
            putStrLn errors
        -- convert to IR
        Right checkedDecision -> do
            putStrLn "Type checking passed."
            let convertedDecision = convertDecision checkedDecision
            print convertedDecision
            putStrLn ""
            -- translate to python
            print "python ver"
            (print . (<>) line . showProg) convertedDecision

    -- print "simala ver"
    -- (print . (<>) line . showProgSimala) convertedRules
