module Main where

import Types
import ConvertDMN
import PrintProg
import PrintProgSimala
import Prettyprinter
import System.IO (readFile)
import Text.XML.HXT.Core hiding (Schema)
import FromMD

-- testConvertDecision :: Bool
-- testConvertDecision = 
--     convertDecision exampleDecision == rule1

-- main :: IO ()
-- main = do
--     putStrLn "Testing conversion of decision table to IR..." ++ show testConvertDecision

main :: IO ()
main = do
    let markdownTable = "|U|Mark (input, number)|Result (output, string)|\n\
                        \|---|---|---|\n\
                        \|1|>=50|Pass|\n\
                        \|2|<50|Fail|"
    -- content <- readFile "input.md"
    -- let markdownTable = content
    -- putStrLn markdownTable

    let decision = convertMDToDMN markdownTable
    print "Parsed Decision:"
    print decision
    putStrLn ""
    let convertedRules = convertDecision decision
    print convertedRules
    putStrLn ""
    print "python ver"
    (print . (<>) line . showProg) convertedRules
    -- print "simala ver"
    -- (print . (<>) line . showProgSimala) convertedRules
