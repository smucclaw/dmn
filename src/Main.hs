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
    -- xmlContent <- readFile "camunda/camunda.dmn"

    -- decisions <- runX (readString [withValidate no] xmlContent
    --                    >>> deep (isElem >>> hasName "definitions") 
    --                    >>> getChildren >>> isElem >>> hasName "decision"
    --                    >>> parseDecision)

    -- case decisions of
    --     [] -> putStrLn "No decision found in the XML file."
    --     (decision:_) -> do
    --         print "Parsed Decision:"
    --         print decision
            
    --         print "Converted Rules:"
    --         let convertedRules = convertDecision decision
    --         print convertedRules
            
    --         print "Python version:"
    --         (print . (<>) line . showProg) convertedRules

    let markdownTable = "|F|stage (input, string)|sector (input, string)|stage_com (input, string)|has_ESG (input, bool)|wants_ESG (input, bool)|opinion (output, string)|\n|---|---|---|---|---|---|---|\n|1|Seed|Information Technology|Pre-Revenue|-|-|interesting|\n|2|Series A|Information Technology|Pre-Profit|-|-|interesting|\n|3|-|-|-|TRUE|TRUE|interesting|\n|4|-|-|-|-|-|reject|"
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
