module Main where

import Types
import ConvertDMN
import PrintProg
import PrintProgSimala
import Prettyprinter
import FromXML
import System.IO (readFile)
import Text.XML.HXT.Core hiding (Schema)

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


    let convertedRules = convertDecision exampleDecision
    print convertedRules
    print "python ver"
    (print . (<>) line . showProg) convertedRules
    print "simala ver"
    (print . (<>) line . showProgSimala) convertedRules
