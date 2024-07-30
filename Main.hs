module Main where

import Types
import ConvertDMN
import PrintProg
import Prettyprinter

-- testConvertDecision :: Bool
-- testConvertDecision = 
--     convertDecision exampleDecision == rule1

-- main :: IO ()
-- main = do
--     putStrLn "Testing conversion of decision table to IR..." ++ show testConvertDecision

main :: IO ()
main = do
    let convertedRules = convertDecision exampleDecision2
    print convertedRules
    (print . (<>) line . showProg) convertedRules
