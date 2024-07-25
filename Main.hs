module Main where

import Types
import ConvertDMN

-- testConvertDecision :: Bool
-- testConvertDecision = 
--     convertDecision exampleDecision == rule1

-- main :: IO ()
-- main = do
--     putStrLn "Testing conversion of decision table to IR..." ++ show testConvertDecision

main :: IO ()
main = do
    let convertedRules = convertDecision exampleDecision
    print convertedRules
