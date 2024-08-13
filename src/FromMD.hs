
module FromMD where

import Types
import Data.List.Split (splitOn)
import Data.Char (toLower, isDigit)

-- Main function to convert markdown table to Decision
convertMDToDMN :: String -> Decision
convertMDToDMN tableString = parseDecisionTable tableString

parseDecisionTable :: String -> Decision
parseDecisionTable input = 
  let (headers, body) = parseMDTable input
      inputSchemaNames = parseInputSchemas (init (tail headers))
  in Decision { decisionOut = parseDecisionOutput (last headers)
    , decisionInfoReq = parseInfoReqs (init (tail headers))
    , decisionLogic = DecTable 
        { hitPolicy = head headers
        , schema = Schema 
            { sInputSchemas = inputSchemaNames
            , sOutputSchema = parseOutputSchema (last headers) -- rn taking output as last column only, need to fix
            }
        , rules = parseRules body inputSchemaNames
    }
  }

parseMDTable :: String -> ([String], [[String]]) -- produces a tuple of headers (schema) and body (rules)
parseMDTable input = 
    let rows = lines input
        headers = parseLine (head rows)
        body = map parseLine (drop 2 rows)
    in (headers, body)
    where 
        parseLine = map trim . filter (not . null) . splitOn "|"
        trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

parseDecisionOutput :: String -> DecOutVar
parseDecisionOutput header = 
    let (name, feelType) = parseHeader header
    in DecOutVar { sDecVarName = name
                , sDecVarFEELType = feelType }

parseHeader :: String -> (String, String)
parseHeader header = 
    let parts = splitOn "," header
        name = trim $ filter (/= '(') $ head $ splitOn "(" $ head parts
        feelType = if length parts > 1 then init (last parts) else "String"
    in (name, trim feelType)

parseInfoReqs :: [String] -> [InfoReq]
parseInfoReqs = map (ReqInputEl . fst . parseHeader)

parseInputSchemas :: [String] -> [InputSchema]
parseInputSchemas = map (\h -> let (name, feelType) = parseHeader h
                                     in InputSchema 
                                        { sInputSchemaId = name
                                        , inputExprFEELType = feelType
                                        })

parseOutputSchema :: String -> OutputSchema
parseOutputSchema header = 
    let (name, feelType) = parseHeader header
    in OutputSchema { sOutputSchemaVarName = name, sOutputSchemaFEELType = feelType }

parseRules :: [[String]] -> [InputSchema] -> [Rule]
parseRules rows inputSchemaNames = zipWith (parseRule inputSchemaNames) [1..] rows

parseRule :: [InputSchema] -> Int -> [String] -> Rule
parseRule inputSchemaNames i row = 
    Rule { ruleId = "rule" ++ show i
         , inputEntries = zipWith parseInputEntry inputSchemaNames (tail (init row))
         , outputEntry = parseOutputEntry (last row)
         }

parseInputEntry :: InputSchema -> String -> InputEntry
parseInputEntry schema entry = 
    InputEntry { sInputEntryId = sInputSchemaId schema
               , sMaybeCondition = parseCondition (inputExprFEELType schema) entry
               }

parseCondition :: String -> String -> Maybe Condition
parseCondition _ "-" = Nothing
parseCondition expectedType s
    | map toLower expectedType == "bool" = parseBooleanCondition s
    | map toLower expectedType == "string" = Just (ConditionString s)
    | map toLower expectedType == "number" = parseNumberCondition s
    | otherwise = error $ "Unsupported type: " ++ expectedType

parseBooleanCondition :: String -> Maybe Condition
parseBooleanCondition s
    | map toLower s == "true"  = Just (ConditionBool True)
    | map toLower s == "false" = Just (ConditionBool False)
    | otherwise = error $ "Invalid Boolean value: " ++ s

parseNumberCondition :: String -> Maybe Condition
parseNumberCondition s =
    let (op, numStr) = span (not . isDigit) s
    in Just (ConditionNumber (if null op then Nothing else Just op) (read numStr))

parseOutputEntry :: String -> OutputEntry
parseOutputEntry s = OutputEntry { sOutputId = "output", sExpr = s }

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
