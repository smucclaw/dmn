
module FromMD where

import Types
import Data.List.Split (splitOn)
import Data.Char (toLower, isDigit)

-- Main function to convert markdown table to Decision
parseMDToDMN :: String -> Decision
parseMDToDMN tableString = parseDecisionTable tableString

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
               , sMaybeCondition = parseCondition entry
               }

parseCondition :: String -> Maybe Condition
parseCondition "-" = Nothing
parseCondition "" = Nothing
parseCondition s
    | map toLower s == "true"  = Just (ConditionBool True)
    | map toLower s == "false" = Just (ConditionBool False)
    | all isDigit s = Just (ConditionInt Nothing (read s))
    | head s == '"' && last s == '"' = Just (ConditionString (init (tail s)))
    | otherwise = parseIntCondition s

parseIntCondition :: String -> Maybe Condition
parseIntCondition s =
    let (op, numStr) = span (not . isDigit) s
    in Just (ConditionInt (Just op) (read numStr))

parseOutputEntry :: String -> OutputEntry
parseOutputEntry s
    | head s == '"' && last s == '"' = OutputEntry { sOutputId = "output", sExpr = init (tail s) }
    | otherwise = error ("Invalid output entry: " ++ s)

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse
