
module FromMD where

import Types
import Data.List.Split (splitOn, splitWhen)
import Data.Char (toLower, isDigit, isSpace)
import Data.List (isInfixOf)

parseMDToDMN :: String -> DRD
parseMDToDMN markdown = 
    let sections = splitOn "\n\n" markdown
        (tables, entries) = separateTablesAndConnections sections
        decisions = map parseDecisionTable tables
        schemas = [(tableID (decisionLogic d), schema (decisionLogic d)) | d <- decisions] 
        entryList = parseEntries (unlines entries) schemas
    in (decisions, entryList)

separateTablesAndConnections :: [String] -> ([String], [String]) -- split depending on "|" or not
separateTablesAndConnections = foldr categorize ([], [])
  where
    categorize section (tables, connections) =
      case section of
        (firstChar:_) | firstChar == '|' -> (tables ++ [section], connections)
        _ -> (tables, connections ++ [section])

-- Main function to convert a singular markdown table to Decision
parseDecisionTable :: String -> Decision
parseDecisionTable input = 
  let (headers, body) = parseMDTable input
      (inputHeaders, outputHeaders) = separateHeaders (tail headers)
      inputSchemaNames = parseInputSchemas inputHeaders
      outputSchemaNames = parseOutputSchema outputHeaders
  in Decision { decisionOut = parseDecisionOutput (last headers) -- need to fix this
    , decisionInfoReq = parseInfoReqs (init (tail headers))
    , decisionLogic = DecTable 
        { tableID = takeWhile (/= ')') (drop 1 (dropWhile (/= '(') (head headers)))
        , hitPolicy = takeWhile (/= ')') (dropWhile isSpace (head headers))
        , schema = Schema 
            { sInputSchemas = inputSchemaNames
            , sOutputSchema = outputSchemaNames
            }
        , rules = parseRules body inputSchemaNames outputSchemaNames
    }
  }
  where 
    separateHeaders :: [String] -> ([String], [String])
    separateHeaders headers = span (isInfixOf "input") headers

parseMDTable :: String -> ([String], [[String]]) -- produces a tuple of headers (schema) and body (rules)
parseMDTable input = 
    let rows = lines input
        headers = parseLine (head rows)
        body = map parseLine (drop 2 rows)
    in (headers, body)
    where 
        parseLine line = init (tail (map trim (splitOn "|" line)))
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

parseOutputSchema :: [String] -> [OutputSchema]
parseOutputSchema headers = 
    map (\header -> let (name, feelType) = parseHeader header
                    in OutputSchema { sOutputSchemaVarName = name, sOutputSchemaFEELType = feelType }) headers

parseRules :: [[String]] -> [InputSchema] -> [OutputSchema] -> [Rule]
parseRules rows inputSchemaNames outputSchemaNames = zipWith (parseRule inputSchemaNames outputSchemaNames) [1..] rows

parseRule :: [InputSchema] -> [OutputSchema] -> Int -> [String] -> Rule
parseRule inputSchemaNames outputSchemaNames i row = 
    Rule { ruleId = "rule" ++ show i
         , inputEntries = zipWith parseInputEntry inputSchemaNames (take ((length inputSchemaNames) + 1) (tail row))
         , outputEntry = zipWith parseOutputEntry outputSchemaNames (drop ((length inputSchemaNames) + 1) row)
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
    | (head s == '[' || head s == '(') && (last s == ']' || last s == ')') = parseRangeCondition s
    | head s == '"' && last s == '"' = Just (ConditionString (init (tail s)))
    | otherwise = parseIntCondition s

parseIntCondition :: String -> Maybe Condition
parseIntCondition i =
    let (op, numStr) = span (not . isDigit) i
    in Just (ConditionInt (Just op) (read numStr))

parseRangeCondition :: String -> Maybe Condition
parseRangeCondition r = 
    let openBracket = [head r]
        innerPart = init (tail r)
        parts = splitOn ".." innerPart
        num1 = read (head parts)
        num2 = read (last parts)
        closeBracket = [last r]
    in Just (ConditionRange openBracket num1 num2 closeBracket)

parseOutputEntry :: OutputSchema -> String -> OutputEntry
parseOutputEntry schema entry = 
    OutputEntry { sOutputId = sOutputSchemaVarName schema, sExpr = trimmedEntry, sOutputFEELType = parseOutputType entry }
    where
        trimmedEntry = filter (/= '\"') entry

parseOutputType :: String -> String
parseOutputType s
    | all isDigit s = "Int"
    | map toLower s == "true" || map toLower s == "false" = "Bool"
    | head s == '"' && last s == '"' = "String"
    | (head s == '[' || head s == '(') && (last s == ']' || last s == ')') = "Int"
    | otherwise = "Error"

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- ie inputs and outputs that are manually entered in each table
parseEntries :: String -> [(Id, Schema)] -> [Entry]
parseEntries entries schemas = map (parseEntry schemas) (lines entries)

parseEntry :: [(Id, Schema)] -> String -> Entry
parseEntry schemas entry = 
    case splitOn "(" entry of
        [table, rest] -> 
            let params = map trim (splitOn "," (init rest)) -- remove the trailing ')'
                maybeSchema = lookup table schemas -- finds corresponding tableid
            in case maybeSchema of
                Just schema -> categorizeEntry table params schema
                Nothing -> error ("Error: Table " ++ table ++ " not yet declared")
        _ -> error ("Error: Invalid entry format: " ++ entry)
    where 
        categorizeEntry :: Id -> [String] -> Schema -> Entry
        categorizeEntry tableId params Schema{sInputSchemas=inputs, sOutputSchema=outputs} =
            let numInputs = length inputs
                numOutputs = length outputs
                (inputParams, outputParams) = splitAt numInputs params
            in Entry 
                { tableId = tableId
                , inputParams = inputParams
                , outputParams = outputParams
                }


parseSchema :: String -> [String] -> ([String], [String])
parseSchema table inout = 
    let [input, output] = splitOn "|" (last inout)
    in (splitOn "," input, splitOn "," output)