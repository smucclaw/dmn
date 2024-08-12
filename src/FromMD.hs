
module FromMD where

import Types
import Data.List.Split (splitOn)
import Data.Char (toLower)

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
        name = filter (/= '(') $ head $ splitOn "(" $ head parts
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
parseCondition s
    | map toLower s == "true"  = Just (ConditionBool True)
    | map toLower s == "false" = Just (ConditionBool False)
    | otherwise = Just (ConditionString s)

-- currently does not check type

parseOutputEntry :: String -> OutputEntry
parseOutputEntry s = OutputEntry { sOutputId = "output", sExpr = s }

trim :: String -> String
trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

Decision 
    {decisionOut = DecOutVar 
        {sDecVarName = "opinion ", sDecVarFEELType = "string"}
        , decisionInfoReq = [ReqInputEl {sReqInput = "stage "}
                            ,ReqInputEl {sReqInput = "sector "}
                            ,ReqInputEl {sReqInput = "stage_com "}
                            ,ReqInputEl {sReqInput = "has_ESG "}
                            ,ReqInputEl {sReqInput = "wants_ESG "}]
        , decisionLogic = DecTable 
            {hitPolicy = "F"
            , schema = Schema 
                {sInputSchemas = [InputSchema {sInputSchemaId = "stage ", inputExprFEELType = "string"}
                                ,InputSchema {sInputSchemaId = "sector ", inputExprFEELType = "string"}
                                ,InputSchema {sInputSchemaId = "stage_com ", inputExprFEELType = "string"}
                                ,InputSchema {sInputSchemaId = "has_ESG ", inputExprFEELType = "bool"}
                                ,InputSchema {sInputSchemaId = "wants_ESG ", inputExprFEELType = "bool"}]
                , sOutputSchema = OutputSchema {sOutputSchemaVarName = "opinion ", sOutputSchemaFEELType = "string"}}
            , rules = [Rule {ruleId = "rule1"
                            , inputEntries = [InputEntry {sInputEntryId = "stage ", sMaybeCondition = Just (ConditionString "Seed")}
                                ,InputEntry {sInputEntryId = "sector ", sMaybeCondition = Just (ConditionString "Information Technology")}
                                ,InputEntry {sInputEntryId = "stage_com ", sMaybeCondition = Just (ConditionString "Pre-Revenue")}
                                ,InputEntry {sInputEntryId = "has_ESG ", sMaybeCondition = Nothing}
                                ,InputEntry {sInputEntryId = "wants_ESG ", sMaybeCondition = Nothing}]
                            , outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}}
                    ,Rule {ruleId = "rule2"
                            , inputEntries = [InputEntry {sInputEntryId = "stage ", sMaybeCondition = Just (ConditionString "Series A")}
                                ,InputEntry {sInputEntryId = "sector ", sMaybeCondition = Just (ConditionString "Information Technology")}
                                ,InputEntry {sInputEntryId = "stage_com ", sMaybeCondition = Just (ConditionString "Pre-Profit")}
                                ,InputEntry {sInputEntryId = "has_ESG ", sMaybeCondition = Nothing}
                                ,InputEntry {sInputEntryId = "wants_ESG ", sMaybeCondition = Nothing}]
                            , outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}}
                    ,Rule {ruleId = "rule3", inputEntries = [InputEntry {sInputEntryId = "stage ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "sector ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "stage_com ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "has_ESG ", sMaybeCondition = Just (ConditionBool True)},InputEntry {sInputEntryId = "wants_ESG ", sMaybeCondition = Just (ConditionBool True)}], outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}}
                    ,Rule {ruleId = "rule4", inputEntries = [InputEntry {sInputEntryId = "stage ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "sector ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "stage_com ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "has_ESG ", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "wants_ESG ", sMaybeCondition = Nothing}], outputEntry = OutputEntry {sOutputId = "output", sExpr = "reject"}}]}}