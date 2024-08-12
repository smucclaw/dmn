
module FromMD where

import Types
import Data.List.Split (splitOn)

parseMDTable :: String -> ([String], [[String]]) -- produces a tuple of headers (schema) and body (rules)
parseMDTable input = 
    let rows = lines input
        headers = parseLine (head rows)
        body = map parseLine (drop 2 rows)
    in (headers, body)
    where 
        parseLine = map trim . filter (not . null) . splitOn "|"
        trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

parseDecisionTable :: String -> String -> Decision
parseDecisionTable name input = 
  let (headers, body) = parseMDTable input
  in Decision { decisionName = name
    , decisionOut = parseDecisionOutput (last headers)
    , decisionInfoReq = parseInfoReqs (init headers)
    , decisionLogic = DecTable 
        { decTableId = name
        , hitPolicy = head headers
        , schema = Schema 
            { sInputSchemas = parseInputSchemas (init headers)
            , sOutputSchema = parseOutputSchema (last headers) -- rn taking output as last column only, need to fix
            }
        , rules = parseRules body
    }
  }

parseDecisionOutput :: String -> DecOutVar
parseDecisionOutput header = 
    let (name, feelType) = parseHeader header
    in DecOutVar { sDecVarName = name
                , sDecVarFEELType = feelType }

parseHeader :: String -> (String, String)
parseHeader header = 
    let (namePart, typePart) = break (== ',') header
        feelType = case typePart of
            [] -> "String"
            _ -> tail typePart
    in (name, feelType)

parse