module TypeChecking where

import Types
import Data.Maybe (isJust, fromJust)
import Data.Char (isDigit, toLower)
import qualified Data.Map as Map

type VarMap = Map.Map String String

typeCheck :: DRD -> VarMap -> Either String DRD
typeCheck (decisions, entries) varMap = 
    let decisionErrors = map typeCheckDecision decisions
        entryErrors = map (typeCheckEntry decisions varMap) entries
    in case (concatMap (either (\e -> [e]) (const [])) decisionErrors) ++ (concatMap (either (\e -> [e]) (const [])) entryErrors) of
        [] -> Right (decisions, entries)
        errors -> Left (unlines errors)

typeCheckDecision :: Decision -> Either String Decision
typeCheckDecision decision = 
    let errors = concatMap (checkRule (schema (decisionLogic decision))) (rules (decisionLogic decision))
    in case errors of
        [] -> Right decision
        _  -> Left (unlines errors)

checkRule :: Schema -> Rule -> [String] -- check each rule
checkRule schema rule = 
    let inputErrors = zipWith checkInputEntry (sInputSchemas schema) (inputEntries rule)
        outputErrors = zipWith checkOutputEntry (sOutputSchema schema) (outputEntry rule)
    in filter (not . null) $ inputErrors ++ outputErrors

checkInputEntry :: InputSchema -> InputEntry -> String -- check individual "entries"
checkInputEntry schema entry = 
    case sMaybeCondition entry of
        Nothing -> ""
        Just condition -> case matchesTypeCondition (map toLower (inputExprFEELType schema)) condition of
            True -> ""
            False -> "Type mismatch in rule for " ++ sInputSchemaId schema ++ 
                     ": expected " ++ inputExprFEELType schema ++ 
                     ", got " ++ show condition

checkOutputEntry :: OutputSchema -> OutputEntry -> String
checkOutputEntry schema entry =
    case map toLower (sOutputSchemaFEELType schema) == map toLower (sOutputFEELType entry) of
        True -> ""
        False -> "Type mismatch in output: expected " ++ sOutputSchemaFEELType schema ++ 
                 ", got " ++ sOutputFEELType entry

matchesTypeCondition :: String -> Condition -> Bool
matchesTypeCondition "string" (ConditionString _) = True
matchesTypeCondition "bool" (ConditionBool _) = True
matchesTypeCondition "int" (ConditionInt _ _) = True
matchesTypeCondition "int" (ConditionRange _ _ _ _) = True
matchesTypeCondition _ _ = False

-- type checking for entries/calls
typeCheckEntry :: [Decision] -> VarMap -> Entry -> Either String Entry
typeCheckEntry decisions varMap entry = 
    let schema = findDecision decisions (tableId entry)
    in case isJust schema of
        False -> Left ("Decision " ++ tableId entry ++ " not found")
        True -> let errors = checkEntry varMap (fromJust schema) entry
                in case errors of
                    [] -> Right entry
                    _  -> Left (unlines errors)

checkEntry :: VarMap -> Schema -> Entry -> [String]
checkEntry varMap schema entry = 
    let inputErrors = zipWith (checkInputs varMap) (sInputSchemas schema) (inputParams entry)
    -- maybe add error checking for outputs ie variables?
    -- according to documentation:
    -- Variable names cannot start with a language keyword, such as and, true, or every. 
    -- The remaining characters in a variable name can be any of the starting characters, as well as digits, white spaces, and special characters such as +, -, /, *, ', and ..
    in filter (not . null) $ inputErrors

checkInputs :: VarMap -> InputSchema -> Param -> String
checkInputs varMap schema param = 
    let expectedType = map toLower (inputExprFEELType schema)
        actualType = map toLower (paramType param)
    in case actualType of
        "var" -> case Map.lookup (paramName param) varMap of
            Just varType -> if map toLower expectedType == map toLower varType
                            then ""
                            else "Type mismatch for variable " ++ paramName param ++ 
                                 ": expected " ++ expectedType ++ ", got " ++ (paramName param)
            Nothing -> "Variable " ++ paramName param ++ " not found in variable map"
        _ -> if expectedType == actualType
             then ""
             else "Type mismatch in input: expected " ++ expectedType ++ " for " ++ sInputSchemaId schema ++ ", got " ++ actualType ++ " for " ++ paramName param
             
-- need to implement type checking for variables
matchesType :: String -> String -> Bool
matchesType "string" _ = True
matchesType "bool" s = s == "true" || s == "false"
matchesType "int" s = all isDigit s
matchesType "int" s = (head s == '[' || head s == '(') && (last s == ']' || last s == ')')
matchesType _ _ = False

findDecision :: [Decision] -> Id -> Maybe Schema
findDecision decisions id = 
    let decision = filter (\d -> tableID (decisionLogic d) == id) decisions
    in case decision of
        [] -> Nothing
        (x:_) -> Just (schema (decisionLogic x))