-- TypeChecker.hs
module TypeChecking where

import Types
import Data.Maybe (isJust)
import Data.Char (isDigit)

typeCheck :: Decision -> Either String Decision
typeCheck decision = 
    let errors = concatMap (checkRule (schema (decisionLogic decision))) (rules (decisionLogic decision))
    in case errors of
        [] -> Right decision
        _  -> Left (unlines errors)

checkRule :: Schema -> Rule -> [String] -- check each rule
checkRule schema rule = 
    let inputErrors = zipWith checkInputEntry (sInputSchemas schema) (inputEntries rule)
        outputError = checkOutputEntry (sOutputSchema schema) (outputEntry rule)
    in filter (not . null) $ inputErrors ++ [outputError]

checkInputEntry :: InputSchema -> InputEntry -> String -- check individual entries
checkInputEntry schema entry = 
    case sMaybeCondition entry of
        Nothing -> ""
        Just condition -> case matchesType (inputExprFEELType schema) condition of
            True -> ""
            False -> "Type mismatch in rule for " ++ sInputSchemaId schema ++ 
                     ": expected " ++ inputExprFEELType schema ++ 
                     ", got " ++ show condition

checkOutputEntry :: OutputSchema -> OutputEntry -> String
checkOutputEntry schema entry =
    case sOutputSchemaFEELType schema == sOutputFEELType entry of
        True -> ""
        False -> "Type mismatch in output: expected " ++ sOutputSchemaFEELType schema ++ 
                 ", got " ++ sOutputFEELType entry

matchesType :: String -> Condition -> Bool
matchesType "string" (ConditionString _) = True
matchesType "bool" (ConditionBool _) = True
matchesType "int" (ConditionInt _ _) = True
matchesType "int" (ConditionRange _ _ _ _) = True
matchesType _ _ = False