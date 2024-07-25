{-# LANGUAGE RecordWildCards #-}

module ConvertDMN where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Types

-- define IR
data CompiledRule = MkCompiledRule [Arg] Expr deriving Show

data Expr = Var Arg 
            | And [Expr] 
            | Or [Expr] 
            | Equal Expr Expr
            | If Expr Expr Expr
            | Const Val 
            deriving Show

data Val = Bool Bool 
            | Int Int 
            | String String
            deriving Show

data Arg = Arg String deriving Show

-- ok corrected conversion function using pattern matching etc
convertDecision :: Decision -> CompiledRule
convertDecision Decision { decisionLogic = DecTable { rules = rs, schema = Schema { sInputSchemas = inputs } } } = 
    MkCompiledRule (map (\InputSchema { sInputSchemaId = id } -> Arg id) inputs) (nestedIfRules rs)

-- nested IF different rules 
nestedIfRules :: [Rule] -> Expr
nestedIfRules [] = error "No rules in decision table"
nestedIfRules [rule] = combineOneRule rule
nestedIfRules (rule:rules) = 
    If (combineOneRule rule) 
       (getOutputEntry $ outputEntry rule) 
       (nestedIfRules rules)

-- AND all the conditions in a rule together
combineOneRule :: Rule -> Expr
combineOneRule Rule { inputEntries = [] } = 
    Const (Bool True)  -- If no input entries, always true
combineOneRule Rule { inputEntries = [entry] } = 
    case checkCondition entry of
        Just expr -> expr  -- If only one condition and it's valid, return it directly
        Nothing -> Const (Bool True)  -- If the single condition is invalid, default to true
combineOneRule Rule { inputEntries = entries } = 
    case mapMaybe checkCondition entries of
        [] -> Const (Bool True)  -- If no valid conditions, always true
        exprs -> And exprs  -- If multiple valid conditions, use And

-- this checks if there is a condition, and forms the Equal etc
checkCondition :: InputEntry -> Maybe Expr
checkCondition InputEntry { sInputEntryId = id, sMaybeCondition = Just (ConditionString val) } = 
    Just (Equal (Var (Arg id)) (Const (String val)))
checkCondition InputEntry { sInputEntryId = id, sMaybeCondition = Just (ConditionBool val) } = 
    Just (Equal (Var (Arg id)) (Const (Bool val)))
checkCondition _ = Nothing
-- e.g. `InputEntry a b = …`
        --  InputEntry c d = …`

getDefaultRule :: [Rule] -> Expr
getDefaultRule rules = 
    let rules = reverse rules
        (lastRule:otherRules) = rules
    in lastRule

getOutputEntry :: OutputEntry -> Expr
getOutputEntry OutputEntry {sExpr = expr} = Const (String expr)

-- conversion function this is wrong lmao
-- convertDecision :: Decision -> CompiledRule
-- convertDecision Decision{..} =
--     case decisionLogic of
--         DecTable{..} ->
--             let args = map (Arg . sInputSchemaId) (sInputSchemas $ schema)
--                 (defaultRule, otherRules) = extractDefaultRule rules
--                 nestedIfs = foldl' (flip combineRules) defaultRule otherRules
--             in MkCompiledRule args nestedIfs
--         _ -> error "Lit exp not yet supported"

-- extractDefaultRule :: [Rule] -> (Expr, [Rule])
-- extractDefaultRule rules =
--     case reverse rules of
--         [] -> error "No rules in decision table"
--         (lastRule:otherRules) ->
--             (convertOutputEntry (outputEntry lastRule), reverse otherRules)

-- combineRules :: Rule -> Expr -> Expr
-- combineRules Rule{..} elseExpr =
--     let conditions = catMaybes $ map inputEntryToConditionExpr inputEntries
--         outputExpr = convertOutputEntry outputEntry
--     in if null conditions
--        then outputExpr 
--        else If (And conditions) outputExpr elseExpr

-- inputEntryToConditionExpr :: InputEntry -> Maybe Expr
-- inputEntryToConditionExpr InputEntry{..} =
--     case sMaybeCondition of
--         Just (ConditionString val) -> Just $ Equal (Var (Arg sInputEntryId)) (Const (String val))
--         Just (ConditionBool val) -> Just $ Equal (Var (Arg sInputEntryId)) (Const (Bool val))
--         Nothing -> Nothing

-- convertOutputEntry :: OutputEntry -> Expr
-- convertOutputEntry OutputEntry{..} = Const (String sExpr)

-- example
rule1 :: CompiledRule -- target
rule1 = MkCompiledRule [Arg "stage", Arg "sector", Arg "stage_com", Arg "has_ESG", Arg "wants_ESG"] 
        (If 
            (And [ Equal (Var (Arg "stage")) (Const (String "Seed"))
                , Equal (Var (Arg "sector")) (Const (String "Information Technology"))
                , Equal (Var (Arg "stage_com")) (Const (String "Pre-Revenue"))
            ])
            (Const (String "interesting"))
            (If 
                (And [ Equal (Var (Arg "stage")) (Const (String "Series A"))
                    , Equal (Var (Arg "sector")) (Const (String "Information Technology"))
                    , Equal (Var (Arg "stage_com")) (Const (String "Pre-Profit"))
                ])
                (Const (String "interesting"))
                (If
                    (And [ Equal (Var (Arg "has_ESG")) (Const (Bool True))
                        , Equal (Var (Arg "wants_ESG")) (Const (Bool True))
                    ])
                    (Const (String "interesting"))
                    (Const (String "reject"))
                )
            )
        )
