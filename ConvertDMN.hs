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

-- InputEntrytoExpr :: InputEntry { sInputEntryId :: Id, sMaybeCondition :: Maybe Condition }
-- e.g. `InputEntry a b = …`
        --  InputEntry c d = …`

convertDecision :: Decision -> CompiledRule
convertDecision Decision { decisionLogic = DecTable { rules = rules, schema = Schema { sInputSchemas = inputs } } } = 
    MkCompiledRule (map (\InputSchema {sInputSchemaId = id} -> Arg id) inputs) (nestedIfRules rules)

-- nested IF different rules 
nestedIfRules :: [Rule] -> Expr
nestedIfRules [] = error "No rules in decision table"
nestedIfRules [rule] = getOutputEntry $ outputEntry rule
nestedIfRules (rule:rules) = 
    case rules of
        [lastRule] -> If (combineOneRule rule)
                         (getOutputEntry $ outputEntry rule)
                         (getOutputEntry $ outputEntry lastRule)
        _ -> If (combineOneRule rule)
                (getOutputEntry $ outputEntry rule)
                (nestedIfRules rules)

-- AND all the conditions in a rule together
combineOneRule :: Rule -> Expr
combineOneRule Rule [entry] = 
    case checkCondition entry of
        Just expr -> expr  -- If only one condition and it's valid, return it directly - figure out an easier way take away the Just?
combineOneRule Rule { inputEntries = entries } = 
    case mapMaybe checkCondition entries of
        exprs -> And exprs  -- If multiple conditions, use And

-- this checks if there is a condition, and forms the Equal etc
checkCondition :: InputEntry -> Maybe Expr
checkCondition InputEntry {sMaybeCondition = Just (ConditionString val), ..} = -- string
    Just (Equal (Var (Arg sInputEntryId)) (Const (String val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionBool val), ..} = -- bool
    Just (Equal (Var (Arg sInputEntryId)) (Const (Bool val)))
checkCondition _ = Nothing

-- to get the "Then" of if/then/else
getOutputEntry :: OutputEntry -> Expr
getOutputEntry OutputEntry {sExpr = expr} = Const (String expr)


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

ruleProduced :: CompiledRule
ruleProduced = MkCompiledRule [Arg "stage",Arg "sector",Arg "stage_com",Arg "has_ESG",Arg "wants_ESG"] 
        (If 
            (And [Equal (Var (Arg "stage")) (Const (String "Seed"))
                ,Equal (Var (Arg "sector")) (Const (String "Information Technology"))
                ,Equal (Var (Arg "stage_com")) (Const (String "Pre-Revenue"))]) 
            (Const (String "Interesting")) 
            (If 
                (And [Equal (Var (Arg "stage")) (Const (String "Series A"))
                    ,Equal (Var (Arg "sector")) (Const (String "Information Technology"))
                    ,Equal (Var (Arg "stage_com")) (Const (String "Pre-Profit"))]) 
                (Const (String "Interesting")) 
                (If 
                    (And [Equal (Var (Arg "has_ESG")) (Const (Bool True))
                        ,Equal (Var (Arg "wants_ESG")) (Const (Bool True))]) 
                    (Const (String "Interesting")) 
                    (Const (String "reject")))))