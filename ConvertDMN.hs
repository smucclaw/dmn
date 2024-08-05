{-# LANGUAGE RecordWildCards #-}

module ConvertDMN where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Types

-- define IR
data CompiledRule = MkCompiledRule Func [Arg] [Expr] deriving Show

data Expr = Var Arg 
            | And [Expr] 
            | Or [Expr] 
            | Equal Expr Expr
            | If Expr Expr (Maybe Expr)
            | MoreThan Expr Expr
            | LessThan Expr Expr
            | MoreThanEqual Expr Expr
            | LessThanEqual Expr Expr
            | Const Val 
            | Return Val
            | InitList ListName -- list name, initialise
            | AppendList ListName Expr -- target list, what to add
            deriving Show

data Func = Func String deriving Show

data Val = Bool Bool 
            | String String
            | Number Int 
            deriving Show

data Arg = Arg String deriving Show

data ListName = ListName String deriving Show

-- InputEntrytoExpr :: InputEntry { sInputEntryId :: Id, sMaybeCondition :: Maybe Condition }
-- e.g. `InputEntry a b = …`
        --  InputEntry c d = …`

convertDecision :: Decision -> CompiledRule
convertDecision Decision { decisionLogic = DecTable { rules = rules
                                                    , schema = Schema { sInputSchemas = inputs }
                                                    , hitPolicy = policy
                                                    , decTableId = funcname } } = 
    MkCompiledRule (Func funcname)
        (map (\InputSchema {sInputSchemaId = id} -> Arg id) inputs) 
        (checkHitPolicy policy rules)

checkHitPolicy :: String -> [Rule] -> [Expr]
checkHitPolicy "U" rules = [nestedIfRules rules] -- unique
checkHitPolicy "F" rules = [nestedIfRules rules] -- first
checkHitPolicy "A" rules = [nestedIfRules rules] -- any
checkHitPolicy "R" rules = InitList (ListName "Results") : multipleHits rules -- rule order
checkHitPolicy _ rules = [nestedIfRules rules] -- default behavior for other hit policies

-- multiple hits - output in form of list ["a", "b", "c"]
multipleHits :: [Rule] -> [Expr]
multipleHits [] = error "No rules in decision table"
multipleHits rules = (map 
                (\rule -> If (combineOneRule rule) (AppendList (ListName "Results") 
                (getOutputEntry $ outputEntry rule)) Nothing) rules)


-- nested IF different rules 
nestedIfRules :: [Rule] -> Expr
nestedIfRules [] = error "No rules in decision table"
nestedIfRules [rule] = getOutputEntry $ outputEntry rule
nestedIfRules (rule:rules) = 
    case rules of
        [lastRule] -> If (combineOneRule rule)
                         (getOutputEntry $ outputEntry rule)
                         (Just (getOutputEntry $ outputEntry lastRule))
        _ -> If (combineOneRule rule)
                (getOutputEntry $ outputEntry rule)
                (Just (nestedIfRules rules))

-- AND all the conditions in a rule together
combineOneRule :: Rule -> Expr
combineOneRule Rule { inputEntries = [entry] } = 
    case checkCondition entry of
        Just expr -> expr  -- If only one condition and it's valid, return it directly
        Nothing -> Const (Bool True) -- should change it to a Maybe Expr so that 
combineOneRule Rule { inputEntries = entries } = 
    case mapMaybe checkCondition entries of
        [] -> Const (Bool True)
        exprs -> And exprs  -- If multiple conditions, use And

-- this checks if there is a condition, and forms the Equal etc
checkCondition :: InputEntry -> Maybe Expr
checkCondition InputEntry {sMaybeCondition = Just (ConditionString val), ..} = -- string
    Just (Equal (Var (Arg sInputEntryId)) (Const (String val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionBool val), ..} = -- bool
    Just (Equal (Var (Arg sInputEntryId)) (Const (Bool val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionNumber Nothing val), ..} = -- number with no operator, default to Equal
    Just (Equal (Var (Arg sInputEntryId)) (Const (Number val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionNumber (Just op) val), ..} = -- number with operator
    Just (chooseOperator sInputEntryId op (Const (Number val)))
checkCondition _ = Nothing

chooseOperator :: String -> String -> Expr -> Expr
chooseOperator id ">" val = MoreThan (Var (Arg id)) val
chooseOperator id "<" val = LessThan (Var (Arg id)) val
chooseOperator id ">=" val = MoreThanEqual (Var (Arg id)) val
chooseOperator id "<=" val = LessThanEqual (Var (Arg id)) val

-- to get the "Then" of if/then/else
getOutputEntry :: OutputEntry -> Expr
getOutputEntry OutputEntry {sExpr = expr} = Return (String expr)


-- example
rule1 :: CompiledRule -- target
rule1 = MkCompiledRule (Func "get_opinion") [Arg "stage", Arg "sector", Arg "stage_com", Arg "has_ESG", Arg "wants_ESG"] 
        [(If 
            (And [ Equal (Var (Arg "stage")) (Const (String "Seed"))
                , Equal (Var (Arg "sector")) (Const (String "Information Technology"))
                , Equal (Var (Arg "stage_com")) (Const (String "Pre-Revenue"))
            ])
            (Return (String "interesting"))
            (Just (If 
                (And [ Equal (Var (Arg "stage")) (Const (String "Series A"))
                    , Equal (Var (Arg "sector")) (Const (String "Information Technology"))
                    , Equal (Var (Arg "stage_com")) (Const (String "Pre-Profit"))
                ])
                (Return (String "interesting"))
                (Just (If
                    (And [ Equal (Var (Arg "has_ESG")) (Const (Bool True))
                        , Equal (Var (Arg "wants_ESG")) (Const (Bool True))
                    ])
                    (Return (String "interesting"))
                    (Just (Return (String "reject")))
                )
            )))
        )]

-- rule2 :: CompiledRule -- for rule order hit policy
-- rule2 = MkCompiledRule [Arg "age"] -- if i do InitList ListName [Expr] and keep MkCompiledRule to one expr- not sure which is better?
--         (InitList 
--             (ListName "What to advertise")
--             [ If 
--                 (MoreThan (Var (Arg "age")) (Const (Number 18)))
--                 (Const (String "cars"))
--                 (Nothing)
--             , If 
--                 (MoreThan (Var (Arg "age")) (Const (Number 12)))
--                 (Const (String "videogames"))
--                 (Nothing)
--             , Const (String "toys")
--             ]
--         )


-- rule3 :: CompiledRule -- alternative rule order hit policy
-- rule3 = MkCompiledRule [Arg "age"] 
--         [InitList (ListName "What to advertise")
--         , (If 
--             (MoreThan (Var (Arg "age")) (Const (Number 18)))
--             (AppendList 
--                 (ListName "What to advertise")
--                 (Const (String "cars"))
--             )
--             (Nothing)
--         )
--         , (If 
--             (MoreThan (Var (Arg "age")) (Const (Number 12)))
--             (AppendList 
--                 (ListName "What to advertise")
--                 (Const (String "videogames"))
--             )
--             (Nothing)
--         )
--         , (AppendList 
--             (ListName "What to advertise")
--             (Const (String "toys"))
--         )]

-- rulemade :: CompiledRule -- running exampleDecision3 produces this
-- rulemade = MkCompiledRule [Arg "age"] 
--             [InitList (ListName "Results")
--             ,If 
--                 (MoreThanEqual (Var (Arg "age")) (Const (Number 18))) 
--                 (AppendList 
--                     (ListName "Results") 
--                     (Const (String "cars"))) 
--                 Nothing
--             ,If 
--                 (MoreThan (Var (Arg "age")) (Const (Number 12))) 
--                 (AppendList 
--                     (ListName "Results") 
--                     (Const (String "videogames"))) 
--                 Nothing
--             ,If 
--                 (Const (Bool True)) -- need to get rid of this
--                 (AppendList (ListName "Results") (Const (String "toys")))
--                 Nothing
--             ]