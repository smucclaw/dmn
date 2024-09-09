{-# LANGUAGE RecordWildCards #-}

module ConvertDMN where

import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Types
import Data.Char (isDigit, toLower)
import qualified Data.Map as Map

-- define IR
data CompiledDRD = 
    DRD [CompiledRule] [Call] deriving Show

-- data CompiledRule = 
--     MkCompiledRule Func [Arg] [Expr] deriving Show

data CompiledRule = 
    MkCompiledRule TableSignature [Arg] [Expr] deriving Show
-- TODO: fix this so that i no longer need [arg] since its inside tablesignature

data Call = 
    MkCall Func [Argument] [Argument] deriving Show -- func name, inputs, outputs

data Argument = 
    ValArgument Val 
    | VarArgument Arg deriving Show -- outputs should always be variables

data TableSignature =
    MkTableSignature Func [ColumnSignature] [ColumnSignature] deriving Show -- inputs and then outputs

data ColumnSignature = 
    MkColumnSignature Arg Type deriving Show

data Type = 
    StringType
    | IntType
    | BoolType
    deriving Show

type Vars = Map.Map String Val

data Expr = Var Arg 
            | And [Expr] 
            | Or [Expr] 
            | Equal Expr Expr
            | If Expr Expr (Maybe Expr)
            | MoreThan Expr Expr
            | LessThan Expr Expr
            | MoreThanEqual Expr Expr
            | LessThanEqual Expr Expr
            | Range Bracket Bracket Expr
            | Const Val 
            | Return [Val]
            | InitList ListName -- list name, initialise
            | AppendList ListName Expr -- target list, what to add
            deriving Show

data Func = Func String deriving Show

data Bracket = Inclusive Expr | Exclusive Expr deriving Show

data Val = Bool Bool 
            | String String
            | Int Int 
            deriving Show

data Arg = Arg String deriving Show

data ListName = ListName String deriving Show


convertDRD :: DRD -> CompiledDRD
convertDRD (decisions, entries) = 
    let compiledTables = map convertTableSignature decisions
        -- vars = Map.empty :: Vars
        -- calls = map (convertEntry compiledRules vars) entries
        compiledDecisions = zipWith convertDecision decisions compiledTables
        compiledCalls = map (\entry -> findTable compiledTables entry) entries
    in DRD compiledDecisions compiledCalls

-- for tables
convertTableSignature :: Decision -> TableSignature
convertTableSignature Decision {decisionLogic = DecTable{tableID = id, schema = Schema {..}}} = 
    MkTableSignature (Func id) (map convertInputSchema sInputSchemas) (map convertOutputSchema sOutputSchema)

convertInputSchema :: InputSchema -> ColumnSignature
convertInputSchema InputSchema {..} = MkColumnSignature (Arg sInputSchemaId) (convertType inputExprFEELType)

convertOutputSchema :: OutputSchema -> ColumnSignature
convertOutputSchema OutputSchema {..} = MkColumnSignature (Arg sOutputSchemaVarName) (convertType sOutputSchemaFEELType)

convertType :: String -> Type
convertType str = case map toLower str of
    "string" -> StringType
    "int" -> IntType
    "bool" -> BoolType
    _ -> error "Type not supported"

-- for calls
findTable :: [TableSignature] -> Entry -> Call
findTable tables Entry {..} =
    case filter (\(MkTableSignature (Func funcId) _ _) -> funcId == tableId) tables of
        [table] -> convertCall table Entry {..}
        _ -> error "Table not found"

convertCall :: TableSignature -> Entry -> Call
convertCall (MkTableSignature func inputcolumns outputcolumns) (Entry id inputs outputs) = 
    MkCall func (map convertArgument inputs) (map convertArgument outputs)

convertArgument :: String -> Argument
convertArgument param 
                | map toLower param == "true"  = ValArgument (Bool True)
                | map toLower param == "false" = ValArgument (Bool False)
                | all isDigit param = ValArgument (Int (read param))
                | head param == '"' && last param == '"' = ValArgument (String (init (tail param)))
                | otherwise = VarArgument (Arg param)

-- for rules
convertDecision :: Decision -> TableSignature -> CompiledRule
convertDecision Decision { decisionLogic = DecTable { tableID = tableid
                                                        , rules = rules
                                                        , schema = Schema { sInputSchemas = inputs }
                                                        , hitPolicy = policy } } table = 
    MkCompiledRule table
        (map (\InputSchema {sInputSchemaId = id} -> Arg id) inputs) 
        (checkHitPolicy policy rules)

checkHitPolicy :: String -> [Rule] -> [Expr]
checkHitPolicy "U" rules = [nestedIfRules rules] -- unique
checkHitPolicy "F" rules = [nestedIfRules rules] -- first
checkHitPolicy "A" rules = [nestedIfRules rules] -- any
-- checkHitPolicy "R" rules = InitList (ListName "Results") : multipleHits rules -- rule order
checkHitPolicy _ rules = [nestedIfRules rules] -- default behavior for other hit policies

-- multiple hits - output in form of list ["a", "b", "c"]
-- multipleHits :: [Rule] -> [Expr]
-- multipleHits [] = error "No rules in decision table"
-- multipleHits rules = (map 
--                 (\rule -> If (combineOneRule rule) (AppendList (ListName "Results") 
--                 (getOutputEntry $ outputEntry rule)) Nothing) rules)


-- nested IF different rules 
nestedIfRules :: [Rule] -> Expr
nestedIfRules [] = error "No rules in decision table"
nestedIfRules [rule] = getOutputEntries (outputEntry rule)
nestedIfRules (rule:rules) = 
    case rules of
        [lastRule] -> If (combineOneRule rule)
                         (getOutputEntries $ outputEntry rule)
                         (Just (getOutputEntries $ outputEntry lastRule))
        _ -> If (combineOneRule rule)
                (getOutputEntries $ outputEntry rule)
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
checkCondition InputEntry {sMaybeCondition = Just (ConditionInt Nothing val), ..} = -- number with no operator, default to Equal
    Just (Equal (Var (Arg sInputEntryId)) (Const (Int val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionInt (Just op) val), ..} = -- number with operator
    Just (chooseOperator sInputEntryId op (Const (Int val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionRange open num1 num2 close), ..} = -- range condition
    let startBracket = bracket (head open) (Const (Int num1))
        endBracket = bracket (head close) (Const (Int num2))
    in Just (Range startBracket endBracket (Var (Arg sInputEntryId)))
checkCondition _ = Nothing

bracket :: Char -> Expr -> Bracket
bracket '[' expr = Inclusive expr
bracket ']' expr = Inclusive expr
bracket '(' expr = Exclusive expr
bracket ')' expr = Exclusive expr

chooseOperator :: String -> String -> Expr -> Expr
chooseOperator id ">" val = MoreThan (Var (Arg id)) val
chooseOperator id "<" val = LessThan (Var (Arg id)) val
chooseOperator id ">=" val = MoreThanEqual (Var (Arg id)) val
chooseOperator id "<=" val = LessThanEqual (Var (Arg id)) val

-- to get the "Then" of if/then/else
getOutputEntries :: [OutputEntry] -> Expr
getOutputEntries outputs = Return (map getOutputEntry outputs)

getOutputEntry :: OutputEntry -> Val
getOutputEntry OutputEntry {sExpr = expr, sOutputFEELType = feelType} = 
    case feelType of
        "String" -> String expr
        "Int" -> Int (read expr)
        "Bool" -> case map toLower expr of
                    "true" -> Bool True
                    "false" -> Bool False
        _ -> String expr


-- convertEntry :: [CompiledRule] -> Vars -> Entry -> ProcessedRule
-- convertEntry compiledRules vars (Entry id inputs outputs) = 
--     case find (\(MkCompiledRule (Func funcId) _ _) -> funcId == id) compiledRules of
--         Just (MkCompiledRule _ args _) -> 
--             let updatedOutputVars = foldr (\(varName, value) acc -> Map.insert varName value acc) outputVars outputs 
-- -- slightly confused, not sure if i should be inserting the values into compiled rule and then putting here? or if i just store the variable name for now
--             in ProcessedRule (Func id) ((zipWith (convertInput updatedOutputVars) args inputs) ++ convertOutputs updatedOutputVars outputs)
--             -- ProcessedRule (Func id) ((zipWith convertInputs args inputs) ++ convertOutputs outputs)
--         Nothing -> error "Table not found for entry" ++ id
--     where 
--         convertOutputs vars = map (\param -> Output (Var (Arg param))) -- output is always a variable

-- convertInput :: Vars -> Arg -> String -> Parameter
-- convertInput outputVars arg param =
--     case param of
--         '"':rest -> Input arg (Const (String (init rest)))
--         "true"   -> Input arg (Const (Bool True))
--         "false"  -> Input arg (Const (Bool False))
--         num | all isDigit num -> Input arg (Const (Int (read num)))
--         var      -> if Map.member var outputVars
--                     then Input arg (Var (Arg var))
--                     else error ("Input variable " ++ var ++ " does not correspond to any output variable")

-- exampleDRD :: CompiledDRD
-- exampleDRD = DRD [rule1] [call1, call2]

-- -- example
-- rule1 :: CompiledRule -- target
-- rule1 = MkCompiledRule (Func "get_opinion") [Arg "stage", Arg "sector", Arg "stage_com", Arg "has_ESG", Arg "wants_ESG"] 
--         [(If 
--             (And [ Equal (Var (Arg "stage")) (Const (String "Seed"))
--                 , Equal (Var (Arg "sector")) (Const (String "Information Technology"))
--                 , Equal (Var (Arg "stage_com")) (Const (String "Pre-Revenue"))
--             ])
--             (Return [String "interesting"])
--             (Just (If 
--                 (And [ Equal (Var (Arg "stage")) (Const (String "Series A"))
--                     , Equal (Var (Arg "sector")) (Const (String "Information Technology"))
--                     , Equal (Var (Arg "stage_com")) (Const (String "Pre-Profit"))
--                 ])
--                 (Return [String "interesting"])
--                 (Just (If
--                     (And [ Equal (Var (Arg "has_ESG")) (Const (Bool True))
--                         , Equal (Var (Arg "wants_ESG")) (Const (Bool True))
--                     ])
--                     (Return [String "interesting"])
--                     (Just (Return [String "reject"]))
--                 )
--             )))
--         )]

-- rule2 :: CompiledRule
-- rule2 = MkCompiledRule ((Func "simple")) [Arg "opinion"]
--         [If (Equal (Var (Arg "opinion")) (Const (String "interesting")))
--             (Return [String "good"])
--             (Just (Return [String "bad"]))]

-- call1 :: Call
-- call1 = 
--     MkCall (Func "get_opinion") 
--     [ValArgument (String "Seed"), ValArgument (String "Information Technology"), ValArgument (String "Pre-Revenue"), ValArgument (Bool True), ValArgument (Bool True)] [VarArgument (Arg "opinion")]

-- call2 :: Call
-- call2 = MkCall (Func "simple") [VarArgument (Arg "opinion")] [VarArgument (Arg "result")]

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