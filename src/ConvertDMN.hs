{-# LANGUAGE RecordWildCards #-}

module ConvertDMN where

import Data.List (foldl')
import Data.Maybe (mapMaybe, isJust)
import Types
import Data.Char (isDigit, toLower)
import Text.Read (readMaybe)
import qualified Data.Map as Map

-- define IR
data CompiledDRD = 
    DRD [CompiledRule] [Call] deriving Show

data CompiledRule = 
    MkCompiledRule TableSignature [Expr] deriving Show

data TableSignature =
    MkTableSignature Func [ColumnSignature] [ColumnSignature] deriving Show -- inputs and then outputs

data ColumnSignature = 
    MkColumnSignature Arg Type deriving Show

data Type = 
    StringType
    | NumType
    | BoolType
    deriving Show

data Call = 
    MkCall Func [Argument] [Argument] deriving Show -- func name, inputs, outputs

data Argument = 
    ValArgument Val 
    | VarArgument Arg deriving Show -- outputs should always be variables

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
            | AppendList ListName [Val] -- target list, what to add
            deriving Show

data Func = Func String deriving Show

data Bracket = Inclusive Expr | Exclusive Expr deriving Show

data Val = Bool Bool 
            | String String
            | Int Int
            | Double Double
            | List ListName
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
    "number" -> NumType
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

convertArgument :: Param -> Argument
convertArgument (Param name _) 
                | map toLower name == "true"  = ValArgument (Bool True)
                | map toLower name == "false" = ValArgument (Bool False)
                | all isDigit name = ValArgument (Int (read name))
                | isJust (readMaybe name :: Maybe Double) = ValArgument (Double (read name))
                | head name == '"' && last name == '"' = ValArgument (String (init (tail name)))
                | otherwise = VarArgument (Arg name)

-- for rules
convertDecision :: Decision -> TableSignature -> CompiledRule
convertDecision Decision { decisionLogic = DecTable { tableID = tableid
                                                        , rules = rules
                                                        , schema = Schema { sInputSchemas = inputs }
                                                        , hitPolicy = policy } } table = 
    MkCompiledRule table
        -- (map (\InputSchema {sInputSchemaId = id} -> Arg id) inputs) 
        (checkHitPolicy policy rules)

checkHitPolicy :: String -> [Rule] -> [Expr]
checkHitPolicy "U" rules = [nestedIfRules rules] -- unique
checkHitPolicy "F" rules = [nestedIfRules rules] -- first
checkHitPolicy "A" rules = [nestedIfRules rules] -- any
-- checkHitPolicy "R" rules = InitList (ListName "Results") : multipleHits rules -- rule order
checkHitPolicy "C" rules = InitList (ListName "Results") : (multipleHits rules ++ [Return [List (ListName "Results")]]) -- collect
checkHitPolicy _ rules = [nestedIfRules rules] -- default behavior for other hit policies

-- multiple hits - output in form of list ["a", "b", "c"]
multipleHits :: [Rule] -> [Expr]
multipleHits [] = error "No rules in decision table"
multipleHits rules = (map 
                (\rule -> If 
                    (combineOneRule rule) 
                    (AppendList (ListName "Results") (map getOutputEntry $ outputEntry rule)) 
                    Nothing) 
                rules)


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
        Just expr -> expr 
        Nothing -> Const (Bool True) 
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
checkCondition InputEntry {sMaybeCondition = Just (ConditionNumber Nothing (Left val)), ..} = -- number with no operator, default to Equal (Int)
    Just (Equal (Var (Arg sInputEntryId)) (Const (Int val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionNumber Nothing (Right val)), ..} = -- number with no operator, default to Equal (Double)
    Just (Equal (Var (Arg sInputEntryId)) (Const (Double val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionNumber (Just op) (Left val)), ..} = -- number with operator (Int)
    Just (chooseOperator sInputEntryId op (Const (Int val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionNumber (Just op) (Right val)), ..} = -- number with operator (Double)
    Just (chooseOperator sInputEntryId op (Const (Double val)))
checkCondition InputEntry {sMaybeCondition = Just (ConditionRange open num1 num2 close), ..} = -- range condition
    let startBracket = bracket (head open) (convertToExpr num1)
        endBracket = bracket (head close) (convertToExpr num2)
    in Just (Range startBracket endBracket (Var (Arg sInputEntryId)))
checkCondition _ = Nothing

-- Function to convert a string to either Int or Double
convertStringToNumber :: String -> Either Int Double
convertStringToNumber s =
    case readMaybe s :: Maybe Int of
        Just intVal -> Left intVal
        Nothing -> case readMaybe s :: Maybe Double of
            Just doubleVal -> Right doubleVal
            Nothing -> error "Invalid number format"

convertToExpr :: String -> Expr
convertToExpr s = 
    case convertStringToNumber s of
        Left intVal -> Const (Int intVal)
        Right doubleVal -> Const (Double doubleVal)

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
        "Number" -> case readMaybe expr :: Maybe Int of
                      Just intVal -> Int intVal
                      Nothing -> case readMaybe expr :: Maybe Double of
                                   Just doubleVal -> Double doubleVal
                                   Nothing -> error "Invalid number format"
        "Bool" -> case map toLower expr of
                    "true" -> Bool True
                    "false" -> Bool False
        _ -> String expr

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