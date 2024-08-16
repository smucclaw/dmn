{-# LANGUAGE RecordWildCards #-}

module Types where

type Id = String
type VarName = String  -- Assuming XMLText is just String for simplicity
type Label = String
type DecName = String  -- Decision Name

type DRD = [Decision]

data Definitions = Definitions
  { sXmlns :: String
  , sXmlnsDmndi :: String
  , sXmlnsDc :: String
  , sXmlnsModeler :: String
  , sXmlnsDi :: String
  , sDefId :: Id
  , sDefName :: String
  , sNamespace :: String
  , sExporter :: String
  , sExporterVersion :: String
  , sModelerExPlat :: String
  , sModelerExPlatVer :: String
  , sDecisions :: [Decision] }
  deriving Show

data DecOutVar = DecOutVar
  { sDecVarName :: DecName
  , sDecVarFEELType :: String  -- Adjusted to String for now
  } deriving Show

data Decision = Decision
  { decisionOut :: DecOutVar
  , decisionInfoReq :: [InfoReq]
  , decisionLogic :: DecLogic  -- This is DecTable or Literal Expression
  } deriving Show

data DecLogic = DecTable -- Add literal expression later
  { hitPolicy :: String
  -- , aggregation :: String -- add once collect is done
  , schema :: Schema
  , rules :: [Rule]
  } deriving Show

data Schema = Schema
  { sInputSchemas :: [InputSchema]
  , sOutputSchema :: OutputSchema
  } deriving Show

data InputSchema = InputSchema
  { sInputSchemaId :: Id
  , inputExprFEELType :: String }
  deriving Show

data OutputSchema = OutputSchema
  { sOutputSchemaVarName :: String
  , sOutputSchemaFEELType :: String } -- Change later
  deriving Show

data InfoReq = ReqInputEl
  { sReqInput :: ReqInput } -- get rid of one
  -- Id ReqInput
  deriving Show
type ReqInput = String

-- data InputExpr = InputExpr
--   { inputExprName :: VarName
--   , inputExprFEELType :: String  -- Change to FEEL type later on
--   } deriving Show
-- removed as it seems redundant

data Rule = Rule
  { ruleId :: Id
  , inputEntries :: [InputEntry]
  , outputEntry :: OutputEntry }
  deriving Show

data InputEntry = InputEntry
  { sInputEntryId :: Id
  , sMaybeCondition :: Maybe Condition }
  deriving Show

data OutputEntry = OutputEntry
  { sOutputId :: Id
  , sExpr :: String
  , sOutputFEELType :: String }
  deriving Show

data Condition = ConditionString String 
                | ConditionBool Bool
                | ConditionInt (Maybe String) Int
                | ConditionRange String Int Int String
  deriving Show

-- data Operator = Equal | NotEqual | LessThan | GreaterThan deriving Show

-- data hitPolicy = 

-- data Output = Output
--   { outputId :: Id
--   , outputExpression :: String
--   } deriving Show

exampleDecision :: Decision
exampleDecision = Decision
  { decisionOut = DecOutVar
    { sDecVarName = "opinion"
    , sDecVarFEELType = "String" }
  , decisionInfoReq = [ReqInputEl "stage"
    , ReqInputEl "sector" 
    , ReqInputEl "stage_com" 
    , ReqInputEl "has_ESG" 
    , ReqInputEl "wants_ESG"]
  , decisionLogic = DecTable
    { hitPolicy = "F"
    , schema = Schema
      { sInputSchemas = [InputSchema "stage" "String"
        , InputSchema "sector" "String"
        , InputSchema "stage_com" "String"
        , InputSchema "has_ESG" "Boolean"
        , InputSchema "wants_ESG" "Boolean"]
      , sOutputSchema = OutputSchema "opinion" "String" }
    , rules = [Rule "rule1" [InputEntry "stage" (Just (ConditionString "Seed"))
        , InputEntry "sector" (Just (ConditionString "Information Technology"))
        , InputEntry "stage_com" (Just (ConditionString "Pre-Revenue"))] 
        (OutputEntry "opinion" "Interesting" "String")
      , Rule "rule2" [InputEntry "stage" (Just (ConditionString "Series A"))
        , InputEntry "sector" (Just (ConditionString "Information Technology"))
        , InputEntry "stage_com" (Just (ConditionString "Pre-Profit"))] 
        (OutputEntry "opinion" "Interesting" "String")
      , Rule "rule3" [InputEntry "has_ESG" (Just (ConditionBool True))
        , InputEntry "wants_ESG" (Just (ConditionBool True))] 
        (OutputEntry "opinion" "Interesting" "String")
      , Rule "rule4" [InputEntry "input1" Nothing
        , InputEntry "sector" Nothing
        , InputEntry "stage_com" Nothing
        , InputEntry "has_ESG" Nothing
        , InputEntry "wants_ESG" Nothing] 
      (OutputEntry "opinion" "reject" "String")
      ]
    }
  }

exampleDecision2 :: Decision
exampleDecision2 = Decision
  { decisionOut = DecOutVar
    { sDecVarName = "result"
    , sDecVarFEELType = "String" }
  , decisionInfoReq = [ReqInputEl "season"]
  , decisionLogic = DecTable
    { hitPolicy = "U"
    , schema = Schema
      { sInputSchemas = [InputSchema "grade" "Int"]
      , sOutputSchema = OutputSchema "result" "String" }
    , rules = [Rule "rule1" [InputEntry "grade" (Just (ConditionInt (Just ">=") 50))] 
        (OutputEntry "result" "Pass" "String")
      , Rule "rule2" [InputEntry "grade" (Just (ConditionInt (Just "<") 50))] 
        (OutputEntry "result" "Fail" "String")
      ]
    }
  }

gradesDecision :: Decision
gradesDecision = Decision
  { decisionOut = DecOutVar "grade" "String"
  , decisionInfoReq = [ReqInputEl "mark"]
  , decisionLogic = DecTable
    { hitPolicy = "U"
    , schema = Schema
      { sInputSchemas = [InputSchema "mark" "Int"]
      , sOutputSchema = OutputSchema "grade" "String" }
    , rules = [Rule "rule1" [InputEntry "mark" (Just (ConditionInt (Just ">=") 70))] 
        (OutputEntry "grade" "A" "String")
      , Rule "rule2" [InputEntry "mark" (Just (ConditionRange "[" 60 70 ")"))] 
        (OutputEntry "grade" "B" "String")
      , Rule "rule3" [InputEntry "mark" (Just (ConditionRange "[" 50 60 ")"))]
        (OutputEntry "grade" "C" "String")
    ]}}


exampleDecision3 :: Decision
exampleDecision3 = Decision
  { decisionOut = DecOutVar
    { sDecVarName = "result"
    , sDecVarFEELType = "String" }
  , decisionInfoReq = [ReqInputEl "age"]
  , decisionLogic = DecTable
    { hitPolicy = "R"
    , schema = Schema
      { sInputSchemas = [InputSchema "age" "Int"]
      , sOutputSchema = OutputSchema "result" "String" }
    , rules = [Rule "rule1" [InputEntry "age" (Just (ConditionInt (Just ">=") 18))] 
        (OutputEntry "result" "cars" "String")
      , Rule "rule2" [InputEntry "age" (Just (ConditionInt (Just ">") 12))] 
        (OutputEntry "result" "videogames" "String") 
      , Rule "rule3" [InputEntry "age" Nothing] 
        (OutputEntry "result" "toys" "String")
      ]
    }
  }