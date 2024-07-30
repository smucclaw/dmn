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
  { sDecVarId :: Id
  , sDecVarName :: DecName
  , sDecVarFEELType :: String  -- Adjusted to String for now
  } deriving Show

data Decision = Decision
  { decisionID :: Id
  , decisionName :: DecName
  , decisionOut :: DecOutVar
  , decisionInfoReq :: [InfoReq]
  , decisionLogic :: DecLogic  -- This is DecTable or Literal Expression
  } deriving Show

data DecLogic = DecTable -- Add literal expression later
  { decTableId :: Id
  , hitPolicy :: String
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
  , sInputLabel :: Maybe Label
  , sInputExprEl :: InputExpr }
  deriving Show

data OutputSchema = OutputSchema
  { sOutputSchemaId :: Id
  , sOutputLabel :: Maybe Label
  , sOutputSchemaVarName :: String
  , sOutputSchemaFEELType :: String } -- Change later
  deriving Show

data InfoReq = ReqInputEl
  { sReqInputId :: Id
  , sReqInput :: ReqInput }
  -- Id ReqInput
  deriving Show
type ReqInput = String

data InputExpr = InputExpr
  { inputId :: Id
  , inputExprFEELType :: String  -- Change to FEEL type later on
  , inputExprName :: VarName
  } deriving Show

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
  , sExpr :: String }
  deriving Show

data Condition = ConditionString String 
                | ConditionBool Bool
                | ConditionNumber (Maybe String) Int
  deriving Show

-- data Operator = Equal | NotEqual | LessThan | GreaterThan deriving Show

-- data hitPolicy = 

data Output = Output
  { outputId :: Id
  , outputExpression :: String
  } deriving Show

exampleDecision :: Decision
exampleDecision = Decision
  { decisionID = "decision1"
  , decisionName = "Pitch Decks"
  , decisionOut = DecOutVar
    { sDecVarId = "output1"
    , sDecVarName = "opinion"
    , sDecVarFEELType = "String" }
  , decisionInfoReq = [ReqInputEl "stage" "stage"
    , ReqInputEl "sector" "sector"
    , ReqInputEl "stage_com" "stage_com"
    , ReqInputEl "has_ESG" "has_ESG"
    , ReqInputEl "wants_ESG" "wants_ESG"]
  , decisionLogic = DecTable
    { decTableId = "table1"
    , hitPolicy = "F"
    , schema = Schema
      { sInputSchemas = [InputSchema "stage" (Just "Stage") (InputExpr "stage" "String" "stage")
        , InputSchema "sector" (Just "Sector") (InputExpr "sector" "String" "sector")
        , InputSchema "stage_com" (Just "Stage_Com") (InputExpr "stage_com" "String" "stage_com")
        , InputSchema "has_ESG" (Just "Has_ESG") (InputExpr "has_ESG" "Boolean" "has_ESG")
        , InputSchema "wants_ESG" (Just "Wants_ESG") (InputExpr "wants_ESG" "Boolean" "wants_ESG")]
      , sOutputSchema = OutputSchema "opinion" (Just "Opinion") "opinion" "String" }
    , rules = [Rule "rule1" [InputEntry "stage" (Just (ConditionString "Seed"))
        , InputEntry "sector" (Just (ConditionString "Information Technology"))
        , InputEntry "stage_com" (Just (ConditionString "Pre-Revenue"))] 
        (OutputEntry "opinion" "Interesting")
      , Rule "rule2" [InputEntry "stage" (Just (ConditionString "Series A"))
        , InputEntry "sector" (Just (ConditionString "Information Technology"))
        , InputEntry "stage_com" (Just (ConditionString "Pre-Profit"))] 
        (OutputEntry "opinion" "Interesting")
      , Rule "rule3" [InputEntry "has_ESG" (Just (ConditionBool True))
        , InputEntry "wants_ESG" (Just (ConditionBool True))] 
        (OutputEntry "opinion" "Interesting")
      , Rule "rule4" [InputEntry "input1" Nothing
        , InputEntry "sector" Nothing
        , InputEntry "stage_com" Nothing
        , InputEntry "has_ESG" Nothing
        , InputEntry "wants_ESG" Nothing] 
      (OutputEntry "opinion" "reject")
      ]
    }
  }

exampleDecision2 :: Decision
exampleDecision2 = Decision
  {
    decisionID = "simple"
  , decisionName = "Simple Decision"
  , decisionOut = DecOutVar
    { sDecVarId = "result"
    , sDecVarName = "result"
    , sDecVarFEELType = "String" }
  , decisionInfoReq = [ReqInputEl "input1" "season"]
  , decisionLogic = DecTable
    { decTableId = "table1"
    , hitPolicy = "U"
    , schema = Schema
      { sInputSchemas = [InputSchema "grade" (Just "Grade") (InputExpr "grade" "Number" "grade")]
      , sOutputSchema = OutputSchema "result" (Just "Result") "result" "String" }
    , rules = [Rule "rule1" [InputEntry "grade" (Just (ConditionNumber (Just ">=") 50))] 
        (OutputEntry "result" "Pass")
      , Rule "rule2" [InputEntry "grade" (Just (ConditionNumber (Just "<") 50))] 
        (OutputEntry "result" "Fail")
      ]
    }
  }

exampleDecision3 :: Decision
exampleDecision3 = Decision
  {
    decisionID = "rule order"
    , decisionName = "Rule Order"
    , decisionOut = DecOutVar
    { sDecVarId = "result"
    , sDecVarName = "result"
    , sDecVarFEELType = "String" }
  , decisionInfoReq = [ReqInputEl "age" "age"]
  , decisionLogic = DecTable
    { decTableId = "table1"
    , hitPolicy = "R"
    , schema = Schema
      { sInputSchemas = [InputSchema "age" (Just "Age") (InputExpr "age" "Number" "age")]
      , sOutputSchema = OutputSchema "result" (Just "Result") "result" "String" }
    , rules = [Rule "rule1" [InputEntry "age" (Just (ConditionNumber (Just ">=") 18))] 
        (OutputEntry "result" "cars")
      , Rule "rule2" [InputEntry "age" (Just (ConditionNumber (Just ">") 12))] 
        (OutputEntry "result" "videogames")
      , Rule "rule3" [InputEntry "age" Nothing] 
        (OutputEntry "result" "toys")
      ]
    }
  }