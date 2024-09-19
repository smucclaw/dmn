{-# LANGUAGE RecordWildCards #-}

module Types where

type Id = String
type VarName = String  -- Assuming XMLText is just String for simplicity
type Label = String
type DecName = String  -- Decision Name

type DRD = ([Decision], [Entry])

data Entry = Entry
  { tableId :: Id
  , inputParams :: [Param]
  , outputParams :: [Param]
  } deriving Show

data Param = Param
  { paramName :: Id
  , paramType :: String
  } deriving Show

-- data Definitions = Definitions
--   { sXmlns :: String
--   , sXmlnsDmndi :: String
--   , sXmlnsDc :: String
--   , sXmlnsModeler :: String
--   , sXmlnsDi :: String
--   , sDefId :: Id
--   , sDefName :: String
--   , sNamespace :: String
--   , sExporter :: String
--   , sExporterVersion :: String
--   , sModelerExPlat :: String
--   , sModelerExPlatVer :: String
--   , sDecisions :: [Decision] }
--   deriving Show

data Decision = Decision
  { decisionLogic :: DecLogic  -- This is DecTable or Literal Expression
  } deriving Show

data DecLogic = DecTable -- TODO: Add literal expression
  { tableID :: Id
  , hitPolicy :: String
  -- , aggregation :: String -- add once collect is done
  , schema :: Schema
  , rules :: [Rule]
  } deriving Show

data Schema = Schema
  { sInputSchemas :: [InputSchema]
  , sOutputSchema :: [OutputSchema]
  } deriving Show

data InputSchema = InputSchema
  { sInputSchemaId :: Id
  , inputExprFEELType :: String }
  deriving Show

data OutputSchema = OutputSchema
  { sOutputSchemaVarName :: String
  , sOutputSchemaFEELType :: String } 
  deriving Show

data InfoReq = ReqInputEl
  { sReqInput :: ReqInput }
  deriving Show
type ReqInput = String

data Rule = Rule
  { ruleId :: Id
  , inputEntries :: [InputEntry]
  , outputEntry :: [OutputEntry] }
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
                | ConditionNumber (Maybe String) (Either Int Double)
                | ConditionRange String Int Int String
                -- | ConditionDecimal (Maybe String) Double
  deriving Show

-- exampleDRD :: DRD
-- exampleDRD = ([exampleDecision, exampleDecision2]
--   , [Entry "table1" ["stage", "sector", "stage_com", "has_ESG", "wants_ESG"] ["opinion"]
--   , Entry "table2" ["grade"] ["result"]])

-- exampleDecision :: Decision
-- exampleDecision = Decision
--   { decisionLogic = DecTable
--     { tableID = "table1"
--     , hitPolicy = "F"
--     , schema = Schema
--       { sInputSchemas = [InputSchema "stage" "String"
--         , InputSchema "sector" "String"
--         , InputSchema "stage_com" "String"
--         , InputSchema "has_ESG" "Boolean"
--         , InputSchema "wants_ESG" "Boolean"]
--       , sOutputSchema = [OutputSchema "opinion" "String"] }
--     , rules = [Rule "rule1" [InputEntry "stage" (Just (ConditionString "Seed"))
--         , InputEntry "sector" (Just (ConditionString "Information Technology"))
--         , InputEntry "stage_com" (Just (ConditionString "Pre-Revenue"))] 
--         [OutputEntry "opinion" "Interesting" "String"]
--       , Rule "rule2" [InputEntry "stage" (Just (ConditionString "Series A"))
--         , InputEntry "sector" (Just (ConditionString "Information Technology"))
--         , InputEntry "stage_com" (Just (ConditionString "Pre-Profit"))] 
--         [OutputEntry "opinion" "Interesting" "String"]
--       , Rule "rule3" [InputEntry "has_ESG" (Just (ConditionBool True))
--         , InputEntry "wants_ESG" (Just (ConditionBool True))] 
--         [OutputEntry "opinion" "Interesting" "String"]
--       , Rule "rule4" [InputEntry "input1" Nothing
--         , InputEntry "sector" Nothing
--         , InputEntry "stage_com" Nothing
--         , InputEntry "has_ESG" Nothing
--         , InputEntry "wants_ESG" Nothing] 
--       [OutputEntry "opinion" "reject" "String"]
--       ]
--     }
--   }

-- exampleDecision2 :: Decision
-- exampleDecision2 = Decision
--   { decisionLogic = DecTable
--     { tableID = "table2"
--     , hitPolicy = "U"
--     , schema = Schema
--       { sInputSchemas = [InputSchema "grade" "Int"]
--       , sOutputSchema = [OutputSchema "result" "String"] }
--     , rules = [Rule "rule1" [InputEntry "grade" (Just (ConditionInt (Just ">=") 50))] 
--         [OutputEntry "result" "Pass" "String"]
--       , Rule "rule2" [InputEntry "grade" (Just (ConditionInt (Just "<") 50))] 
--         [OutputEntry "result" "Fail" "String"]
--       ]
--     }
--   }

-- exampleDecision3 :: Decision
-- exampleDecision3 = Decision
--   { decisionOut = DecOutVar
--     { sDecVarName = "result"
--     , sDecVarFEELType = "String" }
--   , decisionInfoReq = [ReqInputEl "age"]
--   , decisionLogic = DecTable
--     { hitPolicy = "R"
--     , schema = Schema
--       { sInputSchemas = [InputSchema "age" "Int"]
--       , sOutputSchema = OutputSchema "result" "String" }
--     , rules = [Rule "rule1" [InputEntry "age" (Just (ConditionInt (Just ">=") 18))] 
--         (OutputEntry "result" "cars" "String")
--       , Rule "rule2" [InputEntry "age" (Just (ConditionInt (Just ">") 12))] 
--         (OutputEntry "result" "videogames" "String") 
--       , Rule "rule3" [InputEntry "age" Nothing] 
--         (OutputEntry "result" "toys" "String")
--       ]
--     }
--   }