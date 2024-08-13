-- test/Spec.hs
import Test.Hspec
import Main (convertMDToDMN, convertDecision, convertedRules)
import System.IO (readFile)

main :: IO ()
main = hspec $ do
  describe "convertMDToDMN" $ do
    it "parses markdown table 1 correctly" $ do
      markdownTable <- readFile "test/examples/pitchdecks.md"
      let decision = convertMDToDMN markdownTable
      decision `shouldBe` expectedDecision1

  describe "convertDecision" $ do
    it "converts intermediate rep 1 correctly" $ do
      let decision = expectedDecision1
      let rules = convertDecision decision
      rules `shouldBe` expectedConvertedDecision1

  describe "convertedRules" $ do
    it "converts 1 to python correctly" $ do
      markdownTable <- readFile "test/examples/pitchdecks.md"
      let rules = convertedRules markdownTable
      rules `shouldBe` expectedConvertedRules1

expectedDecision1 = Decision {decisionOut = DecOutVar {sDecVarName = "opinion", sDecVarFEELType = "string"}, decisionInfoReq = [ReqInputEl {sReqInput = "stage"},ReqInputEl {sReqInput = "sector"},ReqInputEl {sReqInput = "stage_com"},ReqInputEl {sReqInput = "has_ESG"},ReqInputEl {sReqInput = "wants_ESG"}], decisionLogic = DecTable {hitPolicy = "F", schema = Schema {sInputSchemas = [InputSchema {sInputSchemaId = "stage", inputExprFEELType = "string"},InputSchema {sInputSchemaId = "sector", inputExprFEELType = "string"},InputSchema {sInputSchemaId = "stage_com", inputExprFEELType = "string"},InputSchema {sInputSchemaId = "has_ESG", inputExprFEELType = "bool"},InputSchema {sInputSchemaId = "wants_ESG", inputExprFEELType = "bool"}], sOutputSchema = OutputSchema {sOutputSchemaVarName = "opinion", sOutputSchemaFEELType = "string"}}, rules = [Rule {ruleId = "rule1", inputEntries = [InputEntry {sInputEntryId = "stage", sMaybeCondition = Just (ConditionString "Seed")},InputEntry {sInputEntryId = "sector", sMaybeCondition = Just (ConditionString "Information Technology")},InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Just (ConditionString "Pre-Revenue")},InputEntry {sInputEntryId = "has_ESG", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "wants_ESG", sMaybeCondition = Nothing}], outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}},Rule {ruleId = "rule2", inputEntries = [InputEntry {sInputEntryId = "stage", sMaybeCondition = Just (ConditionString "Series A")},InputEntry {sInputEntryId = "sector", sMaybeCondition = Just (ConditionString "Information Technology")},InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Just (ConditionString "Pre-Profit")},InputEntry {sInputEntryId = "has_ESG", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "wants_ESG", sMaybeCondition = Nothing}], outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}},Rule {ruleId = "rule3", inputEntries = [InputEntry {sInputEntryId = "stage", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "sector", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "has_ESG", sMaybeCondition = Just (ConditionBool True)},InputEntry {sInputEntryId = "wants_ESG", sMaybeCondition = Just (ConditionBool True)}], outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}},Rule {ruleId = "rule4", inputEntries = [InputEntry {sInputEntryId = "stage", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "sector", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "has_ESG", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "wants_ESG", sMaybeCondition = Nothing}], outputEntry = OutputEntry {sOutputId = "output", sExpr = "reject"}}]}}
expectedConvertedDecision1 = MkCompiledRule (Func "opinion") [Arg "stage",Arg "sector",Arg "stage_com",Arg "has_ESG",Arg "wants_ESG"] [If (And [Equal (Var (Arg "stage")) (Const (String "Seed")),Equal (Var (Arg "sector")) (Const (String "Information Technology")),Equal (Var (Arg "stage_com")) (Const (String "Pre-Revenue"))]) (Return (String "interesting")) (Just (If (And [Equal (Var (Arg "stage")) (Const (String "Series A")),Equal (Var (Arg "sector")) (Const (String "Information Technology")),Equal (Var (Arg "stage_com")) (Const (String "Pre-Profit"))]) (Return (String "interesting")) (Just (If (And [Equal (Var (Arg "has_ESG")) (Const (Bool True)),Equal (Var (Arg "wants_ESG")) (Const (Bool True))]) (Return (String "interesting")) (Just (Return (String "reject")))))))]
expectedConvertedRules2 = 
    def opinion ( stage, sector, stage_com, has_ESG, wants_ESG ):
        if stage == 'Seed' and sector == 'Information Technology' and stage_com == 'Pre-Revenue' :
            return 'interesting'
        else:
            if stage == 'Series A' and sector == 'Information Technology' and stage_com == 'Pre-Profit' :
                return 'interesting'
            else:
                if has_ESG == True and wants_ESG == True :
                    return 'interesting'
                else:
                    return 'reject'