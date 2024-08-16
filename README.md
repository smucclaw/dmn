# dmn to l4
Decision Model &amp; Notation (in Markdown and XML) to L4 (Simala)

Currently handles only single-hit policies and one decision table.

## Usage
```
stack run inputfile.md
```

## Composed of:
### 1. Parsing from MD
Parser that takes markdown inputs and parses them to the data structure defined in Types.hs.

Input form:
```
|F|b.stage (input, string)|b.sector (input, string)|b.stage_com (input, string)|b.has_ESG (input, bool)|inv.wants_ESG (input, bool)|opinion (output, string)
|---|---|---|---|---|---|---|
|1|Seed|Information Technology|Pre-Revenue|||interesting|
|2|Series A|Information Technology|Pre-Profit|||interesting|
|3||||TRUE|TRUE|interesting|
|4||||||reject|
```

dashes can also be used to represent a null input.

issues noticed so far:
- lack of ability to specify limited inputs (ie any input can be entered with no form of checking)
- difficulty in representing DRDs
    - this can be resolved by simply treating schemas of the same name in different tables as being the same

### 2. Type Checking and DMN data structure representation
```haskell
exampleParsed :: Decision
exampleParsed = Decision 
  {decisionOut = DecOutVar 
    {sDecVarName = "opinion"
    , sDecVarFEELType = "string"}
  , decisionInfoReq = [ReqInputEl {sReqInput = "stage"}
                      ,ReqInputEl {sReqInput = "sector"}
                      ,ReqInputEl {sReqInput = "stage_com"}
                      ,ReqInputEl {sReqInput = "has_ESG"}
                      ,ReqInputEl {sReqInput = "wants_ESG"}]
  , decisionLogic = DecTable 
  {hitPolicy = "F"
  , schema = Schema 
    {sInputSchemas = 
      [InputSchema {sInputSchemaId = "stage", inputExprFEELType = "string"}
      ,InputSchema {sInputSchemaId = "sector", inputExprFEELType = "string"}
      ,InputSchema {sInputSchemaId = "stage_com", inputExprFEELType = "string"}
      ,InputSchema {sInputSchemaId = "has_ESG", inputExprFEELType = "bool"}
      ,InputSchema {sInputSchemaId = "wants_ESG", inputExprFEELType = "bool"}]
    , sOutputSchema = OutputSchema 
      {sOutputSchemaVarName = "opinion", sOutputSchemaFEELType = "string"}}
    , rules = [Rule {ruleId = "rule1"
      , inputEntries = 
        [InputEntry {sInputEntryId = "stage", sMaybeCondition = Just (ConditionString "Seed")}
        ,InputEntry {sInputEntryId = "sector", sMaybeCondition = Just (ConditionString "Information Technology")}
        ,InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Just (ConditionString "Pre-Revenue")}]
      , outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}}
    ,Rule {ruleId = "rule2"
      , inputEntries = 
        [InputEntry {sInputEntryId = "stage", sMaybeCondition = Just (ConditionString "Series A")}
        ,InputEntry {sInputEntryId = "sector", sMaybeCondition = Just (ConditionString "Information Technology")}
        ,InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Just (ConditionString "Pre-Profit")}]
      , outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}}
    ,Rule {ruleId = "rule3"
      , inputEntries = [InputEntry {sInputEntryId = "stage", sMaybeCondition = Nothing}
        ,InputEntry {sInputEntryId = "sector", sMaybeCondition = Nothing},InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Nothing}
        ,InputEntry {sInputEntryId = "has_ESG", sMaybeCondition = Just (ConditionBool True)}
        ,InputEntry {sInputEntryId = "wants_ESG", sMaybeCondition = Just (ConditionBool True)}]
      , outputEntry = OutputEntry {sOutputId = "output", sExpr = "interesting"}}
    ,Rule {ruleId = "rule4"
      , inputEntries = [InputEntry {sInputEntryId = "stage", sMaybeCondition = Nothing}
        ,InputEntry {sInputEntryId = "sector", sMaybeCondition = Nothing}
        ,InputEntry {sInputEntryId = "stage_com", sMaybeCondition = Nothing}
        ,InputEntry {sInputEntryId = "has_ESG", sMaybeCondition = Nothing}
        ,InputEntry {sInputEntryId = "wants_ESG", sMaybeCondition = Nothing}]
      , outputEntry = OutputEntry {sOutputId = "output", sExpr = "reject"}}]}}
```

The produced DMN data structure is then type checked by comparing the inputExprFEELType with sMaybeCondition.

### 3. From XML - not currently implemented
### 4. Intermediate representation
```hs
exampleConverted :: CompiledRule
exampleConverted = MkCompiledRule (Func "opinion") [Arg "stage", Arg "sector", Arg "stage_com", Arg "has_ESG", Arg "wants_ESG"] 
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
```


### 5. Simala

### 6. Python
The intermediate representation is pretty printed to python, where each decision represents one function.

# Types.hs
This DMN representation uses a minimal version of the XML tags typically produced by DMN.

List of types in DMN:
* **Definitions**: Overall DMN definitions, including metadata and a list of decisions. (currently not in use)
* **DecOutVar**: Output variable of a decision.
* **Decision**: Represents a single decision with various attributes.
* **DecTableOrLitExpr**: Represents either a decision table or a literal expression. (currently only supports decision tables)
* **Schema**: Represents input and output schemas of a decision table.
* **InputSchema**: Schema of an input element, including id and type
* **OutputSchema**: Schema of an output element, including id and type
* **InfoReq**: An information requirement for a decision.
* **DMNRule**: A single rule in a decision table.
* **InputEntry**: Represents an input entry in an indivudual rule.
* **Condition**: Represents a condition, which is a FEEL expression based on the OMG documentation
* **OutputEntry**: Represents an output entry in an individual rule.

This version currently supports strings, bools, and integers.

# Various Hit Policies
## Single hit policies
### Unique
inputs cannot overlap - nested ifs

|U|Mark|Result|
|---|---|---|
|1|>=50|"Pass"|
|2|<50|"Fail"|

### First (F)
Outputs the **first** satisfied rule - nested ifs

|F|b.stage|b.sector|b.stage_com|b.has_ESG|inv.wants_ESG|opinion
|---|---|---|---|---|---|---|
|1|Seed|Information Technology|Pre-Revenue|-|-|interesting|
|2|Series A|Information Technology|Pre-Profit|-|-|interesting|
|3|-|-|-|TRUE|TRUE|interesting|
|4|-|-|-|-|-|reject|

### Any (A)
Multiple rules can be satisfied BUT they must generate the same output - nested ifs??

|A|Vacation Days|State|Result|
|---|---|---|---|
|1|0|-|"refused"|
|2|-|"probation"|"refused"|
|3|>0|"not probation"|"accepted"|

## Multiple hit policies - Not yet implemented
### Rule Order (R)
Returns all in hit order (list) - unnested ifs
* eg if age > 18, returns: "Cars", "Videogames", "Toys"

|R|Age|To Advertise|
|---|---|---|
|1|>18|"Cars"|
|2|>12|"Videogames"|
|3|-|"Toys"|

### Collect (C)
Returns all in any order (list) - unnested ifs

|R|Age|To Advertise|
|---|---|---|
|1|>18|"Cars"|
|2|>12|"Videogames"|
|3|-|"Toys"|

#### Aggregators
* all are outputted as a number
* therefore inputs must be a number too except for count

1. Sum (sum of all output values)
2. Min (smallest output value)
3. Max (largest output value)
4. Count (no. of outputs)

# Examples
## Multiple tables
|U|Mark (input, int)|Grade (string)|
|---|---|---|
|1|>=70|"A"|
|2|[60..70)|"B"|
|3|[50..60)|"C"|
|4|[40..50)|"D"|
|5|[30..40)|"E"|
|6|[20..30)|"F"|

|U|Attended (bool)|Attendance Pass (bool)|
|---|---|---|
|1|true|true|
|2|false|false|

|F|Grade (string)|Attendance Pass (bool)|Overall Result (bool)|
|---|---|---|---|
|1|-|false|"fail"|
|2|"D", "E", "F"|-|"fail"|
|3|"A", "B", "C"|true|"pass"|


grade -> overall result <- attendance
^ Not sure what would be the best way to display a DRD in markdown

Grade = InputEntry 

