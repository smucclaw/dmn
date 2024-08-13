# dmn
Decision Model &amp; Notation (in Markdown and XML) to L4

# Types.hs
List of types in DMN:
* **DecOutVar**: Represents an output variable of a decision.
* **Definitions**: Represents the overall DMN definitions, including metadata and a list of decisions.
* **Decision**: Represents a single decision with various attributes.
* **DecTableOrLitExpr**: Represents either a decision table or a literal expression.
* **Schema**: Represents the schema of a decision table, including input and output schemas.
* **InputSchema**: Represents the schema of an input element.
* **OutputSchema**: Represents the schema of an output element.
* **InfoReq**: Represents an information requirement for a decision.
* **DMNRule**: Represents a single rule in a decision table.
* **InputEntry**: Represents an entry in a decision table's input column.
* **Condition**: Represents a condition, which is a FEEL expression.
* **OutputEntry**: Represents an entry in a decision table's output column.

Currently not using FEEL expressions yet

1. Parsing from MD
// issues noticed so far:
// lack of ability to specify limited inputs (ie any input can be entered with no form of checking)

// specify information in brackets - (input/output, type (default type is string))

2. Parsing from XML
3. DMN data structure representation
4. Intermediate representation
5. Simala

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
## Simple example - one input, one output 
|U|Mark|Grade|
|---|---|---|
|1|70|"A"|
|2|60|"B"|
|3|50|"C"|

## Pitch decks example
|F|b.stage|b.sector|b.stage_com|b.has_ESG|inv.wants_ESG|opinion
|---|---|---|---|---|---|---|
|1|Seed|Information Technology|Pre-Revenue|-|-|interesting|
|2|Series A|Information Technology|Pre-Profit|-|-|interesting|
|3|-|-|-|TRUE|TRUE|interesting|
|4|-|-|-|-|-|reject|

```haskell
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
```

## Expanded 2 table example
|U|Mark|Grade|
|---|---|---|
|1|>=70|"A"|
|2|[60..70)|"B"|
|3|[50..60)|"C"|
|4|[40..50)|"D"|
|5|[30..40)|"E"|
|6|[20..30)|"F"|

|U|Attended|Attendance Pass|
|---|---|---|
|1|true|true|
|2|false|false|

|F|Grade|Attendance Pass|Overall Result|
|---|---|---|---|
|1|-|false|"fail"|
|2|"D", "E", "F"|-|"fail"|
|3|"A", "B", "C"|true|"pass"|


grade -> overall result <- attendance
^ Not sure what would be the best way to display a DRD in markdown

Grade = InputEntry 

