# dmn to l4

This code takes a single DMN table as an input and converts it into Simala.

A list of all transpilations and their current status
|Target|Input/Output|Status|
|---|---|---|
|DMN (Markdown)|Input|Currently handles only single-hit policies. Multi-hit policies in progress|
|XML|Input|In progress|
|Simala|Output|FEEL expressions are limited - Currently supports Int, String and Bool |
|Python|Output|FEEL expressions are limited - Currently supports Int, String and Bool |
|JavaScipt|Output|FEEL expressions are limited - Currently supports Int, String and Bool |

## Usage
```
stack run inputfile.md
```


## Composed of:
### 1. Parsing from MD
Parser that takes markdown inputs and parses them to the data structure defined in Types.hs.

Input form:
```
|F (PitchDecks)|stage (input, String)|sector (input, String)|stage_com (input, String)|has_ESG (input, Bool)|wants_ESG (input, Bool)|opinion (output, String)|
|---|---|---|---|---|---|---|
|1|"Seed"|"Information Technology"|"Pre-Revenue"|||"interesting"|
|2|"Series A"|"Information Technology"|"Pre-Profit"|||"interesting"|
|3||||TRUE|TRUE|"interesting"|
|4||||||"reject"|

PitchDecks("Seed", "Information Technology", "Pre-Profit", true, true, o)

// example of a comment
```

Note:
- dashes can also be used to represent a null input.
- comments can be written with the prefix '//'

issues noticed so far:
- lack of ability to specify limited inputs (ie any input can be entered with no form of checking)

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
```haskell
let
   PitchDecks = fun (input_PitchDecks) => 
      if   input_PitchDecks.stage == 'Seed
        && input_PitchDecks.sector == '`Information Technology`
        && input_PitchDecks.stage_com == '`Pre-Revenue`
       then {opinion = 'interesting} 
      else
        if   input_PitchDecks.stage == '`Series A`
          && input_PitchDecks.sector == '`Information Technology`
          && input_PitchDecks.stage_com == '`Pre-Profit`
         then {opinion = 'interesting} 
        else
          if   input_PitchDecks.has_ESG == true
            && input_PitchDecks.wants_ESG == true
           then {opinion = 'interesting} 
          else
            {opinion = 'reject}
in PitchDecks({stage = 'Seed, sector = '`Information Technology`, stage_com = 'Pre_Profit, has_ESG = true, wants_ESG = true})
```

Running this produces ```{opinion = 'interesting}```

### 6. Python
The intermediate representation is pretty printed to python, where each decision represents one function.

```py
def pitchdecks ( stage, sector, stage_com, has_esg, wants_esg ):
    if stage == 'Seed' and sector == 'Information Technology' and stage_com == 'Pre-Revenue' :
        return 'interesting'
    else:
        if stage == 'Series A' and sector == 'Information Technology' and stage_com == 'Pre-Profit' :
            return 'interesting'
        else:
            if has_esg == True and wants_esg == True :
                return 'interesting'
            else:
                return 'reject'
o = pitchdecks ( 'Seed', 'Information Technology', 'Pre-Profit', True, True )
```

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

|U (pass)|Mark|Result|
|---|---|---|
|1|>=50|"Pass"|
|2|<50|"Fail"|

pass (50, result)

this will translate to a singular function and function call, which should return {result = pass} 

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

This would translate to:
```hs
let
   advertising = fun (input_advertising) => 
      if   input_advertising.Age > 18
      then {`To Advertise` = {1 = 'Cars}} 
      else
        if   input_advertising.Age > 12
        then {`To Advertise` = {2 = 'Videogames}} 
        else
          {`To Advertise` = {3 = 'Toys}}
in advertising({Age = 13})
```

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

This translates to 3 function declarations, a

# STATUS
## CURRENT PROGRESS: 
- single tables working well
  - multiple inputs
  - multiple outputs
  - table names should be declared in the same cell as the hit policy (altho i need to change it to **handle spaces in names**)
  - Feel expressions current accepted include: Bool, Int (including ranges/intervals), and String
  - null inputs (in table declaration) can be represented as '-' or simply left blank; however inputs taken in during calls cannot have null inputs - check dmn documentation if this is true?
  - this transpiles to python, and to some extent simala
  - type checking implemented for rule/function/table declaration, ensures that entries into columns match the type declared in the column header
- all translations of MkCalls work for multiple tables

## TO WORK ON NEXT:
- type check hit policies
  - call types are the same as rules types
  - no recursion
  - ???
  - limit the possible inputs that can be entered
- (lower priority) addition of all possible feel expressions, including date, time, possibly functions?, lists? as seen in [the drools documentation](https://docs.drools.org/latest/drools-docs/drools/DMN/index.html#dmn-feel-data-types-ref_dmn-models)
- think of a way to limit the inputs to certain values only? eg for stage in pitchdecks, it can only be seed, pre-seed etc