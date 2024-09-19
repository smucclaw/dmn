# dmn to l4

This code takes a single DMN table as an input and converts it into Simala.


## Usage
```
stack run inputfile.md
```

Input files must be in the following format, similar to that of a markdown table:

|[Hit policy] (table_name) |Arg1 (input, type)|Arg2 (output, type)|
|---|---|---|
|1|"Input1"|output1|
|2|"Input1"|output2|
...

table_name("Input1", variable_name)

where:
1. The **hit policy** is specified with a singular letter and must be [one of the following listed here](#various-hit-policies).
2. Each arg must be specified to be 
    * either an **input/output**
    * type - **string, int** (including ranges and intervals in the form >, <, and [55..67]), or **bool**
3. There can be **multiple** inputs and outputs
4. Null inputs must be represented as **- or left blank**.
5. Function calls are in a prolog-style format, where outputs are treated as variables and can be reused if there are multiple calls
   * There should be a space between table declarations and function calls

example:
```prolog
function1(inputA, outputA)
function2 (outputA, outputB) // here outputA from function1 is used as an input for function2
```

6. Comments can be added with //

## Multiple tables
|F (Play)|outlook (input, string)| temp(input, int) | humidity (Input, int) |windy (input, bool)| golf (output, bool)| swimming (output, int)|
|---|---|---|---|---|---|---|
|1|"sunny"|>80|>85|-|false|1|
|2|"overcast"||||true|1|
|3|"rain"|-|-|true|false|0|
|4|"rain"|-|-|false|true|0|
|5|"sunny"|[71..80)|(70..95]||false|1|
|6|"sunny"|<71|<=70|false|true|1|

|U (which)|golf (input, bool)|swimming (input, int)|choice (output, string)|
|---|---|---|---|
|1|-|1|"swimming"|
|2|true|0|"golf"|
|3|false|0|"stay at home"|

|F (wear_sunglasses)|outlook (input, string)| sport(input, string)| sunglasses (output, bool)|
|---|---|---|---|
|1|"sunny"|"golf"|true|
|2|-||false|

Play("sunny", 75, 90, true, golf, swim)

which(golf, swim, choice)

wear_sunglasses("sunny", choice, sunglasses)


This translates to 3 function declarations, and 3 calls. The outputs of the first 2 tables are inputted into the last table through the use of the same variable names. In simala, this will be represented as:
```hs
#eval let
   Play = fun (input_Play) => 
      if   input_Play.outlook == 'sunny
        && input_Play.temp > 80
        && input_Play.humidity > 85
      then {golf = false,swimming = 1} 
      else
        if   input_Play.outlook == 'overcast
        then {golf = true,swimming = 1} 
        else
          if   input_Play.outlook == 'rain
            && input_Play.windy == true
          then {golf = false,swimming = 0} 
          else
            if   input_Play.outlook == 'rain
              && input_Play.windy == false
            then {golf = true,swimming = 0} 
            else
              if   input_Play.outlook == 'sunny
                && input_Play.temp >= 71
                && input_Play.temp < 80
                && input_Play.humidity > 70
                && input_Play.humidity <= 95
              then {golf = false,swimming = 1} 
              else
                {golf = true,swimming = 1}
in let
   which = fun (input_which) => 
      if   input_which.swimming == 1
      then {choice = 'swimming} 
      else
        if   input_which.golf == true
          && input_which.swimming == 0
        then {choice = 'golf} 
        else
          {choice = '`stay at home`}
in let
   wear_sunglasses = fun (input_wear_sunglasses) => 
      if   input_wear_sunglasses.outlook == 'sunny
        && input_wear_sunglasses.sport == 'golf
      then {sunglasses = true} 
      else
        {sunglasses = false}
in let
   r0 = Play({outlook = 'sunny,temp = 75,humidity = 90,windy = true})
in let
   golf = r0.golf
in let
   swim = r0.swimming
in let
   r1 = which({golf = golf,swimming = swim})
in let
   choice = r1.choice
in wear_sunglasses({outlook = 'sunny,sport = choice})
```


## Progress
A list of all transpilations and their current status
|Target|Input/Output|Status|
|---|---|---|
|DMN (Markdown)|Input|Currently handles single-hit policies, with some translations to multi-hit policies.|
|XML|Input|In progress|
|Simala|Output|Unique, First, and Any hit policies. FEEL expressions are limited - Currently supports Int, String and Bool |
|Python|Output|Unique, First, Any, Collect, and Rule order hit policies. FEEL expressions are limited - Currently supports Int, String and Bool |
|JavaScipt|Output|Unique, First, Any, Collect, and Rule order hit policies. FEEL expressions are limited - Currently supports Int, String and Bool |


## Current Features: 
- Single tables
  - Handle **multiple inputs/outputs**
  - Feel expressions current accepted include: **Bool, Int, and String**
      - Int can be ranges such as: **>, >=, <, <=, [55..67], (34..67)**, where square brackets represent inclusive values and round brackets represent exclusive values
  - **Null inputs** (in table declaration) can be represented as **'-' or simply left blank**; however inputs taken in during calls cannot have null inputs.
- Multiple tables
   - **Multiple tables** can be connected to each other through **function calls**, where the overlapping columns must share the **same variable name**.
   - Each new table must be be separated by a line. 
* **Multi-hit policies** (such as collect and rule order) return lists in python and simala. Support for multi-hit policies in simala has not yet been added.
- **Type checking** implemented for rule/function/table declaration, ensures that entries into columns match the type declared in the column header
    * Calls are also type checked

## Incomplete features:
- Type checking of hit policies to ensure:
  - No overlapping rules (eg for unique) 
  - No recursion between rules
- Limiting the inputs to be a specific range for a an argument
- Addition of all possible feel expressions, including date, time, possibly functions?, lists? as seen in [the drools documentation](https://docs.drools.org/latest/drools-docs/drools/DMN/index.html#dmn-feel-data-types-ref_dmn-models)


## Composed of:
### 1. Parsing from Markdown
[Parser](src/FromMD.hs) that takes markdown inputs and parses them to the data structure defined in [Types](#typeshs).

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

### 2. Type Checking and DMN data structure representation

Example of table parsed into DMN data structure representation, which can be found in [Types.hs](src/Types.hs):

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

The produced DMN data structure is then [type checked](src/TypeChecking.hs).

The type checking covers:
* Inputs matching with header types
* Call arguments, including variables, matching with their respective columns

### 3. From XML - not currently implemented
### 4. Intermediate representation

This is then converted to an intermediate representation, found in [ConvertDMN.hs](src/ConvertDMN.hs).

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

The IR is then [translated](src/TranslateToSimala.hs) to the [Simala AST](https://github.com/smucclaw/simala/tree/main).

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
The intermediate representation is [pretty printed to python](src/PrintProg.hs), where each decision represents one function.

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

### 7. Javascript
The same is done for [javascript](src/PrintProgJavascript.hs).

```js
function pitchdecks(stage, sector, stage_com, has_esg, wants_esg) {
    if (stage === 'Seed' && sector === 'Information Technology' && stage_com === 'Pre-Revenue') {
        return 'interesting';
    } else if (stage === 'Series A' && sector === 'Information Technology' && stage_com === 'Pre-Profit') {
        return 'interesting';
    } else if (has_esg === true && wants_esg === true) {
        return 'interesting';
    } else {
        return 'reject';
    }
}
let o = pitchdecks('Seed', 'Information Technology', 'Pre-Revenue', true, false);
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
### Unique (U)
Inputs cannot overlap - Represented by nested ifs

|U (pass)|Mark (input, int)|Result (output, string)|
|---|---|---|
|1|>=50|"Pass"|
|2|<50|"Fail"|

pass (50, result)

this will translate to a singular function and function call, which should return {result = pass} 

### First (F)
Outputs the **first** satisfied rule - Represented by nested ifs

### Any (A)
Multiple rules can be satisfied BUT they must generate the same output - Represented by nested ifs.

|A (probation_eg)|Vacation Days (int, input)|Probation (input, bool)|Result (output, string)|
|---|---|---|---|
|1|0|-|"refused"|
|2|-|true|"refused"|
|3|>0|false|"accepted"|

probation_eg (4, true, result)

## Multiple hit policies
### Rule Order (R)
Returns all in hit order (list) - Represented by initialising a list, and adding to it upon every hit.
* eg if age > 18, returns: "Cars", "Videogames", "Toys"

|R (advertise)|Age (input, int)|To Advertise (output, string)|
|---|---|---|
|1|>18|"Cars"|
|2|>12|"Videogames"|
|3|-|"Toys"|

This would theoretically translate to in simala:
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

### Collect (C)
Returns all in any order (list) - Represented by initialising a list, and adding to it upon every hit.

#### Aggregators
THESE HAVE NOT YET BEEN IMPLEMENTED.
* all are outputted as a number
* therefore inputs must be a number too except for count

1. Sum (sum of all output values): C+
2. Min (smallest output value): C<
3. Max (largest output value): C>
4. Count (no. of outputs): C#
