module TranslateToSimala where

import ConvertDMN
import qualified Simala.Expr.Type as Simala
import qualified Data.Text as T
import Data.Char (toLower)
import Data.List (find)


translateToSimala :: CompiledDRD -> Simala.Expr
translateToSimala (DRD rules calls) = 
    let ruleDecls = map translateRule rules
        callDecls = concat $ zipWith (translateCalls rules) (init calls) [0..]
        lastCall = last calls
    in Simala.mkLet (ruleDecls ++ callDecls) (translateLastCall rules lastCall (length calls - 1))

translateLastCall :: [CompiledRule] -> Call -> Int -> Simala.Expr
translateLastCall rules (MkCall (Func funcName) inputs _) index =
    case findRuleByFuncName rules (T.pack funcName) of
        Just (MkCompiledRule (MkTableSignature _ inNames outNames) _ _) -> 
            Simala.App (Simala.Var (T.pack funcName)) 
                [Simala.Record [(extractName inName, compileApp input) | (inName, input) <- zip inNames inputs]]
        Nothing -> error $ "Rule not found for function: " ++ funcName

findRuleByFuncName :: [CompiledRule] -> T.Text -> Maybe CompiledRule
findRuleByFuncName rules funcName = 
    find (\(MkCompiledRule (MkTableSignature (Func name) _ _) _ _) -> name == (T.unpack funcName)) rules

translateCalls :: [CompiledRule] -> Call -> Int -> [Simala.Decl]
translateCalls rules call index = 
    case findRuleByFuncName rules (T.pack $ callFuncName call) of
        Just rule -> translateCall call index (map extractName (ins rule)) (map extractName (out rule))
        Nothing -> error "Corresponding function not found"
    where
        callFuncName (MkCall (Func name) _ _) = name
        out (MkCompiledRule (MkTableSignature (Func funcName) _ outColumns) _ _) = outColumns
        ins (MkCompiledRule (MkTableSignature (Func funcName) inColumns _) _ _) = inColumns



-- createRecordProjection :: Simala.Decl -> [Simala.Decl] -> Simala.Decl
-- createRecordProjection (Simala.NonRec _ callname (Simala.App (Var tablename) [_])) decls = 
--     Simala.Project (Simala.Var callname) (findOutputName tablename decls)

translateRule :: CompiledRule -> Simala.Decl
translateRule rule@(MkCompiledRule (MkTableSignature (Func funcName) inputs _) args exprs) = 
    Simala.NonRec Simala.Transparent (T.pack funcName) (Simala.Fun Simala.Transparent [argName] (compileBody rule exprs))
    where
        argName = T.pack $ "input_" ++ funcName
        compileBody :: CompiledRule -> [ConvertDMN.Expr] -> Simala.Expr
        compileBody r [expr] = compileExpr r argName expr
        compileBody r exprs = Simala.List (map (compileExpr r argName) exprs)

compileExpr :: CompiledRule -> T.Text -> ConvertDMN.Expr -> Simala.Expr
compileExpr rule@(MkCompiledRule (MkTableSignature _ inputs outputs) _ _) argName expr = case expr of
    Var (Arg v) -> 
        case find (\(MkColumnSignature (Arg name) _) -> name == v) inputs of
            Just _ -> Simala.Project (Simala.Var argName) (T.pack v)
            Nothing -> Simala.Var (T.pack v)
    And exprs -> Simala.Builtin Simala.And (map (compileExpr rule argName) exprs)
    Or exprs -> Simala.Builtin Simala.Or (map (compileExpr rule argName) exprs)
    Equal e1 e2 -> Simala.Builtin Simala.Eq [compileExpr rule argName e1, compileExpr rule argName e2]
    If c t (Just e) -> Simala.Builtin Simala.IfThenElse [compileExpr rule argName c, compileExpr rule argName t, compileExpr rule argName e]
    MoreThan e1 e2 -> Simala.Builtin Simala.Gt [compileExpr rule argName e1, compileExpr rule argName e2]
    MoreThanEqual e1 e2 -> Simala.Builtin Simala.Ge [compileExpr rule argName e1, compileExpr rule argName e2]
    LessThan e1 e2 -> Simala.Builtin Simala.Lt [compileExpr rule argName e1, compileExpr rule argName e2]
    LessThanEqual e1 e2 -> Simala.Builtin Simala.Le [compileExpr rule argName e1, compileExpr rule argName e2]
    Range e1 e2 -> compileRange rule argName e1 e2
    Const val -> compileVal val
    Return vals -> Simala.Record [(extractName output, compileVal val) | (output, val) <- zip outputs vals]  -- other cases

extractName :: ColumnSignature -> T.Text
extractName (MkColumnSignature (Arg name) _) = T.pack name


translateCall :: Call -> Int -> [T.Text] -> [T.Text] -> [Simala.Decl]
translateCall (MkCall (Func funcName) inputs outputs) index inNames outNames = 
    let outputArgs = map (\(VarArgument (Arg s)) -> T.pack s) outputs
        baseDecl = Simala.NonRec Simala.Transparent (T.pack ("r" ++ show index)) 
                (Simala.App (Simala.Var (T.pack funcName)) 
                [Simala.Record [(inName, compileApp input) | (inName, input) <- zip inNames inputs]])
        projections = zipWith (\outputArg outName -> Simala.NonRec Simala.Transparent outputArg (Simala.Project (Simala.Var (T.pack ("r" ++ show index))) outName)) outputArgs outNames
    in baseDecl : projections

-- fix name eventually
compileApp :: Argument -> Simala.Expr
compileApp (ValArgument a) = compileVal a
compileApp (VarArgument (Arg a)) = Simala.Var (T.pack a)

compileRange :: CompiledRule -> T.Text -> Bracket -> Bracket -> Simala.Expr 
compileRange rule argName (Inclusive e1) (Inclusive e2) = 
    Simala.Builtin Simala.And [Simala.Builtin Simala.Ge [Simala.Var argName, compileExpr rule argName e1], Simala.Builtin Simala.Le [Simala.Var argName, compileExpr rule argName e2]]
compileRange rule argName (Inclusive e1) (Exclusive e2) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Ge [Simala.Var argName, compileExpr rule argName e1], Simala.Builtin Simala.Lt [Simala.Var argName, compileExpr rule argName e2]]
compileRange rule argName (Exclusive e1) (Inclusive e2) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Gt [Simala.Var argName, compileExpr rule argName e1], Simala.Builtin Simala.Le [Simala.Var argName, compileExpr rule argName e2]]
compileRange rule argName (Exclusive e1) (Exclusive e2) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Gt [Simala.Var argName, compileExpr rule argName e1], Simala.Builtin Simala.Lt [Simala.Var argName, compileExpr rule argName e2]]

compileVal :: Val -> (Simala.Expr)
compileVal val = case val of
    Bool b -> Simala.Lit (Simala.BoolLit b)
    String s -> Simala.Atom (T.pack s) 
    Int i -> Simala.Lit (Simala.IntLit i)
