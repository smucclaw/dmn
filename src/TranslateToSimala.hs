module TranslateToSimala where

import ConvertDMN
import qualified Simala.Expr.Type as Simala
import qualified Data.Text as T
import Data.Char (toLower)
import Data.List (find)


translateToSimala :: CompiledDRD -> Simala.Expr -- should be Let Decl (in) Expr
translateToSimala (DRD rules calls) = 
    let ruleDecls = map translateRule rules -- shld pass down the column signatures for outputs
        callDecls = concat $ zipWith (transateCalls rules) calls [0..]
        lastExpr = extractLastExpr (last callDecls)
        -- recordProjection = createRecordProjection (last callDecls) ruleDecls
    -- in Simala.mkLet ruleDecls (Simala.mkLet callDecls Simala.Undefined)
    in Simala.mkLet (ruleDecls ++ callDecls) lastExpr
    where 
        extractLastExpr (Simala.NonRec _ name _) = Simala.Var name

findRuleByFuncName :: [CompiledRule] -> T.Text -> Maybe CompiledRule
findRuleByFuncName rules funcName = 
    find (\(MkCompiledRule (MkTableSignature (Func name) _ _) _ _) -> name == (T.unpack funcName)) rules

transateCalls :: [CompiledRule] -> Call -> Int -> [Simala.Decl]
transateCalls rules call index = 
    case findRuleByFuncName rules (T.pack $ callFuncName call) of
        Just rule -> translateCall call index (map extractName (out rule))
        Nothing -> error "Corresponding function not found"
    where
        callFuncName (MkCall (Func name) _ _) = name
        out (MkCompiledRule (MkTableSignature (Func funcName) _ outColumns) _ _) = outColumns


-- createRecordProjection :: Simala.Decl -> [Simala.Decl] -> Simala.Decl
-- createRecordProjection (Simala.NonRec _ callname (Simala.App (Var tablename) [_])) decls = 
--     Simala.Project (Simala.Var callname) (findOutputName tablename decls)

translateRule :: CompiledRule -> Simala.Decl -- decl = NonRec (Fun Transparency [Name] Expr) in parser is: NonRec <$> transparency <*> name <* symbol "=" <*> expr
translateRule (MkCompiledRule (MkTableSignature (Func funcName) inputs outputs) args exprs) = 
        Simala.NonRec Simala.Transparent (T.pack funcName) (Simala.Fun Simala.Transparent (map argToName args) (compileBody exprs outputs))
    where
        argToName (Arg a) = T.pack a

translateCall :: Call -> Int -> [T.Text] -> [Simala.Decl]
translateCall (MkCall (Func funcName) inputs outputs) index fieldNames = 
    let outputArgs = map (\(VarArgument (Arg s)) -> T.pack s) outputs
        baseDecl = Simala.NonRec Simala.Transparent (T.pack ("r" ++ show index)) (Simala.App (Simala.Var (T.pack funcName)) (map compileApp inputs))
        projections = zipWith (\outputArg fieldName -> Simala.NonRec Simala.Transparent outputArg (Simala.Project (Simala.Var (T.pack ("r" ++ show index))) fieldName)) outputArgs fieldNames
    in baseDecl : projections

-- fix name evenetually
compileApp :: Argument -> Simala.Expr
compileApp (ValArgument a) = compileVal a
compileApp (VarArgument (Arg a)) = Simala.Var (T.pack a)

compileBody :: [ConvertDMN.Expr] -> [ColumnSignature] -> Simala.Expr
compileBody [expr] outputs = compileExpr expr outputs
compileBody exprs outputs = Simala.List (map (`compileExpr` outputs) exprs)

compileExpr :: ConvertDMN.Expr -> [ColumnSignature] -> Simala.Expr
compileExpr expr outputs
    | Var (Arg v) <- expr = Simala.Var (T.pack v)
    | And exprs <- expr = 
        case exprs of
            [singleExpr] -> compileExpr singleExpr outputs
            _ -> Simala.Builtin Simala.And (map (`compileExpr` outputs) exprs)
    | Or exprs <- expr = Simala.Builtin Simala.Or (map (`compileExpr` outputs) exprs)
    | Equal e1 e2 <- expr = Simala.Builtin Simala.Eq [compileExpr e1 outputs, compileExpr e2 outputs]
    | If c t (Just e) <- expr = Simala.Builtin Simala.IfThenElse [compileExpr c outputs, compileExpr t outputs, compileExpr e outputs] -- find a better way to do this...
    | MoreThan e1 e2 <- expr = Simala.Builtin Simala.Gt [compileExpr e1 outputs, compileExpr e2 outputs]
    | MoreThanEqual e1 e2 <- expr = Simala.Builtin Simala.Ge [compileExpr e1 outputs, compileExpr e2 outputs]
    | LessThan e1 e2 <- expr = Simala.Builtin Simala.Lt [compileExpr e1 outputs, compileExpr e2 outputs]
    | LessThanEqual e1 e2 <- expr = Simala.Builtin Simala.Le [compileExpr e1 outputs, compileExpr e2 outputs]
    | Range e1 e2 <- expr = compileRange e1 e2
    | Const val <- expr = compileVal val
    | Return vals <- expr = Simala.Record [(extractName output, compileVal val) | (output, val) <- zip outputs vals]  -- other cases

extractName :: ColumnSignature -> T.Text
extractName (MkColumnSignature (Arg name) _) = T.pack name

compileRange :: Bracket -> Bracket -> Simala.Expr 
compileRange (Inclusive (Const e1)) (Inclusive (Const e2)) = 
    Simala.Builtin Simala.And [Simala.Builtin Simala.Ge [compileVal e1, compileVal e1], Simala.Builtin Simala.Le [compileVal e2, compileVal e2]]
compileRange (Inclusive (Const e1)) (Exclusive (Const e2)) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Ge [compileVal e1, compileVal e1], Simala.Builtin Simala.Lt [compileVal e2, compileVal e2]]
compileRange (Exclusive (Const e1)) (Inclusive (Const e2)) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Gt [compileVal e1, compileVal e1], Simala.Builtin Simala.Le [compileVal e2, compileVal e2]]
compileRange (Exclusive (Const e1)) (Exclusive (Const e2)) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Gt [compileVal e1, compileVal e1], Simala.Builtin Simala.Lt [compileVal e2, compileVal e2]]

compileVal :: Val -> (Simala.Expr)
compileVal val = case val of
    Bool b -> Simala.Lit (Simala.BoolLit b)
    String s -> Simala.Atom (T.pack s) 
    Int i -> Simala.Lit (Simala.IntLit i)
