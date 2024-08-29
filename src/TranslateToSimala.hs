module TranslateToSimala where

import ConvertDMN
import qualified Simala.Expr.Type as Simala
import qualified Data.Text as T

translateToSimala :: CompiledDRD -> Simala.Decl


--- mkLets :: [(Name, Expr)] -> Expr -> Expr
-- mkLets []             e  = e
-- mkLets ((n, e1) : ds) e2 = Let Transparent n e1 (mkLets ds e2)

translateToSimala 

translateRule :: CompiledRule -> Simala.Expr -- (Fun Transparency [Name] Expr)
translateRule (MkCompiledRule (Func funcName) args exprs) = 
        Simala.Fun Simala.Transparent (map argToName args) (compileBody exprs)
    where
        argToName (Arg a) = T.pack a

compileBody :: [ConvertDMN.Expr] -> Simala.Expr
compileBody [expr] = compileExpr expr
compileBody exprs = Simala.Record (Simala.Row (map compileExpr exprs))

-- type Row a = [(Name, a)]

compileExpr :: ConvertDMN.Expr -> Simala.Expr
compileExpr expr 
    | Var (Arg v) <- expr = Simala.Var (T.pack v)
    | And exprs <- expr = Simala.Builtin Simala.And (map compileExpr exprs)
    | Or exprs <- expr = Simala.Builtin Simala.Or (map compileExpr exprs)
    | Equal e1 e2 <- expr = Simala.Builtin Simala.Eq [compileExpr e1, compileExpr e2]
    | If c t (Just e) <- expr = Simala.Builtin Simala.IfThenElse [compileExpr c, compileExpr t, compileExpr e]
    | MoreThan e1 e2 <- expr = Simala.Builtin Simala.Gt [compileExpr e1, compileExpr e2]
    | MoreThanEqual e1 e2 <- expr = Simala.Builtin Simala.Ge [compileExpr e1, compileExpr e2]
    | LessThan e1 e2 <- expr = Simala.Builtin Simala.Lt [compileExpr e1, compileExpr e2]
    | LessThanEqual e1 e2 <- expr = Simala.Builtin Simala.Le [compileExpr e1, compileExpr e2]
    | Range e1 e2 <- expr = compileRange e1 e2
    | Const val <- expr = compileVal val
    | Return vals <- expr = Simala.Record (map compileVal vals)

compileRange :: Bracket -> Bracket -> Simala.Expr 
compileRange (Inclusive e1) (Inclusive e2) = 
    Simala.Builtin Simala.And [Simala.Builtin Simala.Ge [compileExpr e1, compileExpr e1], Simala.Builtin Simala.Le [compileExpr e2, compileExpr e2]]
compileRange (Inclusive e1) (Exclusive e2) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Ge [compileExpr e1, compileExpr e1], Simala.Builtin Simala.Lt [compileExpr e2, compileExpr e2]]
compileRange (Exclusive e1) (Inclusive e2) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Gt [compileExpr e1, compileExpr e1], Simala.Builtin Simala.Le [compileExpr e2, compileExpr e2]]
compileRange (Exclusive e1) (Exclusive e2) =
    Simala.Builtin Simala.And [Simala.Builtin Simala.Gt [compileExpr e1, compileExpr e1], Simala.Builtin Simala.Lt [compileExpr e2, compileExpr e2]]

compileVal :: Val -> Simala.Expr
compileVal val = case val of
    Bool b -> Simala.Lit (Simala.BoolLit b)
    String s -> Simala.Atom (T.pack s)
    Int i -> Simala.Lit (Simala.IntLit i)
