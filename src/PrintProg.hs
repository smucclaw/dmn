
module PrintProg where

import ConvertDMN
import Prettyprinter (Doc, pretty, (<+>), vsep, hsep, indent, punctuate)
import qualified Data.Text as T
import Data.Char (toLower)

-- indentation: number of blank spaces
nestingDepth :: Int
nestingDepth = 4

class ShowProg x where
    showProg :: x -> Doc ann

instance ShowProg CompiledDRD where
    showProg (DRD rules calls) = vsep [ vsep (map showProg rules)
                                         , vsep (map showProg calls)]

instance ShowProg CompiledRule where
    showProg (MkCompiledRule table e) = (vsep [ hsep [pretty (T.pack "def")
                                                            , showProg table]
                                            , indent nestingDepth (showProg e)])

instance ShowProg TableSignature where
    showProg (MkTableSignature f inputs outputs) = hsep [showProg f
                                                        , pretty (T.pack "(")
                                                        , showProg inputs
                                                        , pretty (T.pack "):")]

instance ShowProg [ColumnSignature] where
    showProg cols = hsep (punctuate (pretty (T.pack ",")) (map showProg cols))

instance ShowProg ColumnSignature where
    showProg (MkColumnSignature a t) = showProg a

instance ShowProg Call where
    showProg (MkCall f inputs outputs) = hsep [showProg outputs
                                            , pretty (T.pack "=")
                                            , showProg f
                                            , pretty (T.pack "(")
                                            , showProg inputs
                                            , pretty (T.pack ")")]

instance ShowProg Func where
    showProg (Func f) =  pretty (T.map toLower (T.replace (T.pack " ") (T.pack "_") (T.pack f)))

instance ShowProg [Expr] where
    showProg exprs = vsep (map showProg exprs)

instance ShowProg Expr where
    showProg (Var (Arg v)) = pretty (T.map toLower (T.replace (T.pack " ") (T.pack "_") (T.pack v)))
    showProg (And exprs) = hsep (punctuate (pretty (T.pack " and")) (map showProg exprs))
    showProg (Equal e1 e2) = showProg e1 <+> pretty (T.pack "==") <+> showProg e2
    showProg (If c t (Just e)) = vsep [ pretty (T.pack "if") <+> showProg c <+> pretty (T.pack ":")
              , indent nestingDepth (showProg t)
              , pretty (T.pack "else:")
              , indent nestingDepth (showProg e)
              ]
    showProg (If c t Nothing) = vsep [ pretty (T.pack "if") <+> showProg c <+> pretty (T.pack ":")
              , indent nestingDepth (showProg t)
              ]
    showProg (MoreThan e1 e2) = showProg e1 <+> pretty (T.pack ">") <+> showProg e2
    showProg (MoreThanEqual e1 e2) = showProg e1 <+> pretty (T.pack ">=") <+> showProg e2
    showProg (LessThan e1 e2) = showProg e1 <+> pretty (T.pack "<") <+> showProg e2
    showProg (LessThanEqual e1 e2) = showProg e1 <+> pretty (T.pack "<=") <+> showProg e2
    showProg (Range e1 e2 e3) = showProg e3 <+> pretty (T.pack ">") <> showProg e1 <+> pretty (T.pack "and") <+> showProg e3 <+> pretty (T.pack "<") <> showProg e2
    showProg (Const val) = showProg val
    showProg (Return vals) = pretty (T.pack "return") <+> showProg vals
    showProg (InitList (ListName l)) = pretty (T.map toLower (T.pack l)) <+> pretty (T.pack "=") <+> pretty (T.pack "[]")
    showProg (AppendList (ListName l) vals) = pretty (T.map toLower (T.pack l)) <> pretty (T.pack ".append(") <+> showProg vals <+> pretty (T.pack ")")
    showProg _ = pretty (T.pack "erorr??")


instance ShowProg Bracket where
    showProg (Inclusive e) = pretty (T.pack "=") <+> showProg e
    showProg (Exclusive e) = pretty (T.pack "this program cant handle exclusive values")

instance ShowProg [Arg] where
    showProg args = hsep (punctuate (pretty (T.pack ",")) (map showProg args))

instance ShowProg Arg where
    showProg (Arg a) = pretty (T.map toLower (T.replace (T.pack " ") (T.pack "_") (T.strip (T.pack a))))

instance ShowProg [Val] where
    showProg vals = hsep (punctuate (pretty (T.pack ",")) (map showProg vals))

instance ShowProg Val where
    showProg (Bool b) = pretty b
    showProg (String s) = pretty (T.pack ("'" ++ s ++ "'"))
    showProg (Int n) = pretty n
    showProg (List (ListName l)) = pretty (T.map toLower (T.pack l))

instance ShowProg [Argument] where
    showProg args = hsep (punctuate (pretty (T.pack ",")) (map showProg args))

instance ShowProg Argument where
    showProg (ValArgument v) = showProg v
    showProg (VarArgument e) = showProg e