
module PrintProg where

import ConvertDMN
import Prettyprinter (Doc, pretty, (<+>), vsep, hsep, indent, punctuate)
import qualified Data.Text as T

-- indentation: number of blank spaces
nestingDepth :: Int
nestingDepth = 4

class ShowProg x where
    showProg :: x -> Doc ann

instance ShowProg CompiledRule where
    showProg (MkCompiledRule f args (e:es)) = (vsep [ hsep [pretty (T.pack "def")
                                                            , showProg f 
                                                            , pretty (T.pack "(")
                                                            , showProg args
                                                            , pretty (T.pack "):") ]
                                            , indent nestingDepth (showProg e)])

instance ShowProg Func where
    showProg (Func f) = pretty f

instance ShowProg Expr where
    showProg (Var (Arg v)) = pretty v
    showProg (And exprs) = hsep (punctuate (pretty (T.pack " and")) (map showProg exprs))
    showProg (Equal e1 e2) = showProg e1 <+> pretty (T.pack "==") <+> showProg e2
    showProg (If c t (Just e)) = vsep [ pretty (T.pack "if") <+> showProg c <+> pretty (T.pack ":")
              , indent nestingDepth (showProg t)
              , pretty (T.pack "else:")
              , indent nestingDepth (showProg e)
            --   , pretty "then" <+> showProg t
            --   , pretty "else"
            --   , nest nestingDepth (showProg e)
              ]
    showProg (MoreThan e1 e2) = showProg e1 <+> pretty (T.pack ">") <+> showProg e2
    showProg (MoreThanEqual e1 e2) = showProg e1 <+> pretty (T.pack ">=") <+> showProg e2
    showProg (LessThan e1 e2) = showProg e1 <+> pretty (T.pack "<") <+> showProg e2
    showProg (LessThanEqual e1 e2) = showProg e1 <+> pretty (T.pack "<=") <+> showProg e2
    showProg (Range e1 e2) = pretty (T.pack "range(") <+> showProg e1 <+> pretty (T.pack ",") <+> showProg e2 <+> pretty (T.pack ")")
    showProg (Const val) = showProg val
    showProg (Return val) = pretty (T.pack "return") <+> showProg val 
    showProg _ = pretty (T.pack "erorr??")


instance ShowProg Bracket where
    showProg (Inclusive e) = showProg e
    showProg (Exclusive e) = pretty (T.pack "this program cant handle exclusive values mb")

instance ShowProg [Arg] where
    -- showProg (Arg a) = pretty a
    showProg args = hsep (punctuate (pretty (T.pack ",")) (map (\(Arg a) -> pretty a) args))

instance ShowProg Val where
    showProg (Bool b) = pretty b
    showProg (String s) = pretty (T.pack ("'" ++ s ++ "'"))
    showProg (Int n) = pretty n