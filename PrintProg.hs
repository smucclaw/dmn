
module PrintProg where

import ConvertDMN
import Prettyprinter

-- indentation: number of blank spaces
nestingDepth :: Int
nestingDepth = 4

class ShowProg x where
    showProg :: x -> Doc ann

instance ShowProg CompiledRule where
    showProg (MkCompiledRule _args (e:es)) = showProg e

instance ShowProg Expr where
    showProg (Var (Arg v)) = pretty v
    showProg (And exprs) = hsep (punctuate (pretty " &&") (map showProg exprs))
    showProg (Equal e1 e2) = showProg e1 <+> pretty "==" <+> showProg e2
    showProg (If c t (Just e)) = 
        nest nestingDepth
        (vsep [ pretty "if" <+> showProg c <+> pretty ":"
              , nest nestingDepth (showProg t)
              , pretty "else:"
              , nest nestingDepth (showProg e)
            --   , pretty "then" <+> showProg t
            --   , pretty "else"
            --   , nest nestingDepth (showProg e)
              ])
    showProg (MoreThan e1 e2) = showProg e1 <+> pretty ">" <+> showProg e2
    showProg (MoreThanEqual e1 e2) = showProg e1 <+> pretty ">=" <+> showProg e2
    showProg (LessThan e1 e2) = showProg e1 <+> pretty "<" <+> showProg e2
    showProg (LessThanEqual e1 e2) = showProg e1 <+> pretty "<=" <+> showProg e2
    showProg (Const val) = showProg val
    showProg _ = pretty "TBD"

instance ShowProg Val where
    showProg (Bool b) = pretty b
    showProg (String s) = pretty ("'" ++ s ++ "'")
    showProg (Number n) = pretty n