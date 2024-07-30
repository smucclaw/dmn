
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
    showProg (If c t (Just e)) = 
        nest nestingDepth
        (vsep [ pretty "if " <+> showProg c
              , pretty "then"
              , nest nestingDepth (showProg t)
              , pretty "else"
              , nest nestingDepth (showProg e)
              ])
    showProg _ = pretty "TBD"