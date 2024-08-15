
module PrintProg where

import ConvertDMN
import Prettyprinter

-- indentation: number of blank spaces
nestingDepth :: Int
nestingDepth = 4

class ShowProg x where
    showProg :: x -> Doc ann

instance ShowProg CompiledRule where
    showProg (MkCompiledRule f args (e:es)) = (vsep [ hsep [pretty "def"
                                                            , showProg f 
                                                            , pretty "("
                                                            , showProg args
                                                            , pretty "):" ]
                                            , indent nestingDepth (showProg e)])

instance ShowProg Func where
    showProg (Func f) = pretty f

instance ShowProg Expr where
    showProg (Var (Arg v)) = pretty v
    showProg (And exprs) = hsep (punctuate (pretty " and") (map showProg exprs))
    showProg (Equal e1 e2) = showProg e1 <+> pretty "==" <+> showProg e2
    showProg (If c t (Just e)) = vsep [ pretty "if" <+> showProg c <+> pretty ":"
              , indent nestingDepth (showProg t)
              , pretty "else:"
              , indent nestingDepth (showProg e)
            --   , pretty "then" <+> showProg t
            --   , pretty "else"
            --   , nest nestingDepth (showProg e)
              ]
    showProg (MoreThan e1 e2) = showProg e1 <+> pretty ">" <+> showProg e2
    showProg (MoreThanEqual e1 e2) = showProg e1 <+> pretty ">=" <+> showProg e2
    showProg (LessThan e1 e2) = showProg e1 <+> pretty "<" <+> showProg e2
    showProg (LessThanEqual e1 e2) = showProg e1 <+> pretty "<=" <+> showProg e2
    showProg (Const val) = showProg val
    showProg (Return val) = pretty "return" <+> showProg val 
    showProg _ = pretty "erorr??"

instance ShowProg [Arg] where
    -- showProg (Arg a) = pretty a
    showProg args = hsep (punctuate (pretty ",") (map (\(Arg a) -> pretty a) args))

instance ShowProg Val where
    showProg (Bool b) = pretty b
    showProg (String s) = pretty ("'" ++ s ++ "'")
    showProg (Int n) = pretty n