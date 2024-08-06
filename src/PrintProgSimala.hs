
module PrintProgSimala where

import ConvertDMN
import Prettyprinter

-- indentation: number of blank spaces
nestingDepth :: Int
nestingDepth = 4

class ShowProgSimala x where
    showProgSimala :: x -> Doc ann

instance ShowProgSimala CompiledRule where
    showProgSimala (MkCompiledRule f (arg:args) (e:es)) = nest nestingDepth
        (vsep [ pretty "let" 
              , nest nestingDepth (pretty "get_opinion =")
              , indent nestingDepth (pretty "fun (" <+> showProgSimala arg <+> pretty ") =>")
              , nest nestingDepth (showProgSimala e)
              ])

instance ShowProgSimala Expr where
    showProgSimala (Var (Arg v)) = pretty v
    showProgSimala (And exprs) = align $ indent nestingDepth (vsep $ zipWith (<>) (pretty "   " : repeat (pretty "&& ")) (map showProgSimala exprs))
    showProgSimala (Equal e1 e2) = showProgSimala e1 <+> pretty "==" <+> showProgSimala e2
    showProgSimala (If c t (Just e)) = 
        nest nestingDepth
        (vsep [ indent nestingDepth (pretty "if ") -- for some reason nest not working
              , nest nestingDepth (showProgSimala c)
              , nest nestingDepth (pretty "then" <+> showProgSimala t)
              , nest nestingDepth (pretty "else" <+> showProgSimala e)
            --   , pretty "then" <+> showProg t
            --   , pretty "else"
            --   , nest nestingDepth (showProg e)
              ])
    showProgSimala (MoreThan e1 e2) = showProgSimala e1 <+> pretty ">" <+> showProgSimala e2
    showProgSimala (MoreThanEqual e1 e2) = showProgSimala e1 <+> pretty ">=" <+> showProgSimala e2
    showProgSimala (LessThan e1 e2) = showProgSimala e1 <+> pretty "<" <+> showProgSimala e2
    showProgSimala (LessThanEqual e1 e2) = showProgSimala e1 <+> pretty "<=" <+> showProgSimala e2
    showProgSimala (Const val) = showProgSimala val
    showProgSimala _ = pretty "TBD"

instance ShowProgSimala Arg where
    showProgSimala (Arg a) = pretty a

instance ShowProgSimala Val where
    showProgSimala (Bool b) = pretty b
    showProgSimala (String s) = pretty ("'" ++ s)
    showProgSimala (Number n) = pretty n


-- in simala:
-- let

--   get_opinion =

--     fun (stage, sector, stage_com, has_esg, wants_esg) =>

--     if
--             stage     == 'seed
--          && sector    == 'information_technology
--          && stage_com == 'pre_revenue
          
--       ||    stage     == 'series_a
--          && sector    == 'information_technology
--          && stage_com == 'pre_profit
          
--       ||    has_esg
--          && wants_ESG

--     then 'interesting
--     else 'reject

-- in 

--   get_opinion
--     (
--       { stage     = 'seed,
--         sector    = 'information_technology,
--         stage_com = 'pre_profit,
--         has_ESG = true }
--     )