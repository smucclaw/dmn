
module PrintProgJavascript where

import ConvertDMN
import Prettyprinter (Doc, pretty, (<+>), vsep, hsep, indent, punctuate)
import qualified Data.Text as T
import Data.Char (toLower)

-- indentation: number of blank spaces
nestingDepth :: Int
nestingDepth = 4

class ShowProgJs x where
    showProgJs :: x -> Doc ann

instance ShowProgJs CompiledDRD where
    showProgJs (DRD rules calls) = vsep [ vsep (map showProgJs rules)
                                         , vsep (map showProgJs calls)]

instance ShowProgJs CompiledRule where
    showProgJs (MkCompiledRule table e) = (vsep [ hsep [pretty (T.pack "function")
                                                            , showProgJs table]
                                            , indent nestingDepth (showProgJs e), pretty (T.pack "}")])

instance ShowProgJs TableSignature where
    showProgJs (MkTableSignature f inputs outputs) = showProgJs f
                                                        <> pretty (T.pack "(")
                                                        <> showProgJs inputs
                                                        <> pretty (T.pack ") {")

instance ShowProgJs [ColumnSignature] where
    showProgJs cols = hsep (punctuate (pretty (T.pack ",")) (map showProgJs cols))

instance ShowProgJs ColumnSignature where
    showProgJs (MkColumnSignature a t) = showProgJs a

instance ShowProgJs Call where
    showProgJs call = 
        case call of
            MkCall f inputs [output] -> hsep [pretty (T.pack "let")
                                            , showProgJs output
                                             , pretty (T.pack "=")
                                             , showProgJs f
                                             <> pretty (T.pack "(")
                                             <> showProgJs inputs
                                             <> pretty (T.pack ");")]
            MkCall f inputs outputs -> hsep [pretty (T.pack "let")
                                            , pretty (T.pack "{")
                                            <> hsep (punctuate (pretty (T.pack ",")) (zipWith (\i v -> pretty (T.pack ("output" ++ show i)) 
                                            <> pretty (T.pack ":") <+> showProgJs v) [1..] outputs))
                                            <> pretty (T.pack "} =")
                                            , showProgJs f
                                            <> pretty (T.pack "(")
                                            <> showProgJs inputs
                                            <> pretty (T.pack ");")]


instance ShowProgJs Func where
    showProgJs (Func f) =  pretty (T.map toLower (T.replace (T.pack " ") (T.pack "_") (T.pack f)))

instance ShowProgJs [Expr] where
    showProgJs exprs = vsep (map showProgJs exprs)

instance ShowProgJs Expr where
    showProgJs (Var (Arg v)) = pretty (T.map toLower (T.replace (T.pack " ") (T.pack "_") (T.pack v)))
    showProgJs (And exprs) = hsep (punctuate (pretty (T.pack " &&")) (map showProgJs exprs))
    showProgJs (Equal e1 e2) = showProgJs e1 <+> pretty (T.pack "===") <+> showProgJs e2
    showProgJs (If c t (Just e)) = vsep [ pretty (T.pack "if (") <> showProgJs c <> pretty (T.pack ") {")
              , indent nestingDepth (showProgJs t)
              , pretty (T.pack "} else ") <> showElse e
              ]
    showProgJs (If c t Nothing) = vsep [ pretty (T.pack "if (") <+> showProgJs c <+> pretty (T.pack ") {")
              , indent nestingDepth (showProgJs t)
              , pretty (T.pack "}")
              ]
    showProgJs (MoreThan e1 e2) = showProgJs e1 <+> pretty (T.pack ">") <+> showProgJs e2
    showProgJs (MoreThanEqual e1 e2) = showProgJs e1 <+> pretty (T.pack ">=") <+> showProgJs e2
    showProgJs (LessThan e1 e2) = showProgJs e1 <+> pretty (T.pack "<") <+> showProgJs e2
    showProgJs (LessThanEqual e1 e2) = showProgJs e1 <+> pretty (T.pack "<=") <+> showProgJs e2
    showProgJs (Range e1 e2 e3) = showProgJs e3 <+> pretty (T.pack ">") <> showProgJs e1 <+> pretty (T.pack "&&") <+> showProgJs e3 <+> pretty (T.pack "<") <> showProgJs e2
    showProgJs (Const val) = showProgJs val
    showProgJs (Return vals) = pretty (T.pack "return") <+> showReturnVals vals <> pretty (T.pack ";")
    showProgJs (InitList (ListName l)) = pretty (T.pack "var") <+> pretty (T.map toLower (T.pack l)) <+> pretty (T.pack "=") <+> pretty (T.pack "[];")
    showProgJs (AppendList (ListName l) vals) = pretty (T.map toLower (T.pack l)) <> pretty (T.pack ".push(") <+> showProgJs vals <+> pretty (T.pack ");")
    showProgJs _ = pretty (T.pack "error!!")

showElse :: Expr -> Doc ann
showElse (If c t (Just e)) = vsep [ pretty (T.pack "if (") <> showProgJs c <> pretty (T.pack ") {")
                                  , indent nestingDepth (showProgJs t)
                                  , pretty (T.pack "} else ") <> showElse e
                                  ]
showElse e = vsep [ pretty (T.pack "{")
                  , indent nestingDepth (showProgJs e)
                  , pretty (T.pack "}")
                  ]

showReturnVals :: [Val] -> Doc ann
showReturnVals [val] = showProgJs val
showReturnVals vals = pretty (T.pack "{") 
                        <> hsep (punctuate (pretty (T.pack ",")) (zipWith (\i v -> pretty (T.pack ("output" ++ show i)) 
                        <> pretty (T.pack ":") <+> showProgJs v) [1..] vals)) 
                        <> pretty (T.pack "}")


instance ShowProgJs Bracket where
    showProgJs (Inclusive e) = pretty (T.pack "=") <+> showProgJs e
    showProgJs (Exclusive e) = showProgJs e

instance ShowProgJs [Arg] where
    showProgJs args = hsep (punctuate (pretty (T.pack ",")) (map showProgJs args))

instance ShowProgJs Arg where
    showProgJs (Arg a) = pretty (T.map toLower (T.replace (T.pack " ") (T.pack "_") (T.strip (T.pack a))))

instance ShowProgJs [Val] where
    showProgJs vals = hsep (punctuate (pretty (T.pack ",")) (map showProgJs vals))

instance ShowProgJs Val where
    showProgJs (Bool True)  = pretty (T.pack "true")
    showProgJs (Bool False) = pretty (T.pack "false")
    showProgJs (String s) = pretty (T.pack ("'" ++ s ++ "'"))
    showProgJs (Int n) = pretty n
    showProgJs (Double n) = pretty n
    showProgJs (List (ListName l)) = pretty (T.map toLower (T.pack l))


instance ShowProgJs [Argument] where
    showProgJs args = hsep (punctuate (pretty (T.pack ",")) (map showProgJs args))

instance ShowProgJs Argument where
    showProgJs (ValArgument v) = showProgJs v
    showProgJs (VarArgument e) = showProgJs e