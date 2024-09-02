
-- produced
simalarule1 :: Simala.Expr
simalarule1 = Let 
    (NonRec Transparent "PitchDecks" 
        (Fun Transparent ["stage","sector","stage_com","has_ESG","wants_ESG"] 
            (Builtin IfThenElse 
                [Builtin And 
                    [Builtin Eq [Var "stage",Atom "Seed"]
                    ,Builtin Eq [Var "sector",Atom "Information Technology"]
                    ,Builtin Eq [Var "stage_com",Atom "Pre-Revenue"]]
                ,Record [("opinion",Atom "interesting")]
                ,Builtin IfThenElse 
                    [Builtin And [Builtin Eq [Var "stage",Atom "Series A"],Builtin Eq [Var "sector",Atom "Information Technology"],Builtin Eq [Var "stage_com",Atom "Pre-Profit"]]
                    ,Record [("opinion",Atom "interesting")]
                    ,Builtin IfThenElse 
                        [Builtin And [Builtin Eq [Var "has_ESG",Lit (BoolLit True)],Builtin Eq [Var "wants_ESG",Lit (BoolLit True)]]
                        ,Record [("opinion",Atom "interesting")]
                        ,Record [("opinion",Atom "reject")]]]]))) 
    (Let 
        (NonRec Transparent "r1" 
            (App (Var "PitchDecks") [Atom "Seed",Var " \"a\"",Var " \"n\"",Var " true",Var " true"])) 
        (Var "r1"))

-- wants
-- let
--   A = fun (stage, sector, stage_com , has_ESG, wants_ESG) => ...
-- in let
--  r1 = A({stage = "seed", sector = "information technology", stage_com = "pre-revenue", has_ESG = true, wants_ESG = true})
-- in
--  d = r1.opinion
-- in
--  d 

simalarule2 :: Simala.Expr
simalarule2 = Let
    (NonRec
        Transparent
        "PitchDecks"
        (Fun
            Transparent ["stage", "sector", "stage_com", "has_ESG", "wants_ESG"]
            (Builtin IfThenElse
                [Builtin And [Builtin Eq [Var "stage", Atom "seed"],
                              Builtin Eq [Var "sector", Atom "information technology"],
                              Builtin Eq [Var "stage_com", Atom "pre-revenue"]],
                 Record [("opinion", Atom "interesting")],
                 Builtin IfThenElse 
                    [Builtin And [Builtin Eq [Var "stage", Atom "series a"],
                                  Builtin Eq [Var "sector", Atom "information technology"],
                                  Builtin Eq [Var "stage_com", Atom "pre-profit"]],
                     Record [("opinion", Atom "interesting")],
                     Record [("opinion", Atom "reject")]]]))
-- in
    (Let
        (NonRec
            Transparent -- decl
            "r1" -- name
            (App -- expr
                (Var "PitchDecks")
                [Atom "Seed",
                 Atom "InformationTechnology",
                 Atom "PreRevenue",
                 Lit (BoolLit True),
                 Lit (BoolLit True)]))
    -- in
        (Let 
            (NonRec Transparent "d" (Project (Var "r1") "opinion"))
            (Var "d")))



let
    `Table A` = fun (inputs) => ...
in let
    `Table B` = fun (inputs) => ...
in let
    d = `Table A` ("Fall", 5, true)
in let 
    b = B(d, false) 
in
    b

let (x, y) = T("pasta", 3) in
let z = S("foo", x) in
z