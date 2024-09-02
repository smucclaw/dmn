
-- produced
simalarule1 :: Simala.Expr
simalarule1 = Let --expr 
    (NonRec -- decl
        Transparent 
        "PitchDecks" -- name
        (Fun -- expr
            Transparent ["stage","sector","stage_com","has_ESG","wants_ESG"] -- [Name]
            (Builtin IfThenElse -- expr
                [Builtin And [Builtin Eq [Var "stage",Var "Seed"]
                            ,Builtin Eq [Var "sector",Var "Information Technology"]
                            ,Builtin Eq [Var "stage_com",Var "Pre-Revenue"]]
                ,Record [("test",Var "interesting")]
                ,Builtin IfThenElse 
                    [Builtin And [Builtin Eq [Var "stage",Var "Series A"]
                                ,Builtin Eq [Var "sector",Var "Information Technology"]
                                ,Builtin Eq [Var "stage_com",Var "Pre-Profit"]]
                    ,Record [("test",Var "interesting")]
                    ,Builtin IfThenElse 
                        [Builtin And [Builtin Eq [Var "has_ESG",Lit (BoolLit True)]
                                    ,Builtin Eq [Var "wants_ESG",Lit (BoolLit True)]]
                        ,Record [("test",Var "interesting")]
                        ,Record [("test",Var "reject")]]]]))) 
    Undefined -- expr

-- wants
-- let
--   A = fun (stage, sector, stage_com , has_ESG, wants_ESG) => ...
-- in let
--  r1 = A({stage = "seed", sector = "information technology", stage_com = "pre-revenue", has_ESG = true, wants_ESG = true})
-- in
--  r1

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
        (Project (Var "r1") "opinion")))