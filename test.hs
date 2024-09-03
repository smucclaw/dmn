
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
            (Var "d"))))



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

Let 
    (NonRec Transparent "TableB" 
        (Fun Transparent ["Dish","Children"] 
            (Builtin IfThenElse 
                [Builtin And 
                    [Builtin Eq [Var "Dish",Atom "Spareribs"]
                    ,Builtin Eq [Var "Children",Lit (BoolLit False)]]
                ,Record [("Beverages",Atom "beer")]
                ,Builtin IfThenElse 
                    [Builtin And 
                        [Builtin Eq [Var "Dish",Atom "Stew"]
                        ,Builtin Eq [Var "Children",Lit (BoolLit False)]]
                    ,Record [("Beverages",Atom "wine")]
                    ,Builtin IfThenElse 
                        [Builtin And 
                            [Builtin Eq [Var "Dish",Atom "Roastbeef"]
                            ,Builtin Eq [Var "Children",Lit (BoolLit False)]]
                        ,Record [("Beverages",Atom "soda")]
                        ,Builtin IfThenElse 
                            [Builtin And [Builtin Eq [Var "Dish",Atom "Steak"]]
                            ,Record [("Beverages",Atom "lemonade")]
                            ,Record [("Beverages",Atom "apple juice")]]]]]))) 
    (Let (NonRec Transparent "TableA" 
        (Fun Transparent ["season","guests","veg guests"] 
            (Builtin IfThenElse 
                [Builtin And [Builtin Eq [Var "season",Atom "Fall"],Builtin Le [Var "guests",Lit (IntLit 8)],Builtin Eq [Var "veg guests",Lit (BoolLit False)]]
                ,Record [("Dish",Atom "Spareribs")]
                ,Builtin IfThenElse 
                    [Builtin And [Builtin Eq [Var "season",Atom "Winter"],Builtin Le [Var "guests",Lit (IntLit 8)],Builtin Eq [Var "veg guests",Lit (BoolLit False)]]
                    ,Record [("Dish",Atom "Roastbeef")]
                    ,Builtin IfThenElse 
                        [Builtin And [Builtin Eq [Var "season",Atom "Spring"],Builtin Le [Var "guests",Lit (IntLit 4)],Builtin Eq [Var "veg guests",Lit (BoolLit False)]]
                        ,Record [("Dish",Atom "Fancy Steak")]
                        ,Builtin IfThenElse 
                            [Builtin And [Builtin Eq [Var "season",Atom "Spring"],Builtin And [Builtin Ge [Lit (IntLit 5),Lit (IntLit 5)],Builtin Le [Lit (IntLit 8),Lit (IntLit 8)]],Builtin Eq [Var "veg guests",Lit (BoolLit False)]]
                            ,Record [("Dish",Atom "Stew")]
                            ,Builtin IfThenElse 
                                [Builtin And [Builtin Eq [Var "season",Atom "Summer"],Builtin Le [Var "guests",Lit (IntLit 8)],Builtin Eq [Var "veg guests",Lit (BoolLit False)]]
                                ,Record [("Dish",Atom "Light salad and a nice steak")]
                                ,Builtin IfThenElse 
                                    [Builtin And [Builtin Gt [Var "guests",Lit (IntLit 8)],Builtin Eq [Var "veg guests",Lit (BoolLit False)]]
                                    ,Record [("Dish",Atom "Stew")]
                                    ,Record [("Dish",Atom "Pasta")]]]]]]]))) 
        (Let (NonRec Transparent "r0" -- r0 = TableA("Fall", 5, true)
            (App 
                (Var "TableA") 
                [Atom "Fall",Var " 5",Var " true"])) 
            (Let (NonRec Transparent " d" -- d = r0.Dish
                (Project (Var "r0") "Dish")) 
                (Let (NonRec Transparent "r1" -- r1 = TableB(d, false)
                    (App 
                        (Var "TableB") 
                        [Var "d",Var " false"])) 
                    (Let (NonRec Transparent " b" -- b = r1.Beverages
                        (Project (Var "r1") "Beverages")) 
                        (Var " b"))))))