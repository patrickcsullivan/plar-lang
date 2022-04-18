import Control.Exception (evaluate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Evaluate (holds)
import Evaluate.Example (boolInterp, boolValuation, modInterp, modValuation)
import Parser (parse, parse')
import Syntax (Formula (..), Rltn (..), Term (..))
import Syntax.Instantiation ((|=>))
import Syntax.Rewrite (pnf, simplify)
import Syntax.Skolemize (skolemize)
import Syntax.Substitution (subst, termSubst)
import Syntax.Vars (freeVars, termVars)
import Test.Hspec (context, describe, hspec, it, pending, shouldBe)

main :: IO ()
main = hspec $ do
  parserSpec
  holdsSpec
  termSubstSpec
  substSpec
  simplifySpec
  freeVarsSpec
  pnfSpec
  skolemizeSpec

parserSpec =
  describe "Parser.parse" $ do
    -- Formula connectives
    it "parses `not` connective" $ do
      parse "~p" `shouldBeRight` Not (Atom (Rltn "p" []))
    it "parses `and` connective" $ do
      parse "x and y" `shouldBeRight` And (Atom (Rltn "x" [])) (Atom (Rltn "y" []))
    -- Quantified formulas
    it "parses forall X" $ do
      parse "forall X. x = y" `shouldBeRight` ForAll "X" (Atom (Rltn "=" [Var "x", Var "y"]))
    it "parses exists x" $ do
      parse "exists X. x = y" `shouldBeRight` Exists "X" (Atom (Rltn "=" [Var "x", Var "y"]))
    -- Predicates / relations
    it "parses names with prime symbol" $ do
      parse "c'" `shouldBeRight` Atom (Rltn "c'" [])
      parse "c'()" `shouldBeRight` Atom (Rltn "c'" [])
      parse "f'(x)" `shouldBeRight` Atom (Rltn "f'" [Var "x"])
    it "parses a nullary predicate without paraentheses" $ do
      parse "c" `shouldBeRight` Atom (Rltn "c" [])
    it "parses a nullary predicate with paraentheses" $ do
      parse "c()" `shouldBeRight` Atom (Rltn "c" [])
    it "parses a prefix predicate" $ do
      parse "f(x, y, z)" `shouldBeRight` Atom (Rltn "f" [Var "x", Var "y", Var "z"])
    it "parses a reserved infix predicate" $ do
      parse "x < y" `shouldBeRight` Atom (Rltn "<" [Var "x", Var "y"])
    -- Domain functions
    it "parses a prefix function" $ do
      parse "f(g(x, y, z))" `shouldBeRight` Atom (Rltn "f" [Fn "g" [Var "x", Var "y", Var "z"]])
    it "parses a reserved infix function" $ do
      parse "f(x + y)" `shouldBeRight` Atom (Rltn "f" [Fn "+" [Var "x", Var "y"]])
    -- Complicated formulas
    it "parses complicated" $ do
      parse "forall X Y. exists Z. x < z + z and f(y) < z"
        `shouldBeRight` ForAll
          "X"
          ( ForAll
              "Y"
              ( Exists
                  "Z"
                  ( And
                      (Atom (Rltn "<" [Var "x", Fn "+" [Var "z", Var "z"]]))
                      (Atom (Rltn "<" [Fn "f" [Var "y"], Var "z"]))
                  )
              )
          )
    it "parses complicated" $ do
      parse "exists X. (x = 0) or exists Y. (y = 1)"
        `shouldBeRight` Exists
          "X"
          ( Or
              (Atom (Rltn "=" [Var "x", Var "0"]))
              ( Exists
                  "Y"
                  (Atom (Rltn "=" [Var "y", Var "1"]))
              )
          )
    it "parses complicated" $ do
      parse "(forall X. P(x) or R(y)) ==> exists Y Z. Q(y) or ~(exists Z. P(z) or Q(z))"
        `shouldBeRight` Imp
          ( ForAll
              "X"
              ( Or
                  (Atom (Rltn "P" [Var "x"]))
                  (Atom (Rltn "R" [Var "y"]))
              )
          )
          ( Exists
              "Y"
              ( Exists
                  "Z"
                  ( Or
                      (Atom (Rltn "Q" [Var "y"]))
                      (Not (Exists "Z" (Or (Atom (Rltn "P" [Var "z"])) (Atom (Rltn "Q" [Var "z"])))))
                  )
              )
          )
    it "parses complicated" $ do
      parse "exists Y. x < y ==> forall U. exists V. x * u < y * v"
        `shouldBeRight` Exists
          "Y"
          ( Imp
              (Atom (Rltn "<" [Var "x", Var "y"]))
              ( ForAll
                  "U"
                  ( Exists
                      "V"
                      ( Atom
                          ( Rltn
                              "<"
                              [ Fn "*" [Var "x", Var "u"],
                                Fn "*" [Var "y", Var "v"]
                              ]
                          )
                      )
                  )
              )
          )

wrong =
  Or
    ( Not
        ( Atom
            ( Rltn
                "<"
                [ Var "x",
                  Fn "f_y" [Var "x"]
                ]
            )
        )
    )
    ( Atom
        ( Rltn
            "<"
            [ Fn
                "*"
                [ Var "x",
                  Var "u"
                ],
              Fn
                "*"
                [ Fn "f_y" [Var "x"],
                  Fn "f_v" [Var "u", Var "x"]
                ]
            ]
        )
    )

holdsSpec =
  describe "Evaluate.holds" $ do
    context "formula: forall X. (x = 0) or (x = 1)" $ do
      let frm = parse' "forall X. (x = 0) or (x = 1)"
      it "holds in Boolean interpretation and valuation" $ do
        holds boolInterp boolValuation frm `shouldBe` True
      it "holds in mod 2 interpretation and valuation" $ do
        holds (modInterp 2) (modValuation 2) frm `shouldBe` True
      it "does not hold in mod 3 interpretation and valuation" $ do
        holds (modInterp 3) (modValuation 3) frm `shouldBe` False

termSubstSpec =
  describe "SyntaxOp.termSubst" $ do
    context "term: f(x, y)" $ do
      let fTrm = Fn "f" [Var "x", Var "y"]
      context "instantiation: x |-> g(a, b), y |-> h(c, d)" $ do
        let gTrm = Fn "g" [Var "a", Var "b"]
        let hTrm = Fn "h" [Var "c", Var "d"]
        let inst = Map.fromList [("x", gTrm), ("y", hTrm)]
        it "substitues terms" $ do
          termSubst inst fTrm `shouldBe` Fn "f" [gTrm, hTrm]
        it "free vars in the substituted term are precisely those free in the terms that are substituted in" $ do
          termVars (termSubst inst fTrm) `shouldBe` (termVars gTrm `Set.union` termVars hTrm)

substSpec =
  describe "Syntax.Substitution.subst" $ do
    context "formula: forall X. x = y" $ do
      let frm = parse' "forall X. x = y"
      context "instantiation: y |-> x" $ do
        let inst = "y" |=> Var "x"
        it "renames x to x'" $ do
          subst inst frm `shouldBe` parse' "forall X'. x' = x"
    context "formula: forall X X'. x = y ==> x = x'" $ do
      let frm = parse' "forall X X'. x = y ==> x = x'"
      context "instantiation: y |-> x" $ do
        let inst = "y" |=> Var "x"
        it "renames x to x' and x' to x''" $ do
          subst inst frm `shouldBe` parse' "forall X' X''. x' = x ==> x' = x''"

freeVarsSpec =
  describe "Syntax.Vars.freeVars" $ do
    it "returns all variables when not quantified" $ do
      freeVars (parse' "P(x, y, z)") `shouldBe` Set.fromList ["x", "y", "z"]
    it "returns only free variables when quantified" $ do
      freeVars (parse' "forall X. P(x, y)") `shouldBe` Set.singleton "y"

simplifySpec =
  describe "Syntax.Rewrite.simplify" $ do
    it "removes quantifiers over unused variables" $ do
      simplify (parse' "forall X. P(y)") `shouldBe` parse' "P(y)"
    it "removes quantifier over variable that's unused because it's shadowed" $ do
      let frm = parse' "exists X Y. P(x) or exists Y. Q(y)"
          exp = parse' "exists X. P(x) or exists Y. Q(y)"
      simplify frm `shouldBe` exp
    it "simplifies complicated formula" $ do
      let frm = parse' "forall X Y Z. x or True ==> True and ~(~P(y))"
          exp = parse' "forall Y. P(y)"
      simplify frm `shouldBe` exp

pnfSpec =
  describe "Syntax.Rewrite.pnf" $ do
    it "converts complicated formula into PNF" $ do
      let frm = parse' "(forall X. P(x) or R(y)) ==> exists Y Z. Q(y) or ~(exists Z. P(z) and Q(z))"
          exp = parse' "exists X. forall Z. ~P(x) and ~R(y) or Q(x) or ~P(z) or ~Q(z)"
      pnf frm `shouldBe` exp

skolemizeSpec =
  describe "Syntax.Skolemize.skolemize" $ do
    it "Skolemizes complicated formula" $ do
      let frm = parse' "exists Y. x < y ==> forall U. exists V. x * u < y * v"
          exp = parse' "~(x < f_y(x)) or x * u < f_y(x) * f_v(u, x)"
      skolemize frm `shouldBe` exp
    it "Skolemizes complicated formula" $ do
      let frm = parse' "forall X. P(x) ==> (exists Y Z. Q(y) or ~(exists Z. P(z) and Q(z)))"
          exp = parse' "~P(x) or Q(c_y()) or ~P(z) or ~Q(z)"
      skolemize frm `shouldBe` exp

-- rewriteSpec =
--   describe "Rewrite" $ do
--     describe "simplify" $ do
--       it "simplifies" $ do
--         let frm = parse' "forall x y z. x or True ==> True and ~~y"
--         simplify frm `shouldBe`
--     context "(forall x. P(x) or R(y)) ==> exists y z. Q(y) or ~(exists z. P(z) or Q(z))" $ do
--       it "" $ do
--         pending

x `shouldBeRight` y = x `shouldBe` Right y

x `shouldBeLeft` y = x `shouldBe` Left y

p =
  Or
    ( Not
        ( Atom
            ( Rltn
                "<"
                [ Var "x",
                  Fn
                    "f_y"
                    [Var "x"]
                ]
            )
        )
    )
    ( Atom
        ( Rltn
            "<"
            [ Fn
                "*"
                [Var "x", Var "u"],
              Fn
                "*"
                [ Fn
                    "f_y"
                    [Var "x"],
                  Fn
                    "f_v"
                    [Var "u", Var "x"]
                ]
            ]
        )
    )

q =
  Or
    ( Not
        ( Atom
            ( Rltn
                "<"
                [ Var "x",
                  Fn
                    "f_y"
                    [Var "x"]
                ]
            )
        )
    )
    ( Atom
        ( Rltn
            "<"
            [ Fn "*" [Var "x", Var "u"],
              Fn
                "*"
                [ Fn "f_y" [Var "x"],
                  Fn
                    "f_v"
                    [Var "u", Var "x"]
                ]
            ]
        )
    )