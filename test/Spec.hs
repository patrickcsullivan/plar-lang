import Control.Exception (evaluate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Evaluate (holds)
import EvaluateExample (boolInterp, boolValuation, modInterp, modValuation)
import qualified Parser.Parser as Parser
import Syntax (Formula (..), Rltn (..), Term (..))
import SyntaxOp (subst, termSubst, termVars, (|=>))
import Test.Hspec (context, describe, hspec, it, pending, shouldBe)

main :: IO ()
main = hspec $ do
  parserSpec
  holdsSpec
  termSubstSpec
  substSpec

parserSpec =
  describe "Parser.Parser.run" $ do
    -- Formula connectives
    it "parses `and` connective" $ do
      Parser.run "x and y" `shouldBeRight` And (Atom (Rltn "x" [])) (Atom (Rltn "y" []))
    -- Quantified formulas
    it "parses forall x" $ do
      Parser.run "forall x. x = y" `shouldBeRight` ForAll "x" (Atom (Rltn "=" [Var "x", Var "y"]))
    it "parses exists x" $ do
      Parser.run "exists x. x = y" `shouldBeRight` Exists "x" (Atom (Rltn "=" [Var "x", Var "y"]))
    -- Predicates / relations
    it "parses names with prime symbol" $ do
      Parser.run "c'" `shouldBeRight` Atom (Rltn "c'" [])
      Parser.run "c'()" `shouldBeRight` Atom (Rltn "c'" [])
      Parser.run "f'(x)" `shouldBeRight` Atom (Rltn "f'" [Var "x"])
    it "parses a nullary predicate without paraentheses" $ do
      Parser.run "c" `shouldBeRight` Atom (Rltn "c" [])
    it "parses a nullary predicate with paraentheses" $ do
      Parser.run "c()" `shouldBeRight` Atom (Rltn "c" [])
    it "parses a prefix predicate" $ do
      Parser.run "f(x, y, z)" `shouldBeRight` Atom (Rltn "f" [Var "x", Var "y", Var "z"])
    it "parses a reserved infix predicate" $ do
      Parser.run "x < y" `shouldBeRight` Atom (Rltn "<" [Var "x", Var "y"])
    -- Domain functions
    it "parses a prefix function" $ do
      Parser.run "f(g(x, y, z))" `shouldBeRight` Atom (Rltn "f" [Fn "g" [Var "x", Var "y", Var "z"]])
    it "parses a reserved infix function" $ do
      Parser.run "f(x + y)" `shouldBeRight` Atom (Rltn "f" [Fn "+" [Var "x", Var "y"]])
    -- Complicated formulas
    it "parses complicated" $ do
      Parser.run "forall x y. exists z. x < z + z and f(y) < z"
        `shouldBeRight` ForAll
          "x"
          ( ForAll
              "y"
              ( Exists
                  "z"
                  ( And
                      (Atom (Rltn "<" [Var "x", Fn "+" [Var "z", Var "z"]]))
                      (Atom (Rltn "<" [Fn "f" [Var "y"], Var "z"]))
                  )
              )
          )
    it "parses complicated" $ do
      Parser.run "exists x. (x = 0) or exists y. (y = 1)"
        `shouldBeRight` Exists
          "x"
          ( Or
              (Atom (Rltn "=" [Var "x", Var "0"]))
              ( Exists
                  "y"
                  (Atom (Rltn "=" [Var "y", Var "1"]))
              )
          )

holdsSpec =
  describe "Evaluate.holds" $ do
    context "formula: forall x. (x = 0) or (x = 1)" $ do
      let frm = Parser.run' "forall x. (x = 0) or (x = 1)"
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
  describe "SyntaxOp.subst" $ do
    context "formula: forall x. x = y" $ do
      let frm = Parser.run' "forall x. x = y"
      context "instantiation: y |-> x" $ do
        let inst = "y" |=> Var "x"
        it "renames x to x'" $ do
          subst inst frm `shouldBe` Parser.run' "forall x'. x' = x"
    context "formula: forall x x'. x = y ==> x = x'" $ do
      let frm = Parser.run' "forall x x'. x = y ==> x = x'"
      context "instantiation: y |-> x" $ do
        let inst = "y" |=> Var "x"
        it "renames x to x' and x' to x''" $ do
          subst inst frm `shouldBe` Parser.run' "forall x' x''. x' = x ==> x' = x''"

x `shouldBeRight` y = x `shouldBe` Right y

x `shouldBeLeft` y = x `shouldBe` Left y