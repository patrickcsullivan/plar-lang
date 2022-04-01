import Control.Exception (evaluate)
import Evaluate (holds)
import EvaluateExample (boolInterp, boolValuation, modInterp, modValuation)
import qualified Parser.Parser as Parser
import Syntax (Formula (..), Rltn (..), Term (..))
import Test.Hspec (context, describe, hspec, it, pending, shouldBe)

main :: IO ()
main = hspec $ do
  parserSpec
  holdsSpec

parserSpec =
  describe "Parser.Parser.run" $ do
    -- Formula connectives
    it "parses `and` connective" $ do
      Parser.run "x and y" `shouldBeRight` And (Atom (Rltn "x" [])) (Atom (Rltn "y" []))
    -- Predicates / relations
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
    context "for formula \"forall x. (x = 0) or (x = 1)\"" $ do
      let frm = Parser.run' "forall x. (x = 0) or (x = 1)"
      it "holds in Boolean interpretation and valuation" $ do
        holds boolInterp boolValuation frm `shouldBe` True
      it "holds in mod 2 interpretation and valuation" $ do
        holds (modInterp 2) (modValuation 2) frm `shouldBe` True
      it "does not hold in mod 3 interpretation and valuation" $ do
        holds (modInterp 3) (modValuation 3) frm `shouldBe` False

x `shouldBeRight` y = x `shouldBe` Right y

x `shouldBeLeft` y = x `shouldBe` Left y