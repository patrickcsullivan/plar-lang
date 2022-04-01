import Control.Exception (evaluate)
import qualified Parser.Parser as Parser
import Syntax (Formula (..), Rltn (..), Term (..))
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  parserSpec

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
    it "parses an infix symbol predicate" $ do
      Parser.run "x < y" `shouldBeRight` Atom (Rltn "<" [Var "x", Var "y"])
    it "parses an infix non-symbol predicate" $ do
      Parser.run "x `and` y" `shouldBeRight` Atom (Rltn "and" [Var "x", Var "y"])
    -- Domain functions
    it "parses a prefix function" $ do
      Parser.run "f(g(x, y, z))" `shouldBeRight` Atom (Rltn "f" [Fn "g" [Var "x", Var "y", Var "z"]])
    it "parses an infix symbol function" $ do
      Parser.run "f(g(x, y, z))" `shouldBeRight` Atom (Rltn "f" [Fn "g" [Var "x", Var "y", Var "z"]])

x `shouldBeRight` y = x `shouldBe` Right y

x `shouldBeLeft` y = x `shouldBe` Left y