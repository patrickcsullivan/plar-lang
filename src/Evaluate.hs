module Evaluate (holds, boolInterp, modInterp) where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Syntax

data Interpretation a = Interpretation
  { -- | A finite, non-empty domain.
    domain :: [a],
    -- | A mapping from names to domain functions.
    fns :: Map String ([a] -> a),
    -- | A mapping from names to predicates (aka relations).
    preds :: Map String ([a] -> Bool)
  }

newtype Valuation a = Valuation
  { unValuation :: Map String a
  }

-- | Returns a function that substitutes the domain "object" `a` in for the
-- variable name `x` in the valuation.
(|->) :: String -> a -> (Valuation a -> Valuation a)
x |-> a = Valuation . Map.adjust (const a) x . unValuation

lookupVal :: String -> Valuation a -> a
lookupVal name (Valuation val) = fromJust $ Map.lookup name val

lookupFn :: String -> Interpretation a -> ([a] -> a)
lookupFn name (Interpretation _ fns _) = fromJust $ Map.lookup name fns

lookupPred :: String -> Interpretation a -> ([a] -> Bool)
lookupPred name (Interpretation _ _ preds) = fromJust $ Map.lookup name preds

termVal :: Interpretation a -> Valuation a -> Term -> a
termVal m val x = case x of
  Var name -> lookupVal name val
  Fn name args -> fn argVals
    where
      fn = lookupFn name m
      argVals = termVal m val <$> args

holds :: Interpretation a -> Valuation a -> Formula Rltn -> Bool
holds m val fm =
  case fm of
    F -> False
    T -> True
    Atom (Rltn name args) -> pred argVals
      where
        pred = lookupPred name m
        argVals = termVal m val <$> args
    Not p -> not (holds m val p)
    And p q -> holds m val p && holds m val q
    Or p q -> holds m val p || holds m val q
    Imp p q -> not (holds m val p) || holds m val q
    Iff p q -> holds m val p == holds m val q
    ForAll x p -> all (\a -> holds m ((x |-> a) val) p) (domain m)
    Exists x p -> any (\a -> holds m ((x |-> a) val) p) (domain m)

boolInterp :: Interpretation Bool
boolInterp =
  Interpretation
    { domain = [False, True],
      fns =
        Map.fromList
          [ ("0", \[] -> False),
            ("1", \[] -> True),
            ("+", \[x, y] -> x || y),
            ("*", \[x, y] -> x && y)
          ],
      preds =
        Map.fromList
          [ ("fEq", \[x, y] -> x == y)
          ]
    }

modInterp :: Int -> Interpretation Int
modInterp n =
  Interpretation
    { domain = [0 .. n -1],
      fns =
        Map.fromList
          [ ("0", \[] -> 0),
            ("1", \[] -> 1),
            ("+", \[x, y] -> (x + y) `mod` n),
            ("*", \[x, y] -> (x * y) `mod` n)
          ],
      preds =
        Map.fromList
          [ ("fEq", \[x, y] -> x == y)
          ]
    }
