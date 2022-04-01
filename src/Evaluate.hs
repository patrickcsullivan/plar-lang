module Evaluate (holds, emptyValuation, Interpretation (..), Valuation (..)) where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Syntax (Formula (..), Rltn (..), Term (..))

-- | Specifies the meaning of function symbols and predicate symbols in first-
-- order logic.
data Interpretation a = Interpretation
  { -- | A finite, non-empty domain.
    domain :: [a],
    -- | A mapping from names to domain functions.
    fns :: Map String ([a] -> a),
    -- | A mapping from names to predicates (aka relations).
    preds :: Map String ([a] -> Bool)
  }

-- | Specifies the meaning of variables by mapping each variable to a domain
-- "object".
newtype Valuation a = Valuation
  { unValuation :: Map String a
  }
  deriving (Show)

-- | Returns an empty valuation.
emptyValuation :: Valuation a
emptyValuation = Valuation Map.empty

-- | Returns a function that updates the the domain "object" associated with the
-- variable name `x` in the valuation.
(|->) :: String -> a -> (Valuation a -> Valuation a)
x |-> a = Valuation . Map.insert x a . unValuation

-- | Returns the domain "object" associated with the given variable name in the
-- valuation. Throws an error if the variable name does not exist in the
-- valuation.
lookupVal :: String -> Valuation a -> a
lookupVal name (Valuation val) =
  case Map.lookup name val of
    Just v -> v
    _ -> error $ "domain variable `" ++ name ++ "` not defined"

-- | Returns the domain function associated with the given function name in the
-- interpretation. Throws an error if the function name does not exist in the
-- interpretation.
lookupFn :: String -> Interpretation a -> ([a] -> a)
lookupFn name (Interpretation _ fns _) =
  case Map.lookup name fns of
    Just fn -> fn
    _ -> error $ "domain funtion `" ++ name ++ "` not defined"

-- | Returns the predicate (aka relation) associated with the given predicate
-- name in the interpretation. Throws an error if the predicate name does not
-- exist in the interpretation.
lookupPred :: String -> Interpretation a -> ([a] -> Bool)
lookupPred name (Interpretation _ _ preds) =
  case Map.lookup name preds of
    Just rltn -> rltn
    _ -> error $ "relation `" ++ name ++ "` not defined"

-- | Evaluates the term in a given interpretation and valuation. Throws an error
-- if the term contains any variable names that are not defined in the
-- valuation or any function names that are not defined in the interpretation.
termVal :: Interpretation a -> Valuation a -> Term -> a
termVal m val x = case x of
  Var name -> lookupVal name val
  Fn name args -> fn argVals
    where
      fn = lookupFn name m
      argVals = termVal m val <$> args

-- | Checks if the formula holds (ie, is true) in the given interpretation and
-- valuation.
--
-- Throws an error if the formula contains any variable names that are not
-- defined in the valuation or any function or predicate (aka relation) names
-- that are not defined in the interpretation
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
