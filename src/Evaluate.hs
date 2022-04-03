module Evaluate
  ( holds,
  )
where

import Evaluate.Interpretation (Interpretation)
import qualified Evaluate.Interpretation as I
import Evaluate.Valuation (Valuation, (|->))
import qualified Evaluate.Valuation as V
import Syntax (Formula (..), Rltn (..), Term (..))

-- | Evaluates the term in a given interpretation and valuation. Throws an error
-- if the term contains any variable names that are not defined in the
-- valuation or any function names that are not defined in the interpretation.
termVal :: Interpretation a -> Valuation a -> Term -> a
termVal m val x = case x of
  Var name -> V.lookup name val
  Fn name args -> fn argVals
    where
      fn = I.lookupFn name m
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
        pred = I.lookupPred name m
        argVals = termVal m val <$> args
    Not p -> not (holds m val p)
    And p q -> holds m val p && holds m val q
    Or p q -> holds m val p || holds m val q
    Imp p q -> not (holds m val p) || holds m val q
    Iff p q -> holds m val p == holds m val q
    ForAll x p -> all (\a -> holds m ((x |-> a) val) p) (I.domain m)
    Exists x p -> any (\a -> holds m ((x |-> a) val) p) (I.domain m)
