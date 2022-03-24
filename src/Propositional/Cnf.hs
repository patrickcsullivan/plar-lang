module Propositional.Cnf where

import Syntax (Formula (..))

-- Disjunctive and conjunctive normal forms

-- | Combine the formulas into an iterated conjunction.
listConj :: [Formula a] -> Formula a
listConj frms =
  case frms of
    [] -> T
    _ -> foldr1 And frms

-- | Combine the formulas into an iterated disjunction.
listDisj :: [Formula a] -> Formula a
listDisj frms =
  case frms of
    [] -> F
    _ -> foldr1 Or frms
