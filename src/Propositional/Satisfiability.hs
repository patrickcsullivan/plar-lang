module Propositional.Satisfiability where

import Propositional.Evaluate (eval')
import Propositional.Formula (allAtoms)
import Propositional.Valuation (allValuations)
import Syntax (Formula (..))

-- BRUTE FORCE

tautology :: Eq a => Formula a -> Bool
tautology frm = (all (eval' frm) . allValuations . allAtoms) frm

unsatisfiable :: Eq a => Formula a -> Bool
unsatisfiable frm = tautology (Not frm)

satisfiable :: Eq a => Formula a -> Bool
satisfiable frm = not (unsatisfiable frm)