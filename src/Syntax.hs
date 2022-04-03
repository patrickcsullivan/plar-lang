module Syntax where

-- | A propositional formula which is intended to be true or false.
data Formula
  = -- | False.
    F
  | -- | True.
    T
  | -- | An atomic formula that contains no propositional connectives.
    Atom Rltn
  | -- | Negation.
    Not Formula
  | -- | Conjunction.
    And Formula Formula
  | -- | Disjunction.
    Or Formula Formula
  | -- | Implication.
    Imp Formula Formula
  | Iff Formula Formula
  | -- | Univerally quantified propositional formula. Takes a variable binding and a scope.
    ForAll String Formula
  | -- | Existentially quantified propositional formula. Takes a variable binding and a scope.
    Exists String Formula
  deriving (Eq, Ord, Show)

-- | A named relation (aka predicate) which is intended to be true or false but
-- is built up from non-propositional variables and constants using functions
-- and predicates.
--
-- A function that maps the "object" domain to Boolean values. A
-- relation/predicate with zero arguments correponds to a simple propositional
-- variable.
data Rltn = Rltn String [Term] deriving (Eq, Ord, Show)

-- | A first-order term which is intended to denote an "object" in the domain
-- being reasoned about. A term can be built up from "object"-denoting variables
-- using functions.
data Term
  = -- | An "object"-denoting variable.
    Var String
  | -- | A function whose domain and range are in the "object" domain.
    Fn String [Term]
  deriving (Eq, Ord, Show)