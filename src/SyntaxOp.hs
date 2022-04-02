-- | Operators on the abstract syntax tree.
module SyntaxOp (termSubst, termVars) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (Formula (..), Rltn (..), Term (..))

-- | Returns the set of domain variables in the term.
termVars :: Term -> Set String
termVars trm =
  case trm of
    Var name -> Set.singleton name
    Fn _ args -> Set.unions (termVars <$> args)

-- | Returns the set of all domain variables in the formula.
vars :: Formula Rltn -> Set String
vars frm = case frm of
  F -> Set.empty
  T -> Set.empty
  Atom (Rltn _ args) -> Set.unions (termVars <$> args)
  Not p -> vars p
  And p q -> vars p `Set.union` vars q
  Or p q -> vars p `Set.union` vars q
  Imp p q -> vars p `Set.union` vars q
  Iff p q -> vars p `Set.union` vars q
  ForAll x p -> Set.insert x (vars p)
  Exists x p -> Set.insert x (vars p)

-- | Returns the set of free domain variables in the formula.
freeVars :: Formula Rltn -> Set String
freeVars frm = case frm of
  F -> Set.empty
  T -> Set.empty
  Atom (Rltn _ args) -> Set.unions (termVars <$> args)
  Not p -> freeVars p
  And p q -> freeVars p `Set.union` freeVars q
  Or p q -> freeVars p `Set.union` freeVars q
  Imp p q -> freeVars p `Set.union` freeVars q
  Iff p q -> freeVars p `Set.union` freeVars q
  ForAll x p -> Set.delete x (freeVars p)
  Exists x p -> Set.delete x (freeVars p)

-- | A variable assignment that maps variables to terms.
type Instantiation = Map String Term

-- | Substitute terms from the instantiation for variables in the given term. If
-- a variable in the given term is not in the instantiation then it is left
-- unchanged.
termSubst :: Instantiation -> Term -> Term
termSubst inst trm = case trm of
  Var name -> case Map.lookup name inst of
    Nothing -> Var name -- The variable has no substitute, so leave it alone.
    Just subst -> subst
  Fn name args -> Fn name (termSubst inst <$> args)