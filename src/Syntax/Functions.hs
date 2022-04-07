module Syntax.Functions
  ( functions,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (Formula (..), Rltn (..), Term (..))

-- | Domain function name.
type FnName = String

-- | Domain function arity.
type FnArity = Int

-- | Return the set of domain functions in the formula.
functions :: Formula -> Set (FnName, FnArity)
functions =
  foldAtoms
    ( \(Rltn _ args) fns ->
        fns `Set.union` Set.unions (trmFns <$> args)
    )
    Set.empty

-- | Return the set of domain functions in the term.
trmFns :: Term -> Set (FnName, FnArity)
trmFns trm = case trm of
  Var s -> Set.empty
  Fn name args -> Set.unions $ Set.singleton (name, length args) : (trmFns <$> args)

-- | Fold the atoms in the formula.
foldAtoms :: (Rltn -> b -> b) -> b -> Formula -> b
foldAtoms f z frm = case frm of
  F -> z
  T -> z
  Atom rltn -> f rltn z
  Not p -> foldAtoms f z p
  And p q -> foldAtoms f (foldAtoms f z p) q
  Or p q -> foldAtoms f (foldAtoms f z p) q
  Imp p q -> foldAtoms f (foldAtoms f z p) q
  Iff p q -> foldAtoms f (foldAtoms f z p) q
  ForAll x p -> foldAtoms f z p
  Exists x p -> foldAtoms f z p