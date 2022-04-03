module Syntax.Skolemize (skolemize) where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (Formula (..), Rltn (..), Term (..))
import Syntax.Instantiation ((|=>))
import Syntax.Name (variant)
import Syntax.Rewrite
import Syntax.Substitution (subst)
import Syntax.Vars (freeVars)

-- | Domain function name.
type FnName = String

-- | Domain function arity.
type FnArity = Int

-- | Simplify the formula, put it in PNF, and remove all quantifiers (by
-- Skolemizing the formula and removing any existential quantifiers). The
-- resulting formula is equisatisfiable with the input formula.
skolemize :: Formula -> Formula
skolemize = specialize . pnf . aSkolemize

-- | Remove universal quantifier from the top of the formula. The resulting
-- formula is equisatisfiable with the input formula.
specialize :: Formula -> Formula
specialize frm =
  case frm of
    ForAll x p -> specialize p
    _ -> frm

-- | Transform the formula into NNF and Skolemize it so that all negations are
-- pushed down to atoms, all implication and equivalence connectives are
-- removed, and all existential quantifiers are removed. The resulting formula
-- is equisatisfiable with the input formula.
aSkolemize :: Formula -> Formula
aSkolemize frm =
  let avoidNames = Set.map fst (frmFns frm)
      (frm', _) = skolemizeNnf (nnf $ simplify frm) avoidNames
   in frm'

-- | Skolemize a forumla that is in NNF.
skolemizeNnf :: Formula -> Set FnName -> (Formula, Set FnName)
skolemizeNnf frm avoidNames =
  case frm of
    Exists y p ->
      let free = freeVars frm
          tryName =
            if Set.null free
              then "c_" ++ y
              else "f_" ++ y
          name = variant tryName avoidNames
          fn = Fn name (Var <$> Set.toAscList free)
          frm' = subst (y |=> fn) p
       in skolemizeNnf frm' (name `Set.insert` avoidNames)
    ForAll x p ->
      let (p', avoidNames') = skolemizeNnf p avoidNames
       in (ForAll x p', avoidNames')
    And p q -> skolemizeNnf2 And p q avoidNames
    Or p q -> skolemizeNnf2 Or p q avoidNames
    _ -> (frm, avoidNames)

-- | Skolemize each side of a connective formula that is in NNF.
skolemizeNnf2 :: (Formula -> Formula -> Formula) -> Formula -> Formula -> Set FnName -> (Formula, Set FnName)
skolemizeNnf2 mkConn p q avoidNames =
  let (p', avoidNames') = skolemizeNnf p avoidNames
      -- When skolemizing the right side of the connective, be careful to avoid
      -- function names introduced while skolemizing the left side.
      (q', avoidNames'') = skolemizeNnf q avoidNames'
   in (mkConn p' q', avoidNames'')

-- | Return the set of domain functions in the formula.
frmFns :: Formula -> Set (FnName, FnArity)
frmFns =
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