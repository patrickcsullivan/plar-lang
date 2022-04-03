module Syntax.Substitution
  ( subst,
    termSubst,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (Formula (..), Rltn (..), Term (..))
import Syntax.Instantiation (Instantiation, (|->))
import qualified Syntax.Instantiation as I
import Syntax.Vars (freeVars, termVars, variant)

-- | Substitutes terms from the instantiation for free variables in the given
-- formula. If a variable in the given formula is not in the instantiation then
-- it is left unchanged. Bound varibles in the formula are renamed as needed to
-- avoid clashing with variables in the substituted terms.
subst :: Instantiation -> Formula Rltn -> Formula Rltn
subst inst frm =
  case frm of
    F -> F
    T -> T
    Atom (Rltn name args) -> Atom (Rltn name (termSubst inst <$> args))
    Not p -> Not (subst inst p)
    And p q -> And (subst inst p) (subst inst q)
    Or p q -> Or (subst inst p) (subst inst q)
    Imp p q -> Imp (subst inst p) (subst inst q)
    Iff p q -> Iff (subst inst p) (subst inst q)
    ForAll x p -> substQ inst ForAll x p
    Exists x p -> substQ inst Exists x p

-- | Substitute terms from the instantiation for variables in the given term. If
-- a variable in the given term is not in the instantiation then it is left
-- unchanged.
termSubst :: Instantiation -> Term -> Term
termSubst inst trm =
  case trm of
    Var name -> case I.lookup name inst of
      Nothing -> Var name -- The variable has no substitute, so leave it alone.
      Just subst -> subst
    Fn name args -> Fn name (termSubst inst <$> args)

-- | Substitutes terms from the instantiation for free variables in the
-- quantified formula. The variable binding is renamed as needed to avoid
-- clashing with variables in the subsituted terms.
substQ ::
  -- | The instantion containing substitions.
  Instantiation ->
  -- | Function for recreating the quantified formula into which terms are
  -- substituted.
  (String -> Formula Rltn -> Formula Rltn) ->
  -- | The variable binding of the quantified formula into which terms are
  -- substituted.
  String ->
  -- | The scope of the quantified formula into which terms are substituted.
  Formula Rltn ->
  Formula Rltn
substQ inst mkQuant x p =
  let -- Is there a free var, y, in p such that when substitution is applied to
      -- y, x appears in the free variables of y's substitute?
      pFreeVars = x `Set.delete` freeVars p
      isClash = any (\y -> x `elem` termVars (inst `termSubst` Var y)) pFreeVars
      -- Come up with a new name for the bound variable that doesn't clash with any of the results of
      -- substituting in p.
      x' =
        if isClash
          then
            let -- TODO: Come back to this. Seems questionable or at least unobvious.
                avoidVars = freeVars $ subst (x `I.removeVar` inst) p
             in variant x avoidVars
          else x
      -- Update the instantiation so that it will rename all instances of the
      -- bound variable x to x', avoiding any clashes with appearances of the
      -- free var x in terms in the instantiation.
      inst' = (x |-> Var x') inst
      p' = subst ((x |-> Var x') inst) p
   in mkQuant x' p'
