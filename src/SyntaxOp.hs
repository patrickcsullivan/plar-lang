-- | Operators on the abstract syntax tree.
module SyntaxOp
  ( Instantiation,
    (|->),
    (|=>),
    subst,
    termSubst,
    termVars,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (Formula (..), Rltn (..), Term (..))

-- | A variable assignment that maps variables to terms.
type Instantiation = Map String Term

-- | Returns a function that adds the given variable-to-term mapping to the
-- instantiation.
(|->) :: String -> Term -> Instantiation -> Instantiation
x |-> a = Map.insert x a

-- | Returns an instantiation containing a single variable-to-term mapping.
(|=>) :: String -> Term -> Instantiation
x |=> a = Map.singleton x a

-- | Substitute terms from the instantiation for variables in the given term. If
-- a variable in the given term is not in the instantiation then it is left
-- unchanged.
termSubst :: Instantiation -> Term -> Term
termSubst inst trm =
  case trm of
    Var name -> case Map.lookup name inst of
      Nothing -> Var name -- The variable has no substitute, so leave it alone.
      Just subst -> subst
    Fn name args -> Fn name (termSubst inst <$> args)

applyToTerm :: Instantiation -> Term -> Term
applyToTerm = termSubst

removeVar :: String -> Instantiation -> Instantiation
removeVar = Map.delete

--------------------------------------------------------------------------------
-- TERM OPERATORS

-- | Returns the set of domain variables in the term.
termVars :: Term -> Set String
termVars trm =
  case trm of
    Var name -> Set.singleton name
    Fn _ args -> Set.unions (termVars <$> args)

--------------------------------------------------------------------------------
-- FORMULA OPERATORS

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
freeVars frm =
  case frm of
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

-- | Returns a variant of the given variable name that is distinct from the list
-- of variable names. Returns the variable name unchanged if it is not in the
-- list of variable names.
variant :: String -> Set String -> String
variant x vars =
  if x `elem` vars
    then variant (x ++ "'") vars
    else x

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

-- | Substitutes terms from the instantiation for free variables in the
-- quantified formula. The variable binding is renamed as needed to avoid
-- clashing with variables in the subsituted terms.
substQ ::
  -- | The instantion containing substitions.
  Instantiation ->
  -- | A function for recreating the quantified formula into which terms are
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
      isClash = any (\y -> x `elem` termVars (inst `applyToTerm` Var y)) pFreeVars
      -- Come up with a new name for the bound variable that doesn't clash with any of the results of
      -- substituting in p.
      x' =
        if isClash
          then
            let -- TODO: Come back to this. Seems questionable or at least unobvious.
                avoidVars = freeVars $ subst (x `removeVar` inst) p
             in variant x avoidVars
          else x
      -- Update the instantiation so that it will rename all instances of the
      -- bound variable x to x', avoiding any clashes with appearances of the
      -- free var x in terms in the instantiation.
      inst' = (x |-> Var x') inst
      p' = subst ((x |-> Var x') inst) p
   in mkQuant x' p'