module Syntax.Rewrite
  ( nnf,
    pnf,
    prenex,
    simplify,
  )
where

import AndOr (AndOr (..), leftElseRight, leftToMaybe, rightToMaybe)
import qualified Debug.Trace as Debug
import Syntax (Formula (..), Rltn, Term (..))
import Syntax.Instantiation ((|=>))
import Syntax.Substitution (subst)
import Syntax.Vars (freeVars, variant)
import Prelude hiding (Either (..))

-- | Converts the formula to prenex normal form (PNF), where all quantifiers
-- occor on the outside with a body (aka matrix) where only propositional
-- connectives are used.
pnf :: Formula Rltn -> Formula Rltn
pnf = prenex . nnf . simplify

-- | Recursviely pulls all quantifiers out to the top of the formula.
prenex :: Formula Rltn -> Formula Rltn
prenex frm =
  case frm of
    ForAll x p -> ForAll x (prenex p)
    Exists x p -> Exists x (prenex p)
    And p q -> pullQs $ And (prenex p) (prenex q)
    Or p q -> pullQs $ Or (prenex p) (prenex q)
    _ -> frm

-- | If the given formula contains a connective at its top level then this
-- function will pull the top-most series of consecutive quantifiers out of each
-- side of the connective.
pullQs :: Formula Rltn -> Formula Rltn
pullQs frm =
  case frm of
    And (ForAll x p) (ForAll y q) -> pullQsFromConnective (LeftRight x y) frm ForAll And p q
    Or (Exists x p) (Exists y q) -> pullQsFromConnective (LeftRight x y) frm Exists Or p q
    And (ForAll x p) q -> pullQsFromConnective (Left x) frm ForAll And p q
    And p (ForAll y q) -> pullQsFromConnective (Right y) frm ForAll And p q
    Or (ForAll x p) q -> pullQsFromConnective (Left x) frm ForAll Or p q
    Or p (ForAll y q) -> pullQsFromConnective (Right y) frm ForAll Or p q
    And (Exists x p) q -> pullQsFromConnective (Left x) frm Exists And p q
    And p (Exists y q) -> pullQsFromConnective (Right y) frm Exists And p q
    Or (Exists x p) q -> pullQsFromConnective (Left x) frm Exists Or p q
    Or p (Exists y q) -> pullQsFromConnective (Right y) frm Exists Or p q
    _ -> frm

-- | Recursively pulls quantifiers out of the formulas on each side of a
-- connective formula. When one or both sides of the connective are quantified
-- renames the variable binding as needed to a new name that does not clash with
-- free variables on either side of the connective.
pullQsFromConnective ::
  -- | Indicates whether the left side, the right side, or both sides of the
  -- connective are quantified. Contains the variable binding for each
  -- quantified side.
  AndOr String String ->
  -- | The connective formula from which to pull out quantifiers.
  Formula Rltn ->
  -- | Function for creating the quantifier that is pulled out.
  (String -> Formula Rltn -> Formula Rltn) ->
  -- | Function for creating the connective that is at the top level of the
  -- formula.
  (Formula Rltn -> Formula Rltn -> Formula Rltn) ->
  -- | Formula from the left side of the connective, excluding the top-most
  -- quantifier if it was quantified.
  Formula Rltn ->
  -- | Formula from the right side of the connective, excluding the top-most
  -- quantifier if it was quantified.
  Formula Rltn ->
  Formula Rltn
pullQsFromConnective varBindings frm mkQ mkConnective p q =
  let z = variant (leftElseRight varBindings) (freeVars frm)
      p' = case leftToMaybe varBindings of
        Just x -> subst (x |=> Var z) p
        _ -> p
      q' = case rightToMaybe varBindings of
        Just y -> subst (y |=> Var z) q
        _ -> q
   in mkQ z (pullQs $ mkConnective p' q')

-- | Puts the formula in negation normal form (NNF) by repeatedly appling the De
-- Morgan laws, the law of double negation, and "infinite De Morgan laws" on
-- quantifiers to push negations down to the atomic forumulas.
nnf :: Formula Rltn -> Formula Rltn
nnf frm =
  case frm of
    And p q -> And (nnf p) (nnf q)
    Or p q -> Or (nnf p) (nnf q)
    Imp p q -> Or (nnf $ Not p) (nnf q)
    Iff p q -> Or (And (nnf p) (nnf q)) (And (nnf $ Not p) (nnf $ Not q))
    Not (Not p) -> nnf p
    Not (And p q) -> Or (nnf $ Not p) (nnf $ Not q)
    Not (Or p q) -> And (nnf $ Not p) (nnf $ Not q)
    Not (Imp p q) -> And (nnf p) (nnf $ Not q)
    Not (Iff p q) -> Or (And (nnf p) (nnf $ Not q)) (And (nnf $ Not p) (nnf q))
    ForAll x p -> ForAll x (nnf p)
    Exists x p -> Exists x (nnf p)
    Not (ForAll x p) -> Exists x (nnf $ Not p)
    Not (Exists x p) -> ForAll x (nnf $ Not p)
    _ -> frm

-- | Recursively simplifies the formula. Simplifies connectives with `True` and
-- `False`. Removes double negation. Removes quantification if it is vacuous
-- (ie: the quantified variable does not appear free in the context).
simplify :: Formula Rltn -> Formula Rltn
simplify frm = case frm of
  Not p -> simplify1 $ Not (simplify1 p)
  And p q -> simplify1 $ And (simplify p) (simplify q)
  Or p q -> simplify1 $ Or (simplify p) (simplify q)
  Imp p q -> simplify1 $ Imp (simplify p) (simplify q)
  Iff p q -> simplify1 $ Iff (simplify p) (simplify q)
  ForAll x p -> simplify1 $ ForAll x (simplify p)
  Exists x p -> simplify1 $ Exists x (simplify p)
  _ -> frm

inspect msg val = (Debug.trace $ msg ++ "\n" ++ show val) val

-- | Simplifies the formula at the top level of the AST. Simplifies connectives
-- with `True` and `False`. Removes double negation. Removes quantification if
-- it is vacuous (ie: the quantified variable does not appear free in the
-- context).
simplify1 :: Formula Rltn -> Formula Rltn
simplify1 frm =
  case frm of
    Not F -> T
    Not T -> F
    Not (Not p) -> p
    And p F -> F
    And F p -> F
    And p T -> p
    And T p -> p
    Or p F -> p
    Or F p -> p
    Or p T -> T
    Or T p -> T
    Imp F p -> T
    Imp p T -> T
    Imp T p -> p
    -- Must come after `Imp F p` pattern match or else this case would match
    -- `Imp F F` and would simplify to `Not F` instead of `T`.
    -- Must come after `Imp T p` pattern match or else this case would match
    -- `Imp T F` and would simplify to `Not T` instead of `F`.
    Imp p F -> Not p
    Iff p T -> p
    Iff T p -> p
    -- Must come after `Iff T p` pattern match or else this case would match
    -- `Iff T F` and would simplify to `Not T` instead of `F`.
    Iff p F -> Not p
    -- Must come after `Iff p T` pattern match or else this case would match
    -- `Iff F T` and would simplify to `Not T` instead of `F`.
    Iff F p -> Not p
    ForAll x p -> if x `elem` freeVars p then frm else p
    Exists x p -> if x `elem` freeVars p then frm else p
    _ -> frm