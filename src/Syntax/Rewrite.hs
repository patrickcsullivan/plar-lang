module Syntax.Rewrite where

import Syntax (Formula (..), Rltn)
import Syntax.Vars (freeVars)

-- | Convert the formula to prenex normal form (PNF), where all quantifiers
-- occor on the outside with a body (aka matrix) where only propositional
-- connectives are used.
pnf :: Formula Rltn -> Formula Rltn
pnf frm = undefined

-- | Put the formula in negation normal form (NNF) by repeatedly appling the De
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
    ForAll x p -> if x `elem` freeVars frm then frm else p
    Exists x p -> if x `elem` freeVars frm then frm else p
    _ -> frm