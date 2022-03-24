module Propositional.Nnf (nenfSimplify, nnfSimplify) where

import Syntax (Formula (..))

nnfSimplify :: Formula a -> Formula a
nnfSimplify = nnf . simplify

nenfSimplify :: Formula a -> Formula a
nenfSimplify = nenf . simplify

-- | Put the formula in negation normal form (NNF) by repeatedly appling the De
-- Morgan laws and the law of double negation to push negations down to the
-- atomic forumulas.
nnf :: Formula a -> Formula a
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
    _ -> frm

nenf :: Formula a -> Formula a
nenf frm =
  case frm of
    Not (Not p) -> nenf p
    Not (And p q) -> Or (nenf $ Not p) (nenf $ Not q)
    Not (Or p q) -> And (nenf $ Not p) (nenf $ Not q)
    Not (Imp p q) -> And (nenf p) (nenf $ Not q)
    Not (Iff p q) -> Iff (nenf p) (nenf q)
    And p q -> And (nenf p) (nenf q)
    Or p q -> Or (nenf p) (nenf q)
    Imp p q -> Or (nenf $ Not p) (nenf q)
    Iff p q -> Iff (nenf p) (nenf q)
    _ -> frm

-- | Simplify the formula to any propositional constants from the top level of
-- the formula.
simplify1 :: Formula a -> Formula a
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
    -------
    Imp F p -> T
    Imp p T -> T
    Imp T p -> p
    Imp p F -> Not p -- must come at the end of this group sof F => F will simplify to T instead of ~F
    -------
    Iff p T -> p
    Iff T p -> p
    Iff p F -> Not p -- must come at the end of this group sof T <=> F will simplify to F instead of ~T
    Iff F p -> Not p -- must come at the end of this group sof F <=> T will simplify to F instead of ~T
    -------
    _ -> frm

simplify :: Formula a -> Formula a
simplify frm =
  case frm of
    Not p -> simplify1 $ Not (simplify p)
    And p q -> simplify1 $ And (simplify p) (simplify q)
    Or p q -> simplify1 $ Or (simplify p) (simplify q)
    Imp p q -> simplify1 $ Imp (simplify p) (simplify q)
    Iff p q -> simplify1 $ Iff (simplify p) (simplify q)
    _ -> frm

isNegativeLiteral :: Formula a -> Bool
isNegativeLiteral frm =
  case frm of
    Not (Atom _) -> True
    _ -> False

isPositiveLiteral :: Formula a -> Bool
isPositiveLiteral frm =
  case frm of
    Atom _ -> True
    _ -> False

negateLiteral :: Formula a -> Formula a
negateLiteral frm =
  case frm of
    Atom a -> Not (Atom a)
    Not (Atom a) -> Atom a
    _ -> frm
