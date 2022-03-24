module Propositional.Formula (allAtoms, conjuncts, subst) where

import Data.List (nub)
import Syntax (Formula (..))

instance Functor Formula where
  fmap f frm =
    case frm of
      F -> F
      T -> T
      Atom a -> Atom $ f a
      Not frm -> Not (f <$> frm)
      And frm frm' -> And (f <$> frm) (f <$> frm')
      Or frm frm' -> Or (f <$> frm) (f <$> frm')
      Imp frm frm' -> Imp (f <$> frm) (f <$> frm')
      Iff frm frm' -> Iff (f <$> frm) (f <$> frm')
      ForAll s frm -> ForAll s (f <$> frm)
      Exists s frm -> Exists s (f <$> frm)

instance Foldable Formula where
  foldr f z frm =
    case frm of
      Atom a -> f a z
      Not frm -> foldr f z frm
      And frm frm' -> foldr f (foldr f z frm') frm
      Or frm frm' -> foldr f (foldr f z frm') frm
      Imp frm frm' -> foldr f (foldr f z frm') frm
      Iff frm frm' -> foldr f (foldr f z frm') frm
      ForAll _ frm -> foldr f z frm
      Exists _ frm -> foldr f z frm
      _ -> z

conjuncts :: Formula a -> [Formula a]
conjuncts frm =
  case frm of
    And p q -> conjuncts p ++ conjuncts q
    _ -> [frm]

allAtoms :: Eq a => Formula a -> [a]
allAtoms = nub . foldr (:) []

subst :: Eq a => a -> Formula a -> Formula a -> Formula a
subst atom new = onAtoms substIfMatch
  where
    substIfMatch a = if a == atom then new else Atom a

onAtoms :: (a -> Formula b) -> Formula a -> Formula b
onAtoms f frm =
  case frm of
    F -> F
    T -> T
    Atom a -> f a
    Not frm -> Not (onAtoms f frm)
    And frm frm' -> And (onAtoms f frm) (onAtoms f frm')
    Or frm frm' -> Or (onAtoms f frm) (onAtoms f frm')
    Imp frm frm' -> Imp (onAtoms f frm) (onAtoms f frm')
    Iff frm frm' -> Iff (onAtoms f frm) (onAtoms f frm')
    ForAll s frm -> ForAll s (onAtoms f frm)
    Exists s frm -> Exists s (onAtoms f frm)

dual :: Formula a -> Formula a
dual frm =
  case frm of
    F -> T
    T -> F
    Atom a -> Atom a
    Not p -> Not (dual p)
    And p q -> Or (dual p) (dual q)
    Or p q -> And (dual p) (dual q)
    _ -> undefined
