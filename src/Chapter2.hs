module Chapter2 where

import Data.List (nub)
import Syntax

conjuncts :: Formula a -> [Formula a]
conjuncts frm =
  case frm of
    And p q -> conjuncts p ++ conjuncts q
    _ -> [frm]

atoms :: Eq a => Formula a -> [a]
atoms = foldr (:) []

orError :: Maybe b -> a -> Either a b
orError Nothing a = Left a
orError (Just b) _ = Right b

allValuations :: Eq a => [a] -> [[(a, Bool)]]
allValuations atoms =
  let options = [True, False] <$ atoms
      combinations = sequenceA options
   in zip atoms <$> combinations

eval :: Eq a => Formula a -> [(a, Bool)] -> Either a Bool
eval frm vals =
  case frm of
    F -> return False
    T -> return True
    Atom a ->
      case lookup a vals of
        Nothing -> Left a
        (Just b) -> return b
    Not frm -> not <$> eval frm vals
    And frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (b1 && b2)
    Or frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (b1 || b2)
    Imp frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (not b1 || b2)
    Iff frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (b1 == b2)
    ForAll _ _ -> undefined
    Exists _ _ -> undefined

imp b1 b2 = not b1 || b2

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