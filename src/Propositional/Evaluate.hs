module Propositional.Evaluate (eval, eval') where

import Syntax (Formula (..))

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

eval' :: Eq a => Formula a -> [(a, Bool)] -> Bool
eval' frm vals = case eval frm vals of
  Right b -> b
  Left _ -> undefined