module Syntax where

newtype Prop = Prop String deriving (Eq, Ord)

instance Show Prop where
  show (Prop name) = name

data Formula a
  = F
  | T
  | Atom a
  | Not (Formula a)
  | And (Formula a) (Formula a)
  | Or (Formula a) (Formula a)
  | Imp (Formula a) (Formula a)
  | Iff (Formula a) (Formula a)
  | ForAll String (Formula a)
  | Exists String (Formula a)
  deriving (Eq, Ord)

instance Show a => Show (Formula a) where
  show = prettyPrint 0

type Precidence = Int

prettyPrint :: Show a => Precidence -> Formula a -> String
prettyPrint prc formula = case formula of
  F -> "False"
  T -> "True"
  Atom a -> show a
  Exists s frm -> undefined
  ForAll s frm -> undefined
  Iff frm frm' -> prettyPrintInfix 2 "<=>" prc frm frm'
  Imp frm frm' -> prettyPrintInfix 3 "==>" prc frm frm'
  Or frm frm' -> prettyPrintInfix 4 "or" prc frm frm'
  And frm frm' -> prettyPrintInfix 5 "and" prc frm frm'
  Not frm -> prettyPrintPrefix 6 "~" prc frm

prettyPrintInfix :: Show a => Precidence -> String -> Precidence -> Formula a -> Formula a -> String
prettyPrintInfix opPrc opStr prc frmLeft frmRight =
  let s = prettyPrint (opPrc + 1) frmLeft ++ " " ++ opStr ++ " " ++ prettyPrint opPrc frmRight
   in if prc > opPrc
        then "(" ++ s ++ ")"
        else s

prettyPrintPrefix :: Show a => Precidence -> String -> Precidence -> Formula a -> String
prettyPrintPrefix opPrc opStr prc frm =
  let s = opStr ++ " " ++ prettyPrint opPrc frm
   in if prc > opPrc
        then "(" ++ s ++ ")"
        else s

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