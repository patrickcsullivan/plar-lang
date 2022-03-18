module Syntax where

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
  deriving (Eq, Ord, Show)