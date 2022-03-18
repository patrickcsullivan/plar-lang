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