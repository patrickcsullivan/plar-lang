module PrettyPrint
  ( prettyPrint,
  )
where

import Syntax (Formula (..))

type Precidence = Int

-- instance Show Formula where
--   show = prettyPrint 0

prettyPrint :: Precidence -> Formula -> String
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

prettyPrintInfix :: Precidence -> String -> Precidence -> Formula -> Formula -> String
prettyPrintInfix opPrc opStr prc frmLeft frmRight =
  let s = prettyPrint (opPrc + 1) frmLeft ++ " " ++ opStr ++ " " ++ prettyPrint opPrc frmRight
   in if prc > opPrc
        then "(" ++ s ++ ")"
        else s

prettyPrintPrefix :: Precidence -> String -> Precidence -> Formula -> String
prettyPrintPrefix opPrc opStr prc frm =
  let s = opStr ++ " " ++ prettyPrint opPrc frm
   in if prc > opPrc
        then "(" ++ s ++ ")"
        else s
