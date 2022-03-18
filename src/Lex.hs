module Lex
  ( lex,
    isSpace,
    isPunctuation,
    isSymbol,
    isNumeric,
    isAlphanumeric,
  )
where

import Prelude hiding (lex)

type Token = String

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r"

isPunctuation :: Char -> Bool
isPunctuation c = c `elem` "()[]{}"

isSymbol :: Char -> Bool
isSymbol c = c `elem` "~'!@#$%^&*-+=|\\:;<>.?/"

isNumeric :: Char -> Bool
isNumeric c = c `elem` "0123456789"

isAlphanumeric :: Char -> Bool
isAlphanumeric c = c `elem` "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

lexWhile :: (Char -> Bool) -> String -> (Token, String)
lexWhile = span

lex :: String -> [Token]
lex input =
  case snd $ lexWhile isSpace input of
    "" -> []
    c : cs ->
      let prop
            | isAlphanumeric c = isAlphanumeric
            | isSymbol c = isSymbol
            | otherwise = const False
       in let (toktl, rest) = lexWhile prop cs
           in (c : toktl) : lex rest
