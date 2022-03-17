{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Chapter1
  ( someFunc,
    Expression (..),
    simplify,
    testExpr,
    lex,
  )
where

import Prelude hiding (lex)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Expression
  = Var String
  | Const Int
  | Add Expression Expression
  | Mul Expression Expression
  deriving (Show)

data Error = Error String

simplify1 :: Expression -> Expression
simplify1 expr = case expr of
  Add (Const m) (Const n) -> Const (m + n)
  Mul (Const m) (Const n) -> Const (m * n)
  Add (Const 0) x -> x
  Add x (Const 0) -> x
  Mul (Const 0) x -> Const 0
  Mul x (Const 0) -> Const 0
  Mul (Const 1) x -> x
  Mul x (Const 1) -> x
  _ -> expr

simplify :: Expression -> Expression
simplify expression = case expression of
  Add e1 e2 -> simplify1 $ Add (simplify e1) (simplify e2)
  Mul e1 e2 -> simplify1 $ Mul (simplify e1) (simplify e2)
  _ -> simplify1 expression

testExpr :: Expression
testExpr = Add (Mul (Const 0) (Var "x")) (Var "y")

-- LEXER

space c = c `elem` " \t\n\r"

punctuation c = c `elem` "()[]{}"

symbolic c = c `elem` "~'!@#$%^&*-+=|\\:;<>.?/"

numeric c = c `elem` "0123456789"

alphanumeric c = c `elem` "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

lexWhile :: (Char -> Bool) -> String -> (String, String)
lexWhile = span

lex :: String -> [String]
lex input =
  case snd $ lexWhile space input of
    "" -> []
    c : cs ->
      let prop
            | alphanumeric c = alphanumeric
            | symbolic c = symbolic
            | otherwise = const False
       in let (toktl, rest) = lexWhile prop cs
           in (c : toktl) : lex rest

-- PARSER

parseAtom :: [String] -> Either Error Expression
parseAtom toks =
  case toks of
    [] -> Left $ Error "Expected an expression at end of input"
    _ -> undefined