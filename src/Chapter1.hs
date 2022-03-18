module Chapter1
  ( someFunc,
    Expression (..),
    simplify,
    testExpr,
    testStr,
    defaultParser,
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

instance Show Expression where
  show = prettyPrint 0

newtype Error = Error String deriving (Show)

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

testStr :: String
testStr = "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)"

-- PARSER

type Token = String

type TokenParser = [Token] -> Either Error (Expression, [Token])

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

parseExpression :: TokenParser
parseExpression toks = do
  (e1, toks1) <- parseProduct toks
  case toks1 of
    "+" : toks1 -> do
      (e2, toks2) <- parseExpression toks1
      return (Add e1 e2, toks2)
    _ -> return (e1, toks1)

parseProduct :: TokenParser
parseProduct toks = do
  (e1, toks1) <- parseAtom toks
  case toks1 of
    "*" : toks1 -> do
      (e2, toks2) <- parseProduct toks1
      return (Mul e1 e2, toks2)
    _ -> return (e1, toks1)

parseAtom :: TokenParser
parseAtom toks =
  case toks of
    [] -> Left $ Error "Expected an expression at end of input"
    "(" : toks1 -> do
      (e2, toks2) <- parseExpression toks1
      case toks2 of
        ")" : toks2 -> return (e2, toks2)
        _ -> Left $ Error "Expected closing bracket"
    tok : toks1 ->
      if all isNumeric tok
        then Right (Const (read tok), toks1)
        else Right (Var tok, toks1)

makeParser :: TokenParser -> String -> Either Error Expression
makeParser parse input = do
  let toks = lex input
  (expr, toksRest) <- parse toks
  case toksRest of
    [] -> return expr
    _ -> Left $ Error "Unparsed input"

defaultParser :: String -> Either Error Expression
defaultParser = makeParser parseExpression

-- PRETTY PRINTER

prettyPrint :: Int -> Expression -> String
prettyPrint precidence expression =
  case expression of
    Var s -> s
    Const n -> show n
    Add e1 e2 ->
      let s = prettyPrint 3 e1 ++ " + " ++ prettyPrint 2 e2
       in if precidence > 2
            then "(" ++ s ++ ")"
            else s
    Mul e1 e2 ->
      let s = prettyPrint 5 e1 ++ " * " ++ prettyPrint 4 e2
       in if precidence > 4
            then "(" ++ s ++ ")"
            else s
