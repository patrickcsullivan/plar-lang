{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use $>" #-}
{-# HLINT ignore "Use <$>" #-}

module Chapter2 where

import Control.Applicative ()
import Control.Monad ()
import Text.ParserCombinators.Parsec (Parser, char, many, many1, noneOf, oneOf, parse, string, (<|>))

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
  deriving (Show)

-- matchTrue :: Parser String
-- matchTrue = string "true"

-- alwaysTrue :: Parser Bool
-- alwaysTrue = pure True -- applicative. return whatever i give.

alphaNumerics :: [Char]
alphaNumerics = "abcdefghijklmnopqrstuvwxyz_'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

spaces :: [Char]
spaces = " \t\n\r"

--

stringLiteral :: Parser String
stringLiteral = char '"' *> many (noneOf ['"']) <* char '"'

--

formulaTrue :: Parser (Formula a)
formulaTrue = lexeme (string "True") *> pure T

formulaFalse :: Parser (Formula a)
formulaFalse = lexeme (string "False") *> pure F

atomName :: Parser String
atomName = lexeme $ many1 (oneOf alphaNumerics)

formulaAtom :: Parser (Formula String)
formulaAtom = Atom <$> atomName

formulaNot :: Parser (Formula String)
formulaNot = Not <$> (lexeme (char '~') *> formula)

formulaAnd :: Parser (Formula String)
formulaAnd = do
  e1 <- formula
  lexeme $ string "/\\"
  e2 <- formula
  return $ And e1 e2

-- formulaOr :: Parser (Formula String)
-- formulaImp = undefined

-- formulaImp :: Parser (Formula String)
-- formulaImp = undefined

-- formulaIff :: Parser (Formula String)
-- formulaIff = do
--   e1 <-

formulaInner :: Parser (Formula String)
formulaInner = formulaTrue <|> formulaFalse <|> formulaAtom <|> formulaNot <|> formulaAnd

lexeme :: Parser a -> Parser a
lexeme p = p <* many (oneOf spaces)

formula :: Parser (Formula String)
formula = withParens <|> withoutParens
  where
    withParens = lexeme (char '(') *> formulaInner <* lexeme (char ')')
    withoutParens = formulaInner

-- parseTrue = parse matchTrue "a test parser"

-- parseAlwaysTrue = parse alwaysTrue "a test"