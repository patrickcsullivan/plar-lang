module Parser.Formula (formula) where

import Data.Maybe (fromMaybe)
import Syntax (Formula (..), Rltn (..), Term (..))
import Text.Parsec (char, many1, parserZero, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Token (GenTokenParser)

formula :: Parser a -> Parser (Formula a)
formula atomInner = Ex.buildExpressionParser formulaTable (formulaCase atomInner)

formulaTable =
  [ [Ex.Prefix (reservedOp "~" >> return Not)],
    [Ex.Infix (reservedOp "and" >> return And) Ex.AssocRight],
    [Ex.Infix (reservedOp "or" >> return Or) Ex.AssocRight],
    [Ex.Infix (reservedOp "==>" >> return Imp) Ex.AssocRight],
    [Ex.Infix (reservedOp "<=>" >> return Iff) Ex.AssocRight]
  ]

formulaCase :: Parser a -> Parser (Formula a)
formulaCase atomInner =
  try formulaTrue
    <|> try formulaFalse
    <|> try (formulaForAll atomInner)
    <|> try (formulaExists atomInner)
    <|> formulaAtom atomInner
    <|> parens (formula atomInner)

formulaAtom :: Parser a -> Parser (Formula a)
formulaAtom atomInner = do
  Atom <$> atomInner

formulaTrue :: Parser (Formula a)
formulaTrue = do
  reserved "True"
  return T

formulaFalse :: Parser (Formula a)
formulaFalse = do
  reserved "False"
  return F

formulaForAll :: Parser a -> Parser (Formula a)
formulaForAll atomInner = do
  reserved "forall"
  (v : vs) <- reverse <$> many1 identifier
  char '.'
  whiteSpace
  scope <- formula atomInner
  return $ foldl (flip ForAll) (ForAll v scope) vs

formulaExists :: Parser a -> Parser (Formula a)
formulaExists atomInner = do
  reserved "exists"
  (v : vs) <- reverse <$> many1 identifier
  char '.'
  whiteSpace
  scope <- formula atomInner
  return $ foldl (flip Exists) (Exists v scope) vs

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { -- Customer symbols are not allowed at the formula level, so we use
          -- the default definitions.
          Tok.reservedOpNames = ["~", "and", "or", "==>", "<=>"],
          Tok.reservedNames = ["True", "False", "forall", "exists"]
        }

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
