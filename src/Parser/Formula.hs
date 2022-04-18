module Parser.Formula (formula) where

import Data.Maybe (fromMaybe)
import Parser.Rltn (rltn)
import Syntax (Formula (..), Rltn (..), Term (..))
import Text.Parsec (alphaNum, char, many1, parserZero, try, upper, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Token (GenTokenParser)

formula :: Parser Formula
formula = Ex.buildExpressionParser formulaTable formulaCase

formulaTable =
  [ [Ex.Prefix (reservedOp "~" >> return Not)],
    [Ex.Infix (reservedOp "and" >> return And) Ex.AssocRight],
    [Ex.Infix (reservedOp "or" >> return Or) Ex.AssocRight],
    [Ex.Infix (reservedOp "==>" >> return Imp) Ex.AssocRight],
    [Ex.Infix (reservedOp "<=>" >> return Iff) Ex.AssocRight]
  ]

formulaCase :: Parser Formula
formulaCase =
  try formulaTrue
    <|> try formulaFalse
    <|> try formulaForAll
    <|> try formulaExists
    <|> formulaAtom
    <|> parens formula

formulaAtom :: Parser Formula
formulaAtom = do
  Atom <$> rltn

formulaTrue :: Parser Formula
formulaTrue = do
  reserved "True"
  return T

formulaFalse :: Parser Formula
formulaFalse = do
  reserved "False"
  return F

formulaForAll :: Parser Formula
formulaForAll = do
  reserved "forall"
  (v : vs) <- reverse <$> many1 identifier
  char '.'
  whiteSpace
  scope <- formula
  return $ foldl (flip ForAll) (ForAll v scope) vs

formulaExists :: Parser Formula
formulaExists = do
  reserved "exists"
  (v : vs) <- reverse <$> many1 identifier
  char '.'
  whiteSpace
  scope <- formula
  return $ foldl (flip Exists) (Exists v scope) vs

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { -- Identifiers at this level are only used for variables, and
          -- variables are only allowed to start with upper case letters.
          Tok.identStart = upper,
          Tok.identLetter = alphaNum <|> char '_' <|> char '\'',
          -- Customer symbols are not allowed at the formula level, so we use
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
