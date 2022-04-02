module Parser.Rltn (rltn) where

import Data.Maybe (fromMaybe)
import Parser.Term (term)
import Syntax (Formula (..), Rltn (..), Term (..))
import Text.Parsec (alphaNum, char, choice, letter, many1, optionMaybe, parserZero, string, try, (<?>), (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token (symbol)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Token (GenTokenParser)

rltn :: Parser Rltn
rltn =
  try infixRltn
    <|> prefixRltn
    <?> "relation"

prefixRltn :: Parser Rltn
prefixRltn = do
  name <- identifier
  args <- fromMaybe [] <$> (optionMaybe . parens . commaSep) term
  return $ Rltn name args

infixRltn :: Parser Rltn
infixRltn = do
  left <- term
  name <- reservedOp
  right <- term
  return $ Rltn name [left, right]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { -- Prefix relations can contain alphanumeric and _ characters.
          Tok.identStart = letter <|> char '_',
          Tok.identLetter = alphaNum <|> char '_' <|> char '\'',
          -- Prevent the relation parser from trying to parse formula keywords
          -- as relations.
          Tok.reservedNames = ["True", "False", "forall", "exists"],
          -- Some reserved operators can be used as infix relations.
          Tok.reservedOpNames = reservedOpNames
        }

reservedOpNames :: [String]
reservedOpNames = ["<", ">", "<=", ">=", "/=", "="]

reservedOp :: Parser String
reservedOp = do
  op <- choice $ string <$> reservedOpNames
  whiteSpace
  return op

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer
