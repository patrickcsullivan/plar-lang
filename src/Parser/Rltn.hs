module Parser.Rltn (rltn) where

import Data.Maybe (fromMaybe)
import Parser.Term (term)
import Syntax (Formula (..), Rltn (..), Term (..))
import Text.Parsec (alphaNum, char, letter, many1, oneOf, optionMaybe, parserZero, try, (<|>))
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

prefixRltn :: Parser Rltn
prefixRltn = do
  name <- identifier
  args <- fromMaybe [] <$> (optionMaybe . parens . commaSep) term
  return $ Rltn name args

infixRltn :: Parser Rltn
infixRltn = do
  left <- term
  name <- (char '`' *> identifier <* char '`') <|> operator
  whiteSpace
  right <- term
  return $ Rltn name [left, right]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { -- Prefix relations can contain alphanumeric and _ characters.
          Tok.identStart = letter <|> char '_',
          Tok.identLetter = alphaNum <|> char '_',
          -- Infix relations can contain a mix of symbols and alphanumeric.
          Tok.opStart = symbols,
          Tok.opLetter = symbols <|> alphaNum <|> char '_',
          -- There are no reserved names.
          Tok.reservedOpNames = [],
          Tok.reservedNames = []
        }
    symbols = oneOf ":!#$%&*+./<=>?@\\^|-~"

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

operator :: Parser String
operator = Tok.operator lexer
