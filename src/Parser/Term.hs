module Parser.Term (term) where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Syntax (Formula (..), Rltn (..), Term (..))
import Text.Parsec (ParseError, alphaNum, char, letter, many, many1, oneOf, optionMaybe, optional, parse, spaces, try, (<?>), (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Token (GenTokenParser)

term :: Parser Term
term = Ex.buildExpressionParser termTable termCase

termTable =
  [ [Ex.Infix (reservedOp "^" >> return (mkBinary "+")) Ex.AssocRight],
    [ Ex.Infix (reservedOp "*" >> return (mkBinary "*")) Ex.AssocLeft,
      Ex.Infix (reservedOp "/" >> return (mkBinary "/")) Ex.AssocLeft
    ],
    [ Ex.Infix (reservedOp "+" >> return (mkBinary "+")) Ex.AssocLeft,
      Ex.Infix (reservedOp "-" >> return (mkBinary "-")) Ex.AssocLeft
    ],
    [ Ex.Infix (reservedOp ":" >> return (mkBinary ":")) Ex.AssocRight
    ]
  ]

mkBinary :: String -> Term -> Term -> Term
mkBinary name arg1 arg2 = Fn name [arg1, arg2]

termCase :: Parser Term
termCase =
  try fn
    <|> var
    <|> parens term
    <?> "term"

fn :: Parser Term
fn = do
  name <- identifier
  args <- parens $ commaSep term
  return $ Fn name args

var :: Parser Term
var = Var <$> identifier

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { -- Prefix domain functions can contain alphanumeric and _ characters.
          Tok.identStart = letter <|> char '_',
          Tok.identLetter = alphaNum <|> char '_',
          -- Infix domain functions can contain a mix of symbols and
          -- alphanumeric characters.
          Tok.opStart = symbols,
          Tok.opLetter = symbols <|> alphaNum <|> char '_',
          -- Arithmetic operators are reserved so each can be given the correct
          -- precedence and associativity.
          Tok.reservedOpNames = ["+", "-", "*", "/", "^"],
          -- There are no reserved prefix domain function or domain variables
          -- names.
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

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer
