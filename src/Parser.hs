module Parser (parseFormula, parseFormula') where

import Data.Maybe (fromMaybe)
import Syntax
import Text.Parsec (ParseError, char, many, many1, optionMaybe, optional, parse, spaces, try, (<|>))
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

-- TOP LEVEL

parseFormula :: String -> Either ParseError (Formula Rltn)
parseFormula = parse (Tok.whiteSpace lexer *> formula rltn <* Tok.whiteSpace lexer) "<parser>"

parseFormula' :: String -> Formula Rltn
parseFormula' s =
  case parseFormula s of
    Right frm -> frm
    Left _ -> undefined

-- LEXER

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style =
      emptyDef
        { Tok.commentLine = "#",
          Tok.reservedOpNames = ["~", "and", "or", "==>", "<=>", "+", "-", "*", "/", "^"],
          Tok.reservedNames = ["True", "False", "forall", "exists"]
        }

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

-- semiSep :: Parser a -> Parser [a]
-- semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- PROPOSITIONAL

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
  spaces
  scope <- formula atomInner
  return $ foldl (flip ForAll) (ForAll v scope) vs

formulaExists :: Parser a -> Parser (Formula a)
formulaExists atomInner = do
  reserved "exists"
  (v : vs) <- reverse <$> many1 identifier
  char '.'
  spaces
  scope <- formula atomInner
  return $ foldl (flip Exists) (Exists v scope) vs

rltn :: Parser Rltn
rltn = do
  name <- identifier
  args <- fromMaybe [] <$> (optionMaybe . parens . commaSep) term
  return $ Rltn name args

-- FIRST-ORDER

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
    [Ex.Infix (reservedOp ":" >> return (mkBinary ":")) Ex.AssocRight]
  ]

mkBinary :: String -> Term -> Term -> Term
mkBinary name arg1 arg2 = Fn name [arg1, arg2]

termCase :: Parser Term
termCase =
  try termFn
    <|> termVar
    <|> parens term

termFn :: Parser Term
termFn = do
  name <- identifier
  args <- parens $ commaSep term
  return $ Fn name args

termVar :: Parser Term
termVar = Var <$> identifier
