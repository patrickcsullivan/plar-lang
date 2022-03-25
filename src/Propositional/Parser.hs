{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Propositional.Parser () where

import Propositional.Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

newtype Prop = Prop String deriving (Eq, Ord)

instance Show Prop where
  show (Prop name) = name

formulaTable =
  [ [Ex.Prefix (reservedOp "~" >> return Not)],
    [Ex.Infix (reservedOp "and" >> return And) Ex.AssocRight],
    [Ex.Infix (reservedOp "or" >> return Or) Ex.AssocRight],
    [Ex.Infix (reservedOp "==>" >> return Imp) Ex.AssocRight],
    [Ex.Infix (reservedOp "<=>" >> return Iff) Ex.AssocRight]
  ]

parseFormula :: String -> Either ParseError (Formula Prop)
parseFormula = parse (Tok.whiteSpace lexer *> formula (Prop <$> identifier) <* Tok.whiteSpace lexer) "<parser>"

parseFormula' :: String -> Formula Prop
parseFormula' s =
  case parseFormula s of
    Right frm -> frm
    Left _ -> undefined

formula :: Parser a -> Parser (Formula a)
formula atomIdentifier = Ex.buildExpressionParser formulaTable (formulaTerm atomIdentifier)

formulaTerm :: Parser a -> Parser (Formula a)
formulaTerm atomIdentifier =
  try formulaTrue
    <|> try formulaFalse
    <|> formulaAtom atomIdentifier
    <|> parens (formula atomIdentifier)

formulaAtom :: Parser a -> Parser (Formula a)
formulaAtom atomIdentifier = do
  a <- atomIdentifier
  return $ Atom a

formulaTrue :: Parser (Formula a)
formulaTrue = do
  reserved "True"
  return T

formulaFalse :: Parser (Formula a)
formulaFalse = do
  reserved "False"
  return F