{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Parser
  ( parseFormula,
  )
where

import Lexer
import Syntax
import Text.Parsec
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok

table =
  [ [Ex.Prefix (reservedOp "~" >> return Not)],
    [Ex.Infix (reservedOp "and" >> return And) Ex.AssocRight],
    [Ex.Infix (reservedOp "or" >> return Or) Ex.AssocRight],
    [Ex.Infix (reservedOp "==>" >> return Imp) Ex.AssocRight],
    [Ex.Infix (reservedOp "<=>" >> return Iff) Ex.AssocRight]
  ]

parseFormula :: String -> Either ParseError (Formula String)
parseFormula = parse (Tok.whiteSpace lexer *> formula <* Tok.whiteSpace lexer) "<parser>"

formula :: Parser (Formula String)
formula = Ex.buildExpressionParser table formulaTerm

formulaTerm :: Parser (Formula String)
formulaTerm =
  try formulaTrue
    <|> try formulaFalse
    <|> formulaAtom
    <|> parens formula

formulaAtom :: Parser (Formula String)
formulaAtom = do
  atom <- identifier
  return $ Atom atom

formulaTrue :: Parser (Formula String)
formulaTrue = do
  reserved "True"
  return T

formulaFalse :: Parser (Formula String)
formulaFalse = do
  reserved "False"
  return F