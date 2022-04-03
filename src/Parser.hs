module Parser
  ( parse,
    parse',
  )
where

import Parser.Formula (formula)
import Syntax (Formula, Rltn)
import qualified Text.Parsec as Parsec

parse :: String -> Either Parsec.ParseError Formula
parse = Parsec.parse formula "<parser>"

parse' :: String -> Formula
parse' s =
  case parse s of
    Right frm -> frm
    Left _ -> undefined
