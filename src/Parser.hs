module Parser
  ( parse,
    parse',
  )
where

import Parser.Formula (formula)
import Parser.Rltn (rltn)
import Syntax (Formula, Rltn)
import qualified Text.Parsec as Parsec

parse :: String -> Either Parsec.ParseError (Formula Rltn)
parse = Parsec.parse (formula rltn) "<parser>"

parse' :: String -> Formula Rltn
parse' s =
  case parse s of
    Right frm -> frm
    Left _ -> undefined
