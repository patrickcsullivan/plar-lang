module Parser.Parser (run, run') where

import Parser.Formula (formula)
import Parser.Rltn (rltn)
import Syntax (Formula, Rltn)
import Text.Parsec (ParseError, parse)

run :: String -> Either ParseError (Formula Rltn)
run = parse (formula rltn) "<parser>"

run' :: String -> Formula Rltn
run' s =
  case run s of
    Right frm -> frm
    Left _ -> undefined
