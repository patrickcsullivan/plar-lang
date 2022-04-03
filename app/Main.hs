module Main where

import Parser (parse')
import Syntax.Rewrite (simplify)

main :: IO ()
main = do
  putStrLn "Hello."
  let f = parse' "(forall x. P(x) or R(y)) ==> exists y z. Q(y) or ~(exists z. P(z) or Q(z))"
  let f' = simplify f
  print f
  print f'
  return ()
