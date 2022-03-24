module Propositional.Valuation (allValuations) where

allValuations :: Eq a => [a] -> [[(a, Bool)]]
allValuations atoms =
  let options = [True, False] <$ atoms
      combinations = sequenceA options
   in zip atoms <$> combinations