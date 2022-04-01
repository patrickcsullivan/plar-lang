module EvaluateExample where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Evaluate (Interpretation (..), Valuation (Valuation))
import Syntax (Formula (..), Rltn (..), Term (..))

boolInterp :: Interpretation Bool
boolInterp =
  Interpretation
    { domain = [False, True],
      fns =
        Map.fromList
          [ ("+", \[x, y] -> x || y),
            ("*", \[x, y] -> x && y)
          ],
      preds =
        Map.fromList
          [ ("=", \[x, y] -> x == y)
          ]
    }

boolValuation :: Valuation Bool
boolValuation = Valuation $ Map.fromList [("0", False), ("1", True)]

modInterp :: Int -> Interpretation Int
modInterp n =
  Interpretation
    { domain = [0 .. n -1],
      fns =
        Map.fromList
          [ ("+", \[x, y] -> (x + y) `mod` n),
            ("*", \[x, y] -> (x * y) `mod` n)
          ],
      preds =
        Map.fromList
          [ ("=", \[x, y] -> x == y)
          ]
    }

modValuation :: Int -> Valuation Int
modValuation n =
  Valuation $ Map.fromList [(show i, i) | i <- [0 .. n -1]]