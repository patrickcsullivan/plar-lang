module Evaluate.Valuation
  ( Valuation (..),
    (|->),
    empty,
    lookup,
  )
where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Syntax (Formula (..), Rltn (..), Term (..))
import Prelude hiding (lookup)

-- | Specifies the meaning of function symbols and predicate symbols in first-
-- order logic.
data Interpretation a = Interpretation
  { -- | A finite, non-empty domain.
    domain :: [a],
    -- | A mapping from names to domain functions.
    fns :: Map String ([a] -> a),
    -- | A mapping from names to predicates (aka relations).
    preds :: Map String ([a] -> Bool)
  }

-- | Specifies the meaning of variables by mapping each variable to a domain
-- "object".
newtype Valuation a = Valuation
  { unValuation :: Map String a
  }
  deriving (Show)

-- | Returns an empty valuation.
empty :: Valuation a
empty = Valuation Map.empty

-- | Returns a function that updates the the domain "object" associated with the
-- variable name `x` in the valuation.
(|->) :: String -> a -> (Valuation a -> Valuation a)
x |-> a = Valuation . Map.insert x a . unValuation

-- | Returns the domain "object" associated with the given variable name in the
-- valuation. Throws an error if the variable name does not exist in the
-- valuation.
lookup :: String -> Valuation a -> a
lookup name (Valuation val) =
  case Map.lookup name val of
    Just v -> v
    _ -> error $ "domain variable `" ++ name ++ "` not defined"
