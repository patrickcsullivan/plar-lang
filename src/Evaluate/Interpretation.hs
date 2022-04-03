module Evaluate.Interpretation
  ( Interpretation (..),
    lookupFn,
    lookupPred,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Syntax (Formula (..), Rltn (..), Term (..))

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

-- | Returns the domain function associated with the given function name in the
-- interpretation. Throws an error if the function name does not exist in the
-- interpretation.
lookupFn :: String -> Interpretation a -> ([a] -> a)
lookupFn name (Interpretation _ fns _) =
  case Map.lookup name fns of
    Just fn -> fn
    _ -> error $ "domain funtion `" ++ name ++ "` not defined"

-- | Returns the predicate (aka relation) associated with the given predicate
-- name in the interpretation. Throws an error if the predicate name does not
-- exist in the interpretation.
lookupPred :: String -> Interpretation a -> ([a] -> Bool)
lookupPred name (Interpretation _ _ preds) =
  case Map.lookup name preds of
    Just rltn -> rltn
    _ -> error $ "relation `" ++ name ++ "` not defined"