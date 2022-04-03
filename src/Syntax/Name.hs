module Syntax.Name
  ( variant,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set

-- | Returns a variant of the given name that is distinct from the list of
-- names. Returns the name unchanged if it is not in the list of names.
variant :: String -> Set String -> String
variant x vars =
  if x `elem` vars
    then variant (x ++ "'") vars
    else x