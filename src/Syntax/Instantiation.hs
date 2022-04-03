module Syntax.Instantiation
  ( Instantiation,
    (|->),
    (|=>),
    lookup,
    removeVar,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax (Formula (..), Rltn (..), Term (..))
import Prelude hiding (lookup)

-- | A variable assignment that maps variables to terms.
type Instantiation = Map String Term

-- | Returns a function that adds the given variable-to-term mapping to the
-- instantiation.
(|->) :: String -> Term -> Instantiation -> Instantiation
x |-> a = Map.insert x a

-- | Returns an instantiation containing a single variable-to-term mapping.
(|=>) :: String -> Term -> Instantiation
x |=> a = Map.singleton x a

removeVar :: String -> Instantiation -> Instantiation
removeVar = Map.delete

lookup :: String -> Instantiation -> Maybe Term
lookup = Map.lookup