module AndOr
  ( AndOr (..),
    leftToMaybe,
    rightToMaybe,
    leftElseRight,
  )
where

import Prelude hiding (Either (..))

-- | Represents a value with two non-mutually exclusive possibilities.
data AndOr a b = Left a | Right b | LeftRight a b

-- | Returns the left value if it exists or the right value otherwise.
leftElseRight :: AndOr a a -> a
leftElseRight andOr =
  case andOr of
    Left a -> a
    LeftRight a _ -> a
    Right a -> a

-- | Returns the left value if it exists.
leftToMaybe :: AndOr a b -> Maybe a
leftToMaybe andOr =
  case andOr of
    Left a -> Just a
    LeftRight a _ -> Just a
    Right _ -> Nothing

-- | Returns the right value if it exists.
rightToMaybe :: AndOr a b -> Maybe b
rightToMaybe andOr =
  case andOr of
    Left _ -> Nothing
    LeftRight _ b -> Just b
    Right b -> Just b
