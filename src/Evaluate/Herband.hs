{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}

module Evaluate.Herband where

import Data.Set (Set)
import qualified Data.Set as Set
import Evaluate.Interpretation (Interpretation (fns))
import Syntax (Formula (..), Rltn (..), Term (..))
import Syntax.Functions (functions)

-- | Domain function name.
type FnName = String

-- | Domain function arity.
type FnArity = Int

-- | Return the functions that will be used to construct a Herband universe. The
-- returned functions are partitioned in to constants (nullay functions) and
-- non-nullary functions.
herbandFunctions :: Formula -> (Set (FnName, FnArity), Set (FnName, FnArity))
herbandFunctions frm =
  let (cnsts, fns) = Set.partition (\(name, arity) -> arity == 0) (functions frm)
   in if null cnsts
        then (Set.singleton ("c", 0), fns)
        else (cnsts, fns)

-- | Generate all ground terms involving "depth-`n`" functions.
groundTerms :: [Term] -> [(FnName, FnArity)] -> Int -> [Term]
groundTerms cnstTrms fns n =
  if n == 0
    then cnstTrms
    else do
      (name, arity) <- fns
      let argCombinations = groundTuples cnstTrms fns (n - 1) arity
      Fn name <$> argCombinations

-- else
--   ( \(name, arity) ->
--       let argCombinations = groundTuples cnstTrms fns (n -1) arity
--        in Fn name <$> argCombinations
--   )
--     `concatMap` fns

-- else do -- monadic version
--   (name, arity) <- fns
--   let argCombinations = groundTuples cnstTrms fns (n -1) arity
--   Fn name <$> argCombinations

-- | Generate all combinations of arguments for an `m`-ary function where each
-- argument has a maximum "depth" of `n`.
groundTuples :: [Term] -> [(FnName, FnArity)] -> Int -> Int -> [[Term]]
groundTuples cnsts fns n m =
  if m == 0
    then
      if n == 0
        then -- The nullary function can have exactly one combination of depth-0
        -- arguments, the empty list of arguments.
          [[]]
        else -- A nullary function has no combinations of depth-n arguments when
        -- n > 0.
          []
    else do
      -- for each depth from 0 to n
      depth <- [0 .. n]
      -- for each term with the given depth
      head <- groundTerms cnsts fns depth
      -- for each combination of (m - 1) args where each arg has a max depth of (n - depth)
      tail <- groundTuples cnsts fns (n - depth) (m - 1)
      -- append the head term to tail of (m - 1) args for a combination of m args
      return $ head : tail

-- foldr
--   ( \k acc -> do
--       h <- groundTerms cnsts fns k
--       t <- groundTuples cnsts fns (n - k) (m -1)
--       h : t
--   )

trms =
  [ Fn "f" [Fn "c" []],
    Fn "f" [Fn "d" []],
    Fn "g" [Fn "c" [], Fn "c" []],
    Fn "g" [Fn "c" [], Fn "d" []],
    Fn "g" [Fn "d" [], Fn "c" []],
    Fn "g" [Fn "d" [], Fn "d" []]
  ]

trms2 =
  [ Fn "f" [Fn "f" [Fn "c" []]],
    Fn "f" [Fn "f" [Fn "d" []]],
    Fn "f" [Fn "g" [Fn "c" [], Fn "c" []]],
    Fn "f" [Fn "g" [Fn "c" [], Fn "d" []]],
    Fn "f" [Fn "g" [Fn "d" [], Fn "c" []]],
    Fn "f" [Fn "g" [Fn "d" [], Fn "d" []]],
    Fn "g" [Fn "c" [], Fn "f" [Fn "c" []]],
    Fn "g" [Fn "c" [], Fn "f" [Fn "d" []]],
    Fn "g" [Fn "c" [], Fn "g" [Fn "c" [], Fn "c" []]],
    Fn "g" [Fn "c" [], Fn "g" [Fn "c" [], Fn "d" []]],
    Fn "g" [Fn "c" [], Fn "g" [Fn "d" [], Fn "c" []]],
    Fn "g" [Fn "c" [], Fn "g" [Fn "d" [], Fn "d" []]],
    Fn "g" [Fn "d" [], Fn "f" [Fn "c" []]],
    Fn "g" [Fn "d" [], Fn "f" [Fn "d" []]],
    Fn "g" [Fn "d" [], Fn "g" [Fn "c" [], Fn "c" []]],
    Fn "g" [Fn "d" [], Fn "g" [Fn "c" [], Fn "d" []]],
    Fn "g" [Fn "d" [], Fn "g" [Fn "d" [], Fn "c" []]],
    Fn "g" [Fn "d" [], Fn "g" [Fn "d" [], Fn "d" []]],
    Fn "g" [Fn "f" [Fn "c" []], Fn "c" []],
    Fn "g" [Fn "f" [Fn "c" []], Fn "d" []],
    Fn "g" [Fn "f" [Fn "d" []], Fn "c" []],
    Fn "g" [Fn "f" [Fn "d" []], Fn "d" []],
    Fn "g" [Fn "g" [Fn "c" [], Fn "c" []], Fn "c" []],
    Fn "g" [Fn "g" [Fn "c" [], Fn "c" []], Fn "d" []],
    Fn "g" [Fn "g" [Fn "c" [], Fn "d" []], Fn "c" []],
    Fn "g" [Fn "g" [Fn "c" [], Fn "d" []], Fn "d" []],
    Fn "g" [Fn "g" [Fn "d" [], Fn "c" []], Fn "c" []],
    Fn "g" [Fn "g" [Fn "d" [], Fn "c" []], Fn "d" []],
    Fn "g" [Fn "g" [Fn "d" [], Fn "d" []], Fn "c" []],
    Fn "g" [Fn "g" [Fn "d" [], Fn "d" []], Fn "d" []]
  ]