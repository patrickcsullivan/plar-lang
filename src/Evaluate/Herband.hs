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

-- | Test larger and larger conjunctions of ground instances until
-- satisfiability is verified.
herbandLoop ::
  -- | Augments the ground instances with a new ground instance.
  (a -> b -> [c] -> [c]) ->
  -- | Satisfiability test.
  ([c] -> Bool) ->
  -- | Initial formula in some transformed list representation.
  a ->
  -- | The constant terms in the formula.
  [Term] ->
  -- | The non-nullary functions in the formula.
  [(FnName, FnArity)] ->
  -- | The free variables in the formula.
  [String] ->
  -- | The next level of the enumeration to generate.
  Int ->
  -- |
  [c] ->
  -- | The ground instance argument combinations tried so far.
  [[Term]] ->
  -- | The remaining ground instances argument combinations to try at the
  -- current level.
  [[Term]] ->
  IO [[Term]]
herbandLoop fAugment fIsSat frm0 cnstTrms fns freeVars n fl triedArgCombs remainingArgCombs = do
  putStrLn $ show (length triedArgCombs) ++ " ground instances tried"
  putStrLn $ show (length fl) ++ " items in list"
  putStrLn ""
  case remainingArgCombs of
    [] ->
      -- When there are no remaining ground argument combinations to try at the
      -- current level, generate the next level of ground argument combinations
      -- and step from level n to level n + 1.
      let newArgCombinations = groundArgCombinations cnstTrms fns n (length freeVars)
       in herbandLoop fAugment fIsSat frm0 cnstTrms fns freeVars (n + 1) fl triedArgCombs newArgCombinations
    argComb : argCombs ->
      let fl' = fAugment frm0 undefined fl
       in if not (fIsSat fl')
            then return $ argComb : triedArgCombs
            else herbandLoop fAugment fIsSat frm0 cnstTrms fns freeVars n fl' (argComb : triedArgCombs) argCombs

-- | Generate all ground terms involving "depth-`n`" functions.
groundTerms :: [Term] -> [(FnName, FnArity)] -> Int -> [Term]
groundTerms cnstTrms fns n =
  if n == 0
    then cnstTrms
    else do
      (name, arity) <- fns
      let argCombinations = groundArgCombinations cnstTrms fns (n - 1) arity
      Fn name <$> argCombinations

-- | Generate all combinations of arguments for an `m`-ary function where each
-- argument has a maximum "depth" of `n`.
groundArgCombinations :: [Term] -> [(FnName, FnArity)] -> Int -> Int -> [[Term]]
groundArgCombinations cnsts fns n m =
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
      tail <- groundArgCombinations cnsts fns (n - depth) (m - 1)
      -- append the head term to tail of (m - 1) args for a combination of m args
      return $ head : tail

-- | Return the functions that will be used to construct a Herband universe. The
-- returned functions are partitioned in to constants (nullay functions) and
-- non-nullary functions.
herbandFunctions :: Formula -> (Set (FnName, FnArity), Set (FnName, FnArity))
herbandFunctions frm =
  let (cnsts, fns) = Set.partition (\(name, arity) -> arity == 0) (functions frm)
   in if null cnsts
        then (Set.singleton ("c", 0), fns)
        else (cnsts, fns)

-- foldr
--   ( \k acc -> do
--       h <- groundTerms cnsts fns k
--       t <- groundArgCombinations cnsts fns (n - k) (m -1)
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