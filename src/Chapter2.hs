module Chapter2 where

import Control.Monad (join)
import Data.Either (fromRight)
import Data.List (nub)
import Syntax

conjuncts :: Formula a -> [Formula a]
conjuncts frm =
  case frm of
    And p q -> conjuncts p ++ conjuncts q
    _ -> [frm]

allAtoms :: Eq a => Formula a -> [a]
allAtoms = nub . foldr (:) []

orError :: Maybe b -> a -> Either a b
orError Nothing a = Left a
orError (Just b) _ = Right b

allValuations :: Eq a => [a] -> [[(a, Bool)]]
allValuations atoms =
  let options = [True, False] <$ atoms
      combinations = sequenceA options
   in zip atoms <$> combinations

eval :: Eq a => Formula a -> [(a, Bool)] -> Either a Bool
eval frm vals =
  case frm of
    F -> return False
    T -> return True
    Atom a ->
      case lookup a vals of
        Nothing -> Left a
        (Just b) -> return b
    Not frm -> not <$> eval frm vals
    And frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (b1 && b2)
    Or frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (b1 || b2)
    Imp frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (not b1 || b2)
    Iff frm frm' -> do
      b1 <- eval frm vals
      b2 <- eval frm' vals
      return (b1 == b2)
    ForAll _ _ -> undefined
    Exists _ _ -> undefined

eval' :: Eq a => Formula a -> [(a, Bool)] -> Bool
eval' frm vals = case eval frm vals of
  Right b -> b
  Left _ -> undefined

-- 2.3 Validity, satisfiability, tautology

tautology :: Eq a => Formula a -> Bool
tautology frm = (all (eval' frm) . allValuations . allAtoms) frm

unsatisfiable :: Eq a => Formula a -> Bool
unsatisfiable frm = tautology (Not frm)

satisfiable :: Eq a => Formula a -> Bool
satisfiable frm = not (unsatisfiable frm)

subst :: Eq a => a -> Formula a -> Formula a -> Formula a
subst atom new = onAtoms substIfMatch
  where
    substIfMatch a = if a == atom then new else Atom a

-- 2.4 The De Morgan laws, adequacy, and duality

dual :: Formula a -> Formula a
dual frm =
  case frm of
    F -> T
    T -> F
    Atom a -> Atom a
    Not p -> Not (dual p)
    And p q -> Or (dual p) (dual q)
    Or p q -> And (dual p) (dual q)
    _ -> undefined

-- 2.5 Simplification and negation normal form

-- | Simplify the formula to any propositional constants from the top level of
-- the formula.
simplify1 :: Formula a -> Formula a
simplify1 frm =
  case frm of
    Not F -> T
    Not T -> F
    Not (Not p) -> p
    And p F -> F
    And F p -> F
    And p T -> p
    And T p -> p
    Or p F -> p
    Or F p -> p
    Or p T -> T
    Or T p -> T
    -------
    Imp F p -> T
    Imp p T -> T
    Imp T p -> p
    Imp p F -> Not p -- must come at the end of this group sof F => F will simplify to T instead of ~F
    -------
    Iff p T -> p
    Iff T p -> p
    Iff p F -> Not p -- must come at the end of this group sof T <=> F will simplify to F instead of ~T
    Iff F p -> Not p -- must come at the end of this group sof F <=> T will simplify to F instead of ~T
    -------
    _ -> frm

simplify :: Formula a -> Formula a
simplify frm =
  case frm of
    Not p -> simplify1 $ Not (simplify p)
    And p q -> simplify1 $ And (simplify p) (simplify q)
    Or p q -> simplify1 $ Or (simplify p) (simplify q)
    Imp p q -> simplify1 $ Imp (simplify p) (simplify q)
    Iff p q -> simplify1 $ Iff (simplify p) (simplify q)
    _ -> frm

isNegativeLiteral :: Formula a -> Bool
isNegativeLiteral frm =
  case frm of
    Not (Atom _) -> True
    _ -> False

isPositiveLiteral :: Formula a -> Bool
isPositiveLiteral frm =
  case frm of
    Atom _ -> True
    _ -> False

negateLiteral :: Formula a -> Formula a
negateLiteral frm =
  case frm of
    Atom a -> Not (Atom a)
    Not (Atom a) -> Atom a
    _ -> frm

-- | Put the formula in negation normal form (NNF) by repeatedly appling the De
-- Morgan laws and the law of double negation to push negations down to the
-- atomic forumulas.
nnf :: Formula a -> Formula a
nnf frm =
  case frm of
    And p q -> And (nnf p) (nnf q)
    Or p q -> Or (nnf p) (nnf q)
    Imp p q -> Or (nnf $ Not p) (nnf q)
    Iff p q -> Or (And (nnf p) (nnf q)) (And (nnf $ Not p) (nnf $ Not q))
    Not (Not p) -> nnf p
    Not (And p q) -> Or (nnf $ Not p) (nnf $ Not q)
    Not (Or p q) -> And (nnf $ Not p) (nnf $ Not q)
    Not (Imp p q) -> And (nnf p) (nnf $ Not q)
    Not (Iff p q) -> Or (And (nnf p) (nnf $ Not q)) (And (nnf $ Not p) (nnf q))
    _ -> frm

nnfSimplify :: Formula a -> Formula a
nnfSimplify = nnf . simplify

nenf :: Formula a -> Formula a
nenf frm =
  case frm of
    Not (Not p) -> nenf p
    Not (And p q) -> Or (nenf $ Not p) (nenf $ Not q)
    Not (Or p q) -> And (nenf $ Not p) (nenf $ Not q)
    Not (Imp p q) -> And (nenf p) (nenf $ Not q)
    Not (Iff p q) -> Iff (nenf p) (nenf q)
    And p q -> And (nenf p) (nenf q)
    Or p q -> Or (nenf p) (nenf q)
    Imp p q -> Or (nenf $ Not p) (nenf q)
    Iff p q -> Iff (nenf p) (nenf q)
    _ -> frm

nenfSimplify = nenf . simplify

-- 2.6 Disjunctive and conjunctive normal forms

-- | Combine the formulas into an iterated conjunction.
listConj :: [Formula a] -> Formula a
listConj frms =
  case frms of
    [] -> T
    _ -> foldr1 And frms

-- | Combine the formulas into an iterated disjunction.
listDisj :: [Formula a] -> Formula a
listDisj frms =
  case frms of
    [] -> F
    _ -> foldr1 Or frms

-- TRUTH TABLE

printTruthTable :: Eq a => Show a => Formula a -> IO ()
printTruthTable frm =
  let atoms = allAtoms frm
      colWidth = (+ 1) . max 5 . maximum $ length . show <$> atoms
      valuations = allValuations atoms
   in do
        putStrLn ""
        putStrLn $ header colWidth atoms
        putStrLn $ rowDivider colWidth atoms
        sequence_ $ putStrLn . valuationRow colWidth frm <$> valuations
        putStrLn $ rowDivider colWidth atoms

header :: Show a => Int -> [a] -> String
header colWidth atoms =
  let atomColHeaders = atomColHeader colWidth <$> atoms
   in join atomColHeaders ++ formulaColHeader

formulaColHeader :: String
formulaColHeader = "| formula"

atomColHeader :: Show a => Int -> a -> String
atomColHeader colWidth atom = padRightSpaces colWidth (show atom)

rowDivider :: Int -> [a] -> String
rowDivider colWidth atoms =
  let width = colWidth * length atoms + length formulaColHeader
   in replicate width '-'

valuationRow :: Eq a => Int -> Formula a -> [(a, Bool)] -> String
valuationRow colWidth frm valuation =
  let evalStr = fromRight "?" $ show <$> eval frm valuation
      valStrs = padRightSpaces colWidth . show . snd <$> valuation
   in join valStrs ++ "| " ++ evalStr

padRightSpaces :: Int -> String -> String
padRightSpaces totalWidth s =
  let padWidth = max 0 $ totalWidth - length s
   in s ++ replicate padWidth ' '