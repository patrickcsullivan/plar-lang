module TruthTable where

import Control.Monad (join)
import Data.Either (fromRight)
import Data.List (nub)
import Syntax (Formula (..), Rltn (..), foldAtoms)

printTruthTable :: Formula -> IO ()
printTruthTable frm =
  let atoms = allAtoms frm
      atomNames = showAtom <$> atoms
      colWidth = (+ 1) . max 5 . maximum $ length <$> atomNames
      valuations = allValuations atomNames
   in do
        putStrLn ""
        putStrLn $ header colWidth atoms
        putStrLn $ rowDivider colWidth atoms
        sequence_ $ putStrLn . valuationRow colWidth frm <$> valuations
        putStrLn $ rowDivider colWidth atoms

showAtom :: Rltn -> String
showAtom (Rltn name _) = name

header :: Int -> [Rltn] -> String
header colWidth atoms =
  let atomColHeaders = atomColHeader colWidth <$> atoms
   in join atomColHeaders ++ formulaColHeader

formulaColHeader :: String
formulaColHeader = "| formula"

atomColHeader :: Int -> Rltn -> String
atomColHeader colWidth (Rltn atomName _) = padRightSpaces colWidth atomName

rowDivider :: Int -> [Rltn] -> String
rowDivider colWidth atoms =
  let width = colWidth * length atoms + length formulaColHeader
   in replicate width '-'

valuationRow :: Int -> Formula -> [(String, Bool)] -> String
valuationRow colWidth frm valuation =
  let evalStr = fromRight "?" $ show <$> eval frm valuation
      valStrs = padRightSpaces colWidth . show . snd <$> valuation
   in join valStrs ++ "| " ++ evalStr

padRightSpaces :: Int -> String -> String
padRightSpaces totalWidth s =
  let padWidth = max 0 $ totalWidth - length s
   in s ++ replicate padWidth ' '

allAtoms :: Formula -> [Rltn]
allAtoms = nub . foldAtoms (:) []

allValuations :: [String] -> [[(String, Bool)]]
allValuations atomsNames =
  let options = [True, False] <$ atomsNames
      combinations = sequenceA options
   in zip atomsNames <$> combinations

eval :: Formula -> [(String, Bool)] -> Either String Bool
eval frm vals =
  case frm of
    F -> return False
    T -> return True
    Atom (Rltn name []) ->
      case lookup name vals of
        Nothing -> Left name
        (Just bool) -> return bool
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
    _ -> undefined

eval' :: Formula -> [(String, Bool)] -> Bool
eval' frm vals = case eval frm vals of
  Right b -> b
  Left _ -> undefined