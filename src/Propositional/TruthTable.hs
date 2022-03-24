module Propositional.TruthTable where

import Control.Monad (join)
import Data.Either (fromRight)
import Propositional.Evaluate (eval)
import Propositional.Formula (allAtoms)
import Propositional.Valuation (allValuations)
import Syntax (Formula)

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