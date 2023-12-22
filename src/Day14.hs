module Day14 where

import Paths_aoc2023 (getDataFileName)
import Data.List (transpose)

type Grid = [[Char]]

-- running total of load, number of periods
type State = (Int, Int)

processRow :: State -> [(Int, Char)] -> Int
processRow (currTotal, _) [] = currTotal
processRow (currTotal, _) ((_, '#'):rest) = processRow (currTotal, 0) rest
processRow (currTotal, currPeriods) ((_, '.'):rest) = processRow (currTotal, currPeriods + 1) rest
processRow (currTotal, currPeriods) ((idx, 'O'):rest) = processRow (updatedTotal, currPeriods) rest
  where
    updatedTotal = idx + currPeriods + currTotal

processRow _ _ = error "unexpected char encountered."

compute :: Grid -> Int
compute gr | not (null gr) =
  sum (fmap (processRow initialState . zip [maxCol, maxCol - 1 .. 1]) transposedGrid)
  where
    maxCol = length (head gr)
    transposedGrid = transpose gr
    initialState = (0, 0)
compute _ = 0

day14 :: IO ()
day14 = do
  inputLines <- lines <$> (getDataFileName "day14-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  let result = compute inputLines
  print result
