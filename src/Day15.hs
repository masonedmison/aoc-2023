module Day15 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Extra (splitOn)
import Data.Foldable (foldl')
import Data.Char (ord)

type Input = [[Char]]

parseInput :: String -> Input
parseInput = splitOn ","

part1 :: Input -> Int
part1 inp =
  sum $ processSeq <$> inp
  where
    processSeq = foldl' step 0
    step acc ch = ((acc + ord ch) * 17)  `mod` 256

day15 :: IO ()
day15 = do
  input <- getDataFileName "day15-input.txt" >>= readFile
  putStrLn "This is what I read from input:"
  putStrLn input
  print $ part1 $ parseInput input
