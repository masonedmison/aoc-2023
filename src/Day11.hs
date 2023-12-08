module Day11 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day11 :: IO ()
day11 = do
  inputLines <- T.lines <$> (getDataFileName "day11-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  putStrLn "TODO: implement Day 11"
