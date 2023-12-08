module Day23 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day23 :: IO ()
day23 = do
  inputLines <- T.lines <$> (getDataFileName "day23-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  putStrLn "TODO: implement Day 23"
