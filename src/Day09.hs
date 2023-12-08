module Day09 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

day09 :: IO ()
day09 = do
  inputLines <- T.lines <$> (getDataFileName "day09-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  putStrLn "TODO: implement Day 09"
