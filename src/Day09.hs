module Day09 where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)
import Utils (negNumParser)

type Rows = [[Int]]

processRow :: [Int] -> Int
processRow is =
  processRow' is [] [head is]
  where
    processRow' (x : y : rest) curr acc =
      processRow' (y : rest) ((x - y) : curr) acc
    processRow' [] curr acc =
      if all (== 0) curr
        then sum acc
        else
          let revCurr = reverse curr
           in processRow' revCurr [] (head revCurr : acc)
    processRow' [_] curr acc = processRow' [] curr acc

parseInput :: Parser [Int]
parseInput =
  sepBy negNumParser space

calcAnswer :: [[Int]] -> Int
calcAnswer is =
  sum $ processRow <$> is

day09 :: IO ()
day09 = do
  inputLines <- T.lines <$> (getDataFileName "day09-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let parsed = fmap (parseOnly parseInput) inputLines
  is <- either fail pure $ sequence parsed
  let result = calcAnswer is
  print result
