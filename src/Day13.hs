module Day13 where

import Paths_aoc2023 (getDataFileName)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Utils (transposeVec)
import Data.List.Split
import System.TimeIt (timeIt)

type Grid = Vector (Vector Char)

parseInput :: String -> [Grid]
parseInput st =
  fmap (V.fromList . fmap V.fromList) spl
  where
    spl = (map lines . splitOn "\n\n") st

checkReflection :: Int -> Int -> V.Vector Char -> Bool
checkReflection missCount n chs
  | n > 0 && n < V.length chs =
    (actualMisses == missCount) && result
    where
      (actualMisses, result) = foldr step (0, True) indicesToCheck
      step (x, y) (missAcc, b)
        | chs ! x == chs ! y = (missAcc, b)
        | missAcc < missCount = (missAcc + 1, b)
        | otherwise = (-1, False)
      indicesToCheck = zip [n-1,n-2..0] [n..V.length chs - 1]

checkReflection _ _ _ = False

-- find initial
findAllReflections :: Int -> Vector Char -> [Int]
findAllReflections _ chs | V.length chs < 2 = []
findAllReflections missCount chs =
  foldr step [] args
  where
    step n acc
      | checkReflection missCount n chs = n : acc
    step _ acc = acc
    args = [1..V.length chs - 1]

-- first row miss, accum miss
compute :: Int -> Int -> Grid -> [Int]
compute firstRowMiss accumMisses rows | not (null rows) =
  foldr step [] firstReflections
  where
    t = V.tail rows
    step n acc
      | length t - length (V.filter (checkReflection 0 n) t) == accumMisses = n : acc
    step _  acc = acc
    firstReflections = findAllReflections firstRowMiss (rows ! 0)
compute _ _ _ = pure 0

part1, part2 :: [Grid] -> Int
part1 grs =
  sum $ single <$> grs
  where
    single gr = sum (horizontal gr) + sum ((*100) <$> vertical gr)
    horizontal = compute 0 0
    vertical = compute 0 0 . transposeVec

part2 grs =
  sum $ single <$> grs
  where
    single gr = sum (horizontalF gr ++ horizontalS gr) + sum ((*100) <$> (verticalF gr ++ verticalS gr))
    horizontalF = compute 1 0
    horizontalS = compute 0 1
    verticalF = compute 1 0 . transposeVec
    verticalS = compute 0 1 . transposeVec

day13 :: IO ()
day13 = do
  input <- getDataFileName "day13-input.txt" >>= readFile
  putStrLn "This is what I read from input:"
  putStrLn input
  let grids = parseInput input
  let result = part2 grids
  timeIt $ print result
