module Day13 where

import Paths_aoc2023 (getDataFileName)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Utils (transposeVec)
import Data.List.Split

type Grid = Vector (Vector Char)

parseInput :: String -> [Grid]
parseInput st =
  fmap (V.fromList . fmap V.fromList) spl
  where
    spl = (map lines . splitOn "\n\n") st

checkReflection :: Int -> V.Vector Char -> Bool
checkReflection n chs 
  | (n - 1) > 0 && (n + 1) < V.length chs =
    foldr step True indicesToCheck
    where
      step (x, y) b
        | chs ! x == chs ! y = b
        | otherwise = False
      indicesToCheck = zip [n-1,n-2..0] [n..V.length chs - 1]

checkReflection _ _ = False

-- find initial
findAllReflections :: Vector Char -> [Int]
findAllReflections chs | V.length chs < 2 = []
findAllReflections chs =
  foldr step [] args
  where
    step n acc
      | checkReflection n chs = n : acc
    step _ acc = acc
    args = [1..V.length chs - 1]

compute :: Grid -> [Int]
compute rows | not (null rows) =
  foldr step [] firstReflections
  where
    t = V.tail rows
    step n acc
      | all (checkReflection n) t = n : acc
    step _  acc = acc
    firstReflections = findAllReflections (V.head rows)
compute _ = pure 0

part1 :: [Grid] -> Int
part1 grs =
  sum $ single <$> grs
  where
    single gr = sum (horizontal gr) + sum ((*100) <$> vertical gr)
    horizontal = compute
    vertical = compute . transposeVec

day13 :: IO ()
day13 = do
  input <- getDataFileName "day13-input.txt" >>= readFile
  putStrLn "This is what I read from input:"
  putStrLn input
  let grids = parseInput input
  let result = part1 grids
  print result
