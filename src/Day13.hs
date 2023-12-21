module Day13 where

import Paths_aoc2023 (getDataFileName)
import Data.Vector ((!), Vector)
import qualified Data.Vector as V
import Utils (transposeVec)
import Data.List.Split

type Grid = Vector (Vector Char)

data SplitPoint = Clean | Dup deriving (Show, Eq)

parseInput :: String -> [Grid]
parseInput st =
  fmap (V.fromList . fmap V.fromList) spl
  where
    spl = (map lines . splitOn "\n\n") st

adjustCol :: (SplitPoint, Int) -> Int
adjustCol (Clean, n) = n
adjustCol (Dup, n) = n

checkReflection :: SplitPoint -> Int -> V.Vector Char -> Bool
checkReflection Dup n chs
  | n  > 0 && (n + 1) < V.length chs =
    foldr step True indicesToCheck
    where
      step (x, y) b
        | chs ! x == chs ! y = b
        | otherwise = False
      indicesToCheck = zip [n,n-1..0] [n..V.length chs - 1]

checkReflection Clean n chs 
  | (n - 1) > 0 && (n + 1) < V.length chs =
    -- trace ("indices to check: " ++ show indicesToCheck) 
    foldr step True indicesToCheck
    where
      step (x, y) b
        | chs ! x == chs ! y = b
        | otherwise = False
      indicesToCheck = zip [n-1,n-2..0] [n..V.length chs - 1]

checkReflection _ _ _ = False

-- find initial
findAllReflections :: Vector Char -> [(SplitPoint, Int)]
findAllReflections chs | V.length chs < 2 = []
findAllReflections chs =
  foldr step [] args
  where
    step (spl, n) acc
      | checkReflection spl n chs = col : acc
        where
          col = case spl of
            Dup -> (Dup, n)
            Clean -> (Clean, n)
    step _ acc = acc
    args = [1..V.length chs - 1] >>= \i -> [(Clean, i), (Dup, i)]

compute :: Grid -> [Int]
compute rows | not (null rows) =
  adjustCol <$> foldr step [] firstReflections
  where
    t = V.tail rows
    step (spl, n) acc
      | all (checkReflection spl n) t = (spl, n) : acc
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
