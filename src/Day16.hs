{-# LANGUAGE TupleSections #-}

module Day16 where

import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Paths_aoc2023 (getDataFileName)
import System.TimeIt (timeIt)

type Coord = (Int, Int)

fromDir :: Coord -> Direction -> Coord
fromDir (x, y) N = (x, y - 1)
fromDir (x, y) S = (x, y + 1)
fromDir (x, y) E = (x + 1, y)
fromDir (x, y) W = (x - 1, y)

type Input = V.Vector (V.Vector Char)

getCoord :: Input -> Coord -> Char
getCoord inp (x, y) = inp V.! y V.! x

getNextFromCoordAndDir :: Input -> Coord -> Direction -> Maybe (Coord, Direction)
getNextFromCoordAndDir inp c d
  | nx >= 0 && nx < length (V.head inp) && ny >= 0 && ny < length inp = Just (nc, d)
  | otherwise = Nothing
  where
    nc@(nx, ny) = fromDir c d

data Direction = N | S | E | W deriving (Eq, Show)

data Status = Count | Halt

type Visited = M.Map Coord [Direction]

vLookup :: Visited -> Coord -> Direction -> Status
vLookup v c d =
  maybe Count visitedSt visitedDirs
  where
    visitedSt dirs
      | d `elem` dirs = Halt
      | otherwise = Count
    visitedDirs = M.lookup c v

getNextMoves :: Input -> Coord -> Direction -> [(Coord, Direction)]
getNextMoves inp c d
  | ch == '.' = toList $ nextFromDir d
  | ch == '\\' = case d of
      E -> toList $ nextFromDir S
      W -> toList $ nextFromDir N
      S -> toList $ nextFromDir E
      N -> toList $ nextFromDir W
  | ch == '/' = case d of
      E -> toList $ nextFromDir N
      W -> toList $ nextFromDir S
      S -> toList $ nextFromDir W
      N -> toList $ nextFromDir E
  | ch == '-' = case d of
      d | d `elem` [W, E] -> toList $ nextFromDir d
      d | d `elem` [N, S] -> catMaybes [nextFromDir E, nextFromDir W]
      _ -> []
  | ch == '|' = case d of
      d | d `elem` [N, S] -> toList $ nextFromDir d
      d | d `elem` [W, E] -> catMaybes [nextFromDir N, nextFromDir S]
      _ -> []
  | otherwise = error "unexpected character."
  where
    nextFromDir = getNextFromCoordAndDir inp c
    ch = getCoord inp c

processCell :: Input -> Visited -> Coord -> Direction -> (Visited, [(Coord, Direction)])
processCell inp vis c d = case vLookup vis c d of
  Count -> (updateCountSt, nexts)
  Halt -> (vis, [])
  where
    nexts = getNextMoves inp c d
    updateCountSt = M.insert c [d] vis

traverseI :: (Coord, Direction) -> Input -> Int
traverseI startCoord inp =
  length $ go M.empty startCoord
  where
    go :: Visited -> (Coord, Direction) -> Visited
    go st (c, d) =
      foldr (flip go) nextSt nexts
      where
        (nextSt, nexts) = processCell inp st c d

part1, part2 :: Input -> Int
part1 = traverseI ((0, 0), E)
part2 inp =
  foldr step 0 indicesToCheck
  where
    nCols = length (V.head inp) - 1
    nRows = length inp - 1
    upIndices = map (,N) $ zip [0 .. nCols] (repeat nRows)
    downIndices = map (,S) $ zip [0 .. nCols] (repeat 0)
    rightIndices = map (,E) $ zip (repeat 0) [0 .. nRows]
    leftIndices = map (,W) $ zip (repeat nCols) [0 .. nRows]
    indicesToCheck = upIndices ++ downIndices ++ rightIndices ++ leftIndices
    step start maxAcc =
      let sum = traverseI start inp
       in max sum maxAcc

day16 :: IO ()
day16 = do
  inputLines <- lines <$> (getDataFileName "day16-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let input = V.fromList (V.fromList <$> inputLines)
  let result = part2 input
  timeIt $ print result
