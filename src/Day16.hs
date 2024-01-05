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

data Status = Count | NoCount | Halt

type Visited = M.Map Coord [Direction]

vLookup :: Visited -> Coord -> Direction -> Status
vLookup v c d =
  maybe Count visitedSt visitedDirs
  where
    visitedSt dirs
      | d `elem` dirs = Halt
      | otherwise = NoCount
    visitedDirs = M.lookup c v

data TSTate = TSTate {visited :: Visited, count :: Int} deriving (Show)

type Cache = M.Map (Coord, Direction) Int

new :: TSTate
new =
  TSTate M.empty 0

withCount :: TSTate -> (Int -> Int) -> TSTate
withCount st f =
  let currCount = count st
   in st {count = f currCount}

withVisited :: TSTate -> (Visited -> Visited) -> TSTate
withVisited st f =
  let currVis = visited st
   in st {visited = f currVis}

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

processCell :: Input -> TSTate -> Coord -> Direction -> (TSTate, [(Coord, Direction)])
processCell inp st c d = case vLookup (visited st) c d of
  Count -> (updateCountSt, nexts)
  NoCount -> (withVisited st (M.adjust (d :) c), nexts)
  Halt -> (st, [])
  where
    nexts = getNextMoves inp c d
    updateCountSt = withCount (withVisited st (M.insert c [d])) (+ 1)

traverseI :: (Coord, Direction) -> Input -> Int
traverseI startCoord inp =
  count $ go new startCoord
  where
    go :: TSTate -> (Coord, Direction) -> TSTate
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
    upIndices = map (,N) $ zip [0 .. nCols] (repeat 0)
    downIndices = map (,S) $ zip [0 .. nCols] (repeat 0)
    rightIndices = map (,E) $ zip (repeat 0) [0 .. nRows]
    leftIndices = map (,W) $ zip (repeat 0) [0 .. nRows]
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
