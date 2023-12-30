{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Day17 where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Paths_aoc2023 (getDataFileName)
import System.TimeIt (timeIt)

type Grid = V.Vector (V.Vector Int)

gLookup :: Coord -> Grid -> Int
gLookup (x, y) gr = gr V.! y V.! x

instance DijkstraGraph Grid where
  edges gr (x, y) =
    (\(c, dir) -> (c, Dist (gLookup c gr), dir)) <$> neighborIndices
    where
      maxCol = length (V.head gr)
      maxRow = length gr
      neighborIndices =
        filter
          (\((x, y), _) -> x >= 0 && x < maxCol && y >= 0 && y < maxRow)
          [((x - 1, y), W), ((x + 1, y), E), ((x, y - 1), N), ((x, y + 1), S)]

type Coord = (Int, Int)

type DistMap = M.Map (Coord, DirCount) Distance

data Direction = N | S | W | E deriving (Eq, Show, Ord)

isReverse :: Direction -> Direction -> Bool
isReverse N S = True
isReverse S N = True
isReverse E W = True
isReverse W E = True
isReverse _ _ = False

type DirCount = (Direction, Int)

addOne :: DirCount -> Direction -> DirCount
addOne (d1, c1) d2 | d1 == d2 = (d2, c1 + 1)
addOne _ d2 = (d2, 1)

data DijkstraState = DijkstraState
  { visited :: M.Map Coord DirCount,
    distances :: DistMap,
    queue :: H.MinPrioHeap Distance (Coord, DirCount)
  }

initSt :: Coord -> DijkstraState
initSt src =
  let initialDistances = M.singleton (src, (E, 0)) (Dist 0)
      initialQ = H.fromList [(Dist 0, (src, (E, 0)))]
   in DijkstraState M.empty initialDistances initialQ

data Distance = Dist Int | Infinity deriving (Eq, Show)

instance Semigroup Distance where
  (<>) (Dist x) (Dist y) = Dist $ x + y
  (<>) _ _ = Infinity

instance Ord Distance where
  Infinity <= Infinity = True
  Infinity <= Dist _ = False
  Dist _ <= Infinity = True
  Dist x <= Dist y = x <= y

(!??) :: DistMap -> (Coord, DirCount) -> Distance
(!??) distMap c = fromMaybe Infinity $ M.lookup c distMap

class DijkstraGraph graph where
  edges :: graph -> Coord -> [(Coord, Distance, Direction)]

dijkstra :: Coord -> Coord -> Grid -> Distance
dijkstra src end gr =
  minimum (fmap (\dc -> finalDists !?? (end, dc)) finalIndices)
  where
    finalIndices = map (S,) [4 .. 10] ++ map (E,) [4 .. 10]
    finalDists = processQ initialSt
    initialSt = initSt src
    prune vis coord dc =
      maybe False shouldPrune dc'
      where
        dc' = M.lookup coord vis
        shouldPrune dc' = dc' == dc

    processQ :: DijkstraState -> DistMap
    processQ ds@(DijkstraState v0 d0 q0) = case H.view q0 of
      Nothing -> d0
      Just ((_, (c, (_, dirCount))), _) | c == end && dirCount >= 4 -> d0
      Just ((_, (c, d)), q1) | prune v0 c d -> processQ ds {queue = q1}
      Just ((_, (coord, cdc@(currDir, currDirCount))), q1) ->
        let v1 = M.insert coord cdc v0
            allNeighbors = edges gr coord
            allNeighborsWithCurDc = fmap (\(c, d, dir) -> (c, d, addOne cdc dir)) allNeighbors
            filterByDir (d, n)
              | currDirCount > 0 && currDirCount < 4 = d == currDir
              | otherwise = n <= 10
            validNeighbors = filter (\(n, _, ndc@(d, _)) -> not (prune v1 n ndc) && filterByDir ndc && not (isReverse currDir d)) allNeighborsWithCurDc
         in 
          processQ $
              foldl' (foldNeighbor coord cdc) (DijkstraState v1 d0 q1) validNeighbors

foldNeighbor :: Coord -> DirCount -> DijkstraState -> (Coord, Distance, DirCount) -> DijkstraState
foldNeighbor currCoord currDir ds@(DijkstraState _ d0 q0) (neighborN, neighborDist, ndc@(neighborDir, neighborDirCount))
  | altDistance < d0 !?? (neighborN, (neighborDir, neighborDirCount)) =
      ds
        { distances = M.insert (neighborN, (neighborDir, neighborDirCount)) altDistance d0,
          queue = H.insert (altDistance, (neighborN, ndc)) q0
        }
  | otherwise = ds
  where
    altDistance = d0 !?? (currCoord, currDir) <> neighborDist

parseInput :: String -> Grid
parseInput st =
  let ls = fmap (digitToInt <$>) $ lines st
   in V.fromList (V.fromList <$> ls)

part1 :: Grid -> Distance
part1 gr =
  dijkstra (0, 0) (maxX - 1, maxY - 1) gr
  where
    maxX = length (V.head gr)
    maxY = length gr

day17 :: IO ()
day17 = do
  input <- getDataFileName "day17-input.txt" >>= readFile
  let grid = parseInput input
  let result = part1 grid
  timeIt $ print result
