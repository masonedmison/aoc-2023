module Day16 where

import Data.Foldable (Foldable (toList))
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import qualified Data.Vector as V
import Debug.Trace (trace)
import Paths_aoc2023 (getDataFileName)

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

data State = State {visited :: Visited, count :: Int} deriving (Show)

new :: State
new =
  State M.empty 0

withCount :: State -> (Int -> Int) -> State
withCount st f =
  let currCount = count st
   in st {count = f currCount}

withVisited :: State -> (Visited -> Visited) -> State
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

processCell :: Input -> State -> Coord -> Direction -> (State, [(Coord, Direction)])
processCell inp st c d = case vLookup (visited st) c d of
  Count -> (updateCountSt, nexts)
  NoCount -> (withVisited st (M.adjust (d :) c), nexts)
  Halt -> (st, [])
  where
    nexts = getNextMoves inp c d
    updateCountSt = withCount (withVisited st (M.insert c [d])) (+ 1)

traverseI :: Input -> Int
traverseI inp =
  count $ go new initial
  where
    initial = ((0, 0), E)
    go :: State -> (Coord, Direction) -> State
    go st (c, d) =
      foldr step nextSt nexts
      where
        (nextSt, nexts) =
          processCell inp st c d
        step (c, d) accSt = go accSt (c, d)

day16 :: IO ()
day16 = do
  inputLines <- lines <$> (getDataFileName "day16-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let input = V.fromList (V.fromList <$> inputLines)
  let result = traverseI input
  print result
