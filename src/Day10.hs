{-# LANGUAGE TupleSections #-}
module Day10 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Vector as V
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)

type Coord = (Int, Int)

-- length of columns X length of rows
type UpperBounds = (Int, Int)

type Visited = S.Set Coord

type Maze = V.Vector (V.Vector Char)
fromList :: [[Char]] -> Maze
fromList chs =
  V.fromList $ V.fromList <$> chs

get :: Maze -> Coord -> Char
get mz (x, y) = mz V.! y V.! x

getBounds :: Maze -> UpperBounds
getBounds mz =
  (length (V.head mz), length mz)

data Direction = N | S | W | E | Standstill deriving (Eq, Show)

validEastDirs, validWestDirs, validNorthDirs, validSouthDirs :: Char -> Bool
validEastDirs chR = chR `elem` ['-', '7', 'J', 'S']
validWestDirs chR = chR `elem` ['-', 'L', 'F', 'S']
validNorthDirs chR = chR `elem` ['|', 'F', '7', 'S']
validSouthDirs chR = chR `elem` ['|', 'L', 'J', 'S']

validByDir :: Char -> Direction -> Bool
validByDir ch dir = case dir of
  E -> validEastDirs ch
  W -> validWestDirs ch
  S -> validSouthDirs ch
  N -> validNorthDirs ch
  _ -> False

validTransition' :: Direction -> Char -> Char -> Bool
validTransition' dir chL chR = case (chL, chR) of
  (_, '.') -> False
  ('.', _) -> False
  ('S', _) -> validByDir chR dir
  ('F', _) -> case dir of
    E -> validEastDirs chR
    S -> validSouthDirs chR
    _ -> False
  ('7', _) -> case dir of
    W -> validWestDirs chR
    S -> validSouthDirs chR
    _ -> False
  ('L', _) -> case dir of
    N -> validNorthDirs chR
    E -> validEastDirs chR
    _ -> False
  ('J', _) -> case dir of
    N -> validNorthDirs chR
    W -> validWestDirs chR
    _ -> False
  ('|', _) -> case dir of
    N -> validNorthDirs chR
    S -> validSouthDirs chR
    _ -> False
  ('-', _) -> case dir of
    E -> validEastDirs chR
    W -> validWestDirs chR
    _ -> False
  _ -> False
  

validTransition :: Direction -> Char -> Char -> Maybe Char
validTransition dir srcCh destCh =
    if validTransition' dir srcCh destCh then Just destCh
    else Nothing

findNeighbors :: UpperBounds -> Coord -> [Coord]
findNeighbors (xLen, yLen) (x, y) =
  filter (\(x, y) -> (x >= 0 && x < xLen) && (y >= 0 && y < yLen)) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

dirFromCoord :: Coord -> Coord -> Direction
dirFromCoord (srcX, srcY) (destX, destY)
  | destX < srcX = W
  | destX > srcX = E
  | destY < srcY = N
  | destY > srcY = S
  | otherwise = Standstill

getValidNeighbors :: UpperBounds -> Maze -> Char -> Coord -> [(Coord, Char)]
getValidNeighbors ubounds mz srcCh coord =
  let neighbors = findNeighbors ubounds coord
      withDirs = zip neighbors (dirFromCoord coord <$> neighbors)
    in
      -- trace ("mapMaybe with withDirs = " ++ show withDirs)
      mapMaybe (\(dest, dir) -> fmap (dest,) (validTransition dir srcCh (get mz dest))) withDirs
      -- head (filter (\(c, _) -> S.notMember c visited) valids)

findStart :: Maze -> Coord
findStart mz =
  case maybeS mz of
    Just v -> v
    Nothing -> error "expected 'S' starting point in maze."
  where
    findS = V.findIndex (=='S')
    maybeS mz =
      do
      y <- V.findIndex (not . null . findS) mz
      x <- findS (mz V.! y)
      return (x, y)


findLengthOfLoop :: Maze -> Int
findLengthOfLoop mz =
  trace ("starting go with stChar: " ++ show stChar ++ " and stCoord: " ++ show stCoord) 
  go stChar stCoord (S.fromList[s, stCoord]) 1
  where
    bounds = getBounds mz
    -- st = starting coordinate of S
    s = findStart mz
    -- by definition, s should have exactly two neighbors
    -- choose one to start go with (with count = 1) and record the other
    -- as the termination check
    -- we should add both stCoord, and s to visited upon starting the recursion of go
    ((stCoord, stChar):(endCoord, _):_) = getValidNeighbors bounds mz 'S' s
    go currChar currCoord visited count
      | currCoord == endCoord && count /= 0 = count + 1
      | otherwise =
        -- trace ("Calling getValidNeighors with currChar: " ++ show currChar ++ " currCoord: " ++ show currCoord ++ " visited: " ++ show visited) 
        let neighbors = getValidNeighbors bounds mz currChar currCoord
            -- trace ("calling head with neighbors: " ++ show neighbors ++ " visited: " ++ show visited)
            (nextCoord, nextChar) =  (head (filter (\(c, _) -> S.notMember c visited) neighbors))
          in
            --trace ("recursing in go with nextChar: " ++ show nextChar ++ " nextCoord: " ++ show nextCoord) 
            go nextChar nextCoord (nextCoord `S.insert` visited) count + 1

parseIntoMaze :: IO Maze
parseIntoMaze =
  do
    inputLines <- lines <$> (getDataFileName "day10-input.txt" >>= readFile)
    putStrLn "This is what I read from input:"
    putStrLn $ unlines inputLines
    return (fromList inputLines)

day10 :: IO ()
day10 = do
  inputLines <- lines <$> (getDataFileName "day10-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let mz = fromList inputLines
  let loopLen = findLengthOfLoop mz
  let farthest = ceiling (fromIntegral loopLen / 2)
  print farthest
