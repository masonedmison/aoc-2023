{-# LANGUAGE TupleSections #-}

module Day10 where

import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Data.Tuple.All (Sel3 (..))
import qualified Data.Vector as V
import Paths_aoc2023 (getDataFileName)

type Coord = (Int, Int)

type LoopCoords = M.Map Coord Char

-- length of columns X length of rows
type UpperBounds = (Int, Int)

type Visited = S.Set Coord

type Maze = V.Vector (V.Vector Char)

fromList :: [[Char]] -> Maze
fromList chs =
  V.fromList $ V.fromList <$> chs

get :: Maze -> Coord -> Char
get mz (x, y) = mz V.! y V.! x

replace :: Maze -> Coord -> Char -> Maze
replace mz (x, y) ch = V.update mz (V.singleton (y, V.update (mz V.! y) (V.singleton (x, ch))))

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
  if validTransition' dir srcCh destCh
    then Just destCh
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
   in mapMaybe (\(dest, dir) -> fmap (dest,) (validTransition dir srcCh (get mz dest))) withDirs

findStart :: Maze -> Coord
findStart mz =
  case maybeS mz of
    Just v -> v
    Nothing -> error "expected 'S' starting point in maze."
  where
    findS = V.findIndex (== 'S')
    maybeS mz =
      do
        y <- V.findIndex (not . null . findS) mz
        x <- findS (mz V.! y)
        return (x, y)

getLoopCoords :: UpperBounds -> Maze -> LoopCoords
getLoopCoords bounds mz =
  go stChar stCoord (M.fromList [(s, 'S'), (stCoord, stChar)])
  where
    -- s = starting coordinate of S
    s = findStart mz
    -- by definition, s should have exactly two neighbors
    -- choose one to start go with (with count = 1) and record the other
    -- as the termination check
    -- we should add both stCoord, and s to visited upon starting the recursion of go
    ((stCoord, stChar) : (endCoord, _) : _) = getValidNeighbors bounds mz 'S' s
    go currChar currCoord visited
      | currCoord == endCoord = visited
      | otherwise =
          let neighbors = getValidNeighbors bounds mz currChar currCoord
              (nextCoord, nextChar) = head (filter (\(c, _) -> M.notMember c visited) neighbors)
           in go nextChar nextCoord (M.insert nextCoord nextChar visited)

scanLine :: V.Vector (Char, Coord) -> LoopCoords -> Int
scanLine line coords =
  sel3 (V.foldl (\(io, pc, c) entry -> adjustState entry io pc c) (False, ' ', 0) line)
  where
    isCorner char = char `elem` ['L', 'F', 'J', '7']
    inLoopCoords coord = M.member coord coords
    adjustState (ch, coord) inout prevCorner count = case ch of
      _ | isCorner ch && inLoopCoords coord -> case (prevCorner, ch) of
        ('F', 'J') -> (not inout, ' ', count)
        ('L', '7') -> (not inout, ' ', count)
        (_, 'L') -> (inout, 'L', count)
        (_, 'F') -> (inout, 'F', count)
        _ -> (inout, prevCorner, count)
      '|' | inLoopCoords coord -> (not inout, ' ', count)
      _ | inout && not (inLoopCoords coord) -> (inout, prevCorner, count + 1)
      _ -> (inout, prevCorner, count)

scanLines :: Maze -> LoopCoords -> Int
scanLines mz coords =
  V.foldr (\l accCount -> accCount + scanLine l coords) 0 indexedMaze
  where
    indexedMaze = V.map (\(y, xs) -> V.map (\(x, ch) -> (ch, (x, y))) (V.indexed xs)) (V.indexed mz)

parseIntoMaze :: IO Maze
parseIntoMaze =
  do
    inputLines <- lines <$> (getDataFileName "day10-input.txt" >>= readFile)
    putStrLn "This is what I read from input:"
    putStrLn $ unlines inputLines
    return (fromList inputLines)

adjustS :: Maze -> Maze
adjustS mz =
  replace mz sCoord 'J'
  where
    sCoord = findStart mz

day10 :: IO ()
day10 = do
  inputLines <- lines <$> (getDataFileName "day10-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let mz' = fromList inputLines
  let bounds = getBounds mz'
  let loopCoords = getLoopCoords bounds mz'
  let mz = adjustS mz'
  let enc = scanLines mz loopCoords
  print enc
