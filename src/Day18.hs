module Day18 where

import Paths_aoc2023 (getDataFileName)
import Data.List.Extra (splitOn, scanl')
import Numeric (readHex)
import Data.Char (digitToInt)

type Input = [(Char, Int, String)]
type Coord = (Int, Int)
type Lagoon = [Coord]
digTrench :: Input -> Lagoon
digTrench = scanl' digCorner (0, 0)
  where
    digCorner (x, y) ('R', n, _) = (x + n, y)
    digCorner (x, y) ('L', n, _) = (x - n, y)
    digCorner (x, y) ('U', n, _) = (x, y - n)
    digCorner (x, y) ('D', n, _) = (x, y + n)
    digCorner _ _ = error "ruh roh!"

area :: Lagoon -> Int
area lag =
   1 + perimeter `div` 2 + (abs . (`div` 2) . sum . zipWith crossProduct lag $ tail lag ++ [head lag])
  where
    crossProduct (x0, y0) (x1, y1) = x0 * y1 - x1 * y0
    dist (x0, y0) (x1, y1) = abs (x0 - x1) + abs (y0 - y1)
    perimeter = sum . zipWith dist lag $ (tail lag ++ [head lag])

convertInput :: Input -> Input
convertInput inp =
  fromOld <$> inp
  where
    fromOld (_, _, rgb@(_:hexDir)) =
      let (n, _) = head . readHex . take 5 $ hexDir
          dir = ['R', 'D', 'L', 'U'] !! digitToInt (last hexDir)
        in (dir, n, rgb)
    fromOld _ = error "ruh roh."

part1, part2 :: Input -> Int
part1 inp =
  area trenchCorners
  where
    trenchCorners = digTrench inp

part2 inp =
  area $ trenchCorners inp
  where
    trenchCorners = digTrench . convertInput

parseInput :: String -> Input
parseInput =
  fmap (fmap go) segs
  where
    segs st = splitOn [' '] <$> lines st
    go ((fc:_):n:c:_) = (fc, read n :: Int, tail $ init c)
    go _ = error "expected 3 elements."

day18 :: IO ()
day18 = do
  inputS <- getDataFileName "day18-input.txt" >>= readFile
  let inp = parseInput inputS
  let result = part2 inp
  print result
