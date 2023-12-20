{-# LANGUAGE TupleSections #-}
module Day11 where

import Paths_aoc2023 (getDataFileName)
import Data.List (delete, transpose)
import qualified Data.Set as S
import Debug.Trace (trace)

type Coord = (Int, Int)

type Grid = [[Char]]
galaxies :: Grid -> [Coord]
galaxies gr =
  [(x, y) | (row, y) <- zip gr [0..],
       ('#', x) <- zip row [0..]
  ]

expandSpace :: Grid -> Int -> Coord -> Coord
expandSpace gr by (x, y) =
  (
    x + (length (filter (<x) noGalaxyCols) * (by - 1)),
    y + (length (filter (<y) noGalaxyRows) * (by - 1))
    )
  where
    noGalaxyRows = snd <$> filter (\(row, _) -> all (=='.') row) (zip gr [0..])
    noGalaxyCols = snd <$> filter (\(col, _) -> all (=='.') col) (zip (transpose gr) [0..])

manhattenDist :: Coord -> Coord -> Int
manhattenDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

getDistances :: [Coord] -> Int
getDistances coords =
  trace ("num of allDistinct pairs: " ++ show (length allDistinctPairs) )
  sum (uncurry manhattenDist <$> S.toList allDistinctPairs)
  where
    allDistinctPairs = S.fromList $ foldr (\c acc ->
      let pairs = [(c,)] <*> delete c coords
          filtered = filter (\p -> (snd p, fst p) `notElem` acc) pairs
            in
            filtered ++ acc
      ) [] coords

computeAnswer :: Grid -> Int -> Int
computeAnswer gr by =
  let gCoords = galaxies gr
      expandedCoords = expandSpace gr by <$> gCoords
      in
        trace ("getDistances:\n\tgCoords: " ++ show gCoords ++ "\n\texpandedCoords: " ++ show expandedCoords) getDistances expandedCoords

day11 :: IO ()
day11 = do
  inputLines <- lines <$> (getDataFileName "day11-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines inputLines
  let ans = computeAnswer inputLines 1000000
  print ans
