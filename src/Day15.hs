module Day15 where

import Data.Char
import Data.Foldable (foldl')
import Data.List.Split
import Data.Map.Strict (Map, adjust, empty, insert, toList, (!))
import Paths_aoc2023 (getDataFileName)

type BoxEntry = (String, Int)

type Boxes = Map Int [BoxEntry]

new :: Boxes
new = foldl' (\acc i -> insert i [] acc) empty [0 .. 255]

type Input = [String]

parseInput :: String -> Input
parseInput = splitOn "," . head . lines

hash :: String -> Int
hash =
  foldl' step 0
  where
    step acc ch = ((acc + ord ch) * 17) `mod` 256

processEntry :: Boxes -> String -> Boxes
processEntry bs =
  go . span (`notElem` ['-', '='])
  where
    go (label, "-") = adjust (filter ((/= label) . fst)) (hash label) bs
    go (label, _ : n)
      | label `elem` labs = adjust (const $ before ++ [(label, read n)] ++ after) hashed bs
      | otherwise = adjust (++ [(label, read n)]) hashed bs
      where
        hashed = hash label
        entries = bs ! hashed
        labs = fmap fst entries
        (before, _ : after) = span ((/= label) . fst) entries
    go _ = error "ruh roh"

calculateBoxTotal :: Boxes -> Int
calculateBoxTotal bs =
  sum $ toList bs >>= uncurry calculateBox
  where
    calculateBox boxN bx =
      (\(i, (_, len)) -> (boxN + 1) * i * len) <$> zip [1 ..] bx

day15 :: IO ()
day15 = do
  input <- parseInput <$> (getDataFileName "day15-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  let bs = foldl' processEntry new input
  let result = calculateBoxTotal bs
  print result
