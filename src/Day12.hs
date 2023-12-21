module Day12 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text
    ( Parser, choice, many', many1, sepBy, digit, char, parseOnly )
import Utils (digitsToInt)
import System.TimeIt (timeIt)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)
import Data.List (intercalate)

-- current number of damaged springs, remaining row, groups
type Row = (Int, String, [Int])

type Input = (String, [Int])

type Key = (Int, String, [Int])
type Cache = M.Map Key Int

updateWithValue :: Cache -> Key -> Int -> (Cache, Int)
updateWithValue cache row v =
  let updated = M.insert row v cache
    in (updated, v)

computeRow :: Cache -> Row -> (Cache, Int)
computeRow cache k@(_, _, _) | isJust r = (cache, fromJust r)
  where
    r = M.lookup k cache
computeRow cache k@(n, '?':rest, cgs) = updateWithValue c2 k (v1 + v2)
  where 
    (c1, v1) = computeRow cache (n, '.':rest, cgs)
    (c2, v2) = computeRow c1(n, '#':rest, cgs)
computeRow cache (n, '#':rest, cgs) = computeRow cache (n + 1, rest, cgs)
computeRow cache k@(n, _, c:_) | n > c = updateWithValue cache k 0
computeRow cache (0, '.':rest, cgs) = computeRow cache (0, rest, cgs)
computeRow cache k@(n, '.':rest, c:cgs) 
  | n == c = computeRow cache (0, rest, cgs)
  | otherwise = updateWithValue cache k 0
computeRow cache k@(0, xs, [ ]) | all (`elem` ['?', '.']) xs = updateWithValue cache k 1
computeRow cache (n, "", [c]) 
  | n == c = (cache, 1)
  | otherwise = (cache, 0)
computeRow cache _ = (cache, 0) 

part1 :: [Input] -> Int
part1 inp = 
  sum $ snd <$> counts
  where
    counts = map (\(r, gs) -> computeRow M.empty (0, r, gs)) inp

part2 inp =
  part1 expanded
  where
    expanded = map (\(s, is) -> (intercalate "?" (replicate 5 s), concat (replicate 5 is))) inp

inputParser :: Parser Input
inputParser = do
  chs <- many1 (choice [char '.', char '?', char '#'])
  _ <- many' $ char ' '
  cgs' <- sepBy (many1 digit) (char ',')
  let cgs = digitsToInt <$> cgs'
  return (chs, cgs)

day12 :: IO ()
day12 = do
  inputLines <- T.lines <$> (getDataFileName "day12-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let csps' = parseOnly inputParser <$> inputLines
  csps <- either fail pure $ sequence csps'
  timeIt $ print $ part2 csps
