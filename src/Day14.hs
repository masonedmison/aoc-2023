module Day14 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Paths_aoc2023 (getDataFileName)
import System.TimeIt (timeIt)
import Utils (transposeVec)

type Grid = V.Vector (V.Vector Char)

fromList :: [[Char]] -> Grid
fromList chs = V.fromList (V.fromList <$> chs)

shiftRow :: Int -> V.Vector (Int, Char) -> V.Vector Char -> V.Vector Char
shiftRow periods chs acc | not (null chs) = case V.head chs of
  (idx, 'O')
    | periods > 0 ->
        shiftRow periods (V.tail chs) updatedAcc
    where
      updatedAcc = V.update acc (V.fromList [(idx - periods, 'O'), (idx, '.')])
  (_, '.') -> shiftRow (periods + 1) (V.tail chs) acc
  _ -> shiftRow 0 (V.tail chs) acc
shiftRow _ _ acc = acc

runSpinCycle :: Grid -> Grid
runSpinCycle gr =
  V.reverse (V.reverse <$> spun gr)
  where
    spun =
      (tilt . transposeVec . V.reverse)
        . (tilt . transposeVec . V.reverse)
        . (tilt . transposeVec)
        . (tilt . transposeVec)
    tilt = fmap (\row -> shiftRow 0 (V.indexed row) row)

findCycle :: Grid -> (Int, Int)
findCycle gr =
  go 1 M.empty initialSpin
  where
    initialSpin = runSpinCycle gr
    go :: Int -> M.Map Grid Int -> Grid -> (Int, Int)
    go count prevStates gr = case seenElement of
      Just idx ->
        (count, idx)
      _ -> go (count + 1) (M.insert gr count prevStates) currSpin
      where
        seenElement = M.lookup gr prevStates
        currSpin = runSpinCycle gr

getLoad :: Grid -> Int
getLoad gr =
  V.sum $ processRow 0 <$> indexedAndTransposed
  where
    indexedAndTransposed = fmap (\row -> V.zip (V.fromList [length row, length row - 1 .. 1]) row) (transposeVec gr)
    processRow count row | not (null row) = case V.head row of
      (idx, 'O') -> processRow (idx + count) $ V.tail row
      _ -> processRow count $ V.tail row
    processRow count _ = count

compute :: Grid -> Int
compute gr =
  getLoad runCycles
  where
    (cycleEnd, cycleStart) = findCycle gr
    cycleLength = cycleEnd - cycleStart
    cyclesToRun = (1000000000 - cycleStart) `mod` cycleLength + cycleStart
    runCycles = foldl' (\grAcc f -> f grAcc) gr $ replicate cyclesToRun runSpinCycle

day14 :: IO ()
day14 = do
  inputLines <- lines <$> (getDataFileName "day14-input.txt" >>= readFile)
  let grid = fromList inputLines
  let result = compute grid
  timeIt $ print result
