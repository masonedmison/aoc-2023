{-# LANGUAGE TupleSections #-}

module Day03 where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (find)
import Data.Ix (inRange)
import qualified Data.Map.Strict as Map
import Data.Set (union)
import qualified Data.Set as Set
import Paths_aoc2023 (getDataFileName)

digitsToInt :: [Char] -> Int
digitsToInt chs =
  snd $ foldl (\(base, acc) i -> (base * 10, i * base + acc)) (1, 0) (digitToInt <$> reverse chs)

type Indexed2dList = [([(Char, Int)], Int)]

-- XRange, Y
type IdxRange = ((Int, Int), Int)

processRow :: Int -> [(Char, Int)] -> [(Char, Int)] -> [(Int, IdxRange)]
processRow yIdx [] running
  | not (null running) = [(digitsToInt $ map fst running, (xRange, yIdx))]
  | otherwise = []
  where
    xRange = (snd (head running), snd (last running))
processRow yIdx ((c, i) : xs) running
  | isDigit c = processRow yIdx xs (running ++ [(c, i)])
  | not (null running) = (digitsToInt $ map fst running, (xRange, yIdx)) : processRow yIdx xs []
  | otherwise = processRow yIdx xs []
  where
    xRange = (snd (head running), snd (last running))

buildNumWithRange :: Indexed2dList -> [(Int, IdxRange)]
buildNumWithRange [] = []
buildNumWithRange ((x, yIdx) : xs) =
  processRow yIdx x [] ++ buildNumWithRange xs

isSym :: Char -> Bool
isSym ch
  | ch /= '.' && not (isDigit ch) = True
  | otherwise = False

buildSymSet :: Indexed2dList -> Set.Set (Int, Int)
buildSymSet [] = Set.empty
buildSymSet ((x, yIdx) : xs) =
  processRow x Set.empty `union` buildSymSet xs
  where
    processRow :: [(Char, Int)] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
    processRow [] acc = acc
    processRow ((c, i) : xs') acc
      | isSym c = processRow xs' (Set.insert (i, yIdx) acc)
      | otherwise = processRow xs' acc

asterikCoords :: Indexed2dList -> Map.Map (Int, Int) [Int]
asterikCoords [] = Map.empty
asterikCoords ((x, yIdx) : xs) =
  processRow x Map.empty `Map.union` asterikCoords xs
  where
    processRow :: [(Char, Int)] -> Map.Map (Int, Int) [Int] -> Map.Map (Int, Int) [Int]
    processRow [] acc = acc
    processRow ((c, i) : xs') acc
      | c == '*' = processRow xs' (Map.insert (i, yIdx) [] acc)
      | otherwise = processRow xs' acc

calcNeighborCoords :: (Int, Int) -> IdxRange -> [(Int, Int)]
calcNeighborCoords (xUpper, yUpper) ((xLow, xHi), y) =
  let xRange = [xLow - 1 .. xHi + 1]
   in filter (\(x, y') -> inRange (0, xUpper) x && inRange (0, yUpper) y') $
        map (,y - 1) xRange
          ++ map (,y + 1) xRange
          ++ [(xLow - 1, y), (xHi + 1, y)]

processNumsP1 :: (Int, Int) -> [(Int, IdxRange)] -> Set.Set (Int, Int) -> [Int]
processNumsP1 upperBounds nums symsCoords =
  do
    (n, idxRange) <- nums
    let found = find (`Set.member` symsCoords) (calcNeighborCoords upperBounds idxRange)
    [n | not (null found)]

processNumsP2 :: (Int, Int) -> [(Int, IdxRange)] -> Map.Map (Int, Int) [Int] -> [Int]
processNumsP2 upperBounds nums astCoords =
  fmap (\(_, x : y : _) -> x * y) (Map.toAscList (Map.filter (\x -> length x == 2) foldedCoords))
  where
    allCoords :: [(Int, [(Int, Int)])]
    allCoords =
      do
        (n, idxRange) <- nums
        return (n, calcNeighborCoords upperBounds idxRange)
    foldedCoords =
      foldr
        ( \(n, c) acc ->
            foldr (Map.adjust (n :)) acc c
        )
        astCoords
        allCoords

part1 :: Indexed2dList -> IO ()
part1 withIdx =
  do
    let nums = buildNumWithRange withIdx
    let symCoords = buildSymSet withIdx
    let partNums = processNumsP1 (length withIdx - 1, length (fst (head withIdx)) - 1) nums symCoords
    let summed = sum partNums
    print nums
    print symCoords
    print partNums
    print summed

part2 :: Indexed2dList -> IO ()
part2 withIdx =
  do
    let nums = buildNumWithRange withIdx
    let astCoords = asterikCoords withIdx
    let partNums = processNumsP2 (length withIdx - 1, length (fst (head withIdx)) - 1) nums astCoords
    let summed = sum partNums
    print nums
    print astCoords
    print partNums
    print summed

day03 :: IO ()
day03 = do
  input <- lines <$> (getDataFileName "day03-input.txt" >>= readFile)
  putStrLn "This is what I read from input:"
  putStrLn $ unlines input
  let withIdx :: Indexed2dList = zip (fmap (\x -> zip x [0 ..]) input) [0 ..]
  part2 withIdx
-- part1 withIdx
