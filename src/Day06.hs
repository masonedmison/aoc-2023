module Day06 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Attoparsec.Text
import Utils (digitsToInt)

data Race = Race {
  time :: Int,
  distance :: Int
}

dataParser :: Parser Race
dataParser =
  do
    string "Time:"
    skipSpace
    times <- many1 digit
    let timeInts = digitsToInt times
    many' endOfLine
    string "Distance:"
    skipSpace
    dists <- many1 digit
    let distInts = digitsToInt dists
    return (Race {time = timeInts, distance = distInts})

-- length held, time
calculateDistance :: Int -> Int -> Int
calculateDistance lh t = (t - lh) * lh

calculateNumWins :: Race -> Int
calculateNumWins r =
  length (increaseUntilLose 0 [])
  where
    d = distance r
    t = time r
    -- length held, acc
    increaseUntilLose :: Int -> [Int] -> [Int]
    increaseUntilLose lh acc
      | lh >= t = acc
      | otherwise =
        let travelled = calculateDistance lh t
        in
          if travelled > d then increaseUntilLose (lh + 1) (lh:acc)
          else increaseUntilLose (lh + 1) acc


day06 :: IO ()
day06 = do
  inputLines <- getDataFileName "day06-input.txt" >>= TIO.readFile
  putStrLn "This is what I read from input:"
  TIO.putStrLn inputLines
  let racesEither = parseOnly dataParser inputLines
  races <- either fail pure racesEither
  let productOfWins = calculateNumWins races
  print productOfWins
