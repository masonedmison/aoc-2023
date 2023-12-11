{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Day05 where

import Data.Attoparsec.Text as P
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Fmt
import Paths_aoc2023 (getDataFileName)
import Utils (digitsToInt, skipLine)
import Data.List

type Lookup = Int -> Int
luEmpty :: Lookup
luEmpty = id

type LookupRev = Int -> Int
constLookupRev :: Int -> Int -> Int -> Lookup -> Lookup
constLookupRev d s r l k = let diff = k - d
                          in
                            if diff < r && diff >= 0 then s + diff
                            else l k

-- dest, src, range, orElse lookup
constLookup :: Int -> Int -> Int -> Lookup -> Lookup
constLookup d s r l k = let diff = k - s
                          in
                            if diff < r && diff >= 0 then d + diff
                            else l k

data Lookups = Lookups
  { seeds :: [Int],
    seedToSoil :: Int -> Int,
    soilToFertizlier :: Int -> Int,
    fertToWater :: Int -> Int,
    waterToLight :: Int -> Int,
    lightToTemp :: Int -> Int,
    tempToHumidity :: Int -> Int,
    humidityToLocation :: Int -> Int
  }

lupFns :: Lookups -> [Int -> Int]
lupFns ls = [seedToSoil ls, soilToFertizlier ls, fertToWater ls, waterToLight ls, lightToTemp ls, tempToHumidity ls, humidityToLocation ls]
locationToSeed, seedToLocation :: Lookups -> Int -> Int

seedToLocation ls = foldr1 (.) $ reverse $ lupFns ls
locationToSeed ls = 
  seedToSoil ls . soilToFertizlier ls . fertToWater ls . waterToLight ls . lightToTemp ls . tempToHumidity ls . humidityToLocation ls

lineParser :: Parser [Int]
lineParser =
  do
    chs <- sepBy (many1 digit) (many1 (char ' '))
    return (digitsToInt <$> chs)


lineToLookup, lineToLookupRev :: [Int] -> Lookup -> Lookup
lineToLookup (d : s : r : _) l =
  constLookupRev d s r l
lineToLookup [] l = l
lineToLookup l _ = error (fmt $ "expected a line with 3 elements, but got" +| l |+ ".")

lineToLookupRev (d : s : r : _) l =
  constLookupRev d s r l
lineToLookupRev [] l = l
lineToLookupRev l _ = error (fmt $ "expected a line with 3 elements, but got" +| l |+ ".")

lookupParser :: Bool -> Parser Lookup
lookupParser rev =
  do
    skipLine
    lines <- sepBy lineParser endOfLine
    let lup = if rev then lineToLookupRev else lineToLookup
    let lookup = foldr lup luEmpty lines
    return lookup

dataParser :: Bool -> Parser Lookups
dataParser rev =
  do
    seeds <- string "seeds:" *> many1 space *> lineParser
    many' endOfLine
    seedtof <- lookupParser rev
    soiltof <- lookupParser rev
    ftow <- lookupParser rev
    wtol <- lookupParser rev
    ltot <- lookupParser rev
    ttoh <- lookupParser rev
    htol <- lookupParser rev
    return
      Lookups
        { seeds = seeds,
          seedToSoil = seedtof,
          soilToFertizlier = soiltof,
          fertToWater = ftow,
          waterToLight = wtol,
          lightToTemp = ltot,
          tempToHumidity = ttoh,
          humidityToLocation = htol
        }

part1, part2, part2V2 :: Lookups -> IO ()
part1 lookups =
  let locs = minimum $ seedToLocation lookups <$> seeds lookups
    in
    print locs

expandSeeds :: [Int] -> [Int]
expandSeeds (x:y:rest) =
  [x..x + y - 1] ++ expandSeeds rest
expandSeeds _ = []

part2 lookups =
  let locs = minimum $ seedToLocation lookups <$> expandSeeds (seeds lookups)
    in
      print locs

seedRanges :: [Int] -> [(Int, Int)]
seedRanges (x:y:rest) =
  (x, y) : seedRanges rest
seedRanges _ = []

containsSeed :: Int -> [(Int, Int)] -> Bool

containsSeed s ((s', r):rest) =
  (s >= s' && s + s' < r) || containsSeed s rest
containsSeed _ [] = False


part2V2 lookups =
  let cands = [0..1000000000]
      ranges = seedRanges $ seeds lookups
      locMapsToSeed x = containsSeed (locationToSeed lookups x) ranges
      r = locationToSeed lookups 46
      first = find locMapsToSeed cands
      in
        do
        print r
        case first of
          Nothing -> putStrLn "no result found"
          Just v -> print v



day05 :: IO ()
day05 = do
  input <- getDataFileName "day05-input.txt" >>= TIO.readFile
  putStrLn "This is what I read from input:"
  TIO.putStrLn input
  lookups <- either fail pure $ parseOnly (dataParser True) input
  part2V2 lookups