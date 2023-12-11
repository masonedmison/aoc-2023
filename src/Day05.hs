{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use tuple-section" #-}

module Day05 where

import Data.Attoparsec.Text as P
import qualified Data.Text.IO as TIO
import Fmt
import Paths_aoc2023 (getDataFileName)
import Utils (digitsToInt, skipLine)

type Lookup = [(Int, Int)] -> [(Int, Int)]

luEmpty :: Lookup
luEmpty = id

constLookup :: (Int, Int, Int) -> Lookup -> Lookup
constLookup = mapRanges

data Lookups = Lookups
  { seeds :: [Int],
    seedToSoil :: Lookup,
    soilToFertizlier :: Lookup,
    fertToWater :: Lookup,
    waterToLight :: Lookup,
    lightToTemp :: Lookup,
    tempToHumidity :: Lookup,
    humidityToLocation :: Lookup
  }

lupFns :: Lookups -> [Lookup]
lupFns ls = [seedToSoil ls, soilToFertizlier ls, fertToWater ls, waterToLight ls, lightToTemp ls, tempToHumidity ls, humidityToLocation ls]

seedToLocation :: Lookups -> [(Int, Int)] -> [(Int, Int)]
seedToLocation ls = foldr1 (.) $ reverse $ lupFns ls

lineParser :: Parser [Int]
lineParser =
  do
    chs <- sepBy (many1 digit) (many1 (char ' '))
    return (digitsToInt <$> chs)

lineToLookup :: [Int] -> Lookup -> Lookup
lineToLookup (d : s : r : _) l = constLookup (d, s, r) l
lineToLookup [] l = l
lineToLookup l _ = error (fmt $ "expected a line with 3 elements, but got" +| l |+ ".")

lookupParser :: Parser Lookup
lookupParser =
  do
    skipLine
    lines <- sepBy lineParser endOfLine
    let lup = lineToLookup
    let lookup = foldr lup luEmpty lines
    return lookup

mapRanges :: (Int, Int, Int) -> Lookup -> Lookup
mapRanges m d l =
  l >>= mapRange m d

mapRange :: (Int, Int, Int) -> Lookup -> (Int, Int) -> [(Int, Int)]
mapRange (md, ms, mr) d (s, r)
  -- total mapping
  | s >= ms && (s + r) <= (ms + mr) =
      let diff = md - ms
       in [(s + diff, r)]
  -- potential overlaps
  | otherwise =
      let (overLo, overHi) = (max ms s, min (s + r) (ms + mr) - 1)
          diff = md - ms
       in -- no overlap
          if overHi <= overLo
            then d [(s, r)] -- no mapping here...
            else -- calculate overlap

              let overMapping = (overLo + diff, (overHi - overLo) + 1)
                  topSNumInRange = (s + r) - 1
               in -- is there one left, or two?
                  let (bl, bh) = (min overLo s, overLo - s)
                      (tl, th) = (min overHi topSNumInRange, topSNumInRange - overHi)
                   in case (bh, th) of
                        (_, _) | bh > 0 && th > 0 -> overMapping : d [(bl, bh), (tl, th)]
                        (_, _) | bh > 0 -> overMapping : d [(bl, bh)]
                        (_, _) | th > 0 -> overMapping : d [(tl, th)]
                        _ -> d [(s, r)]

dataParser :: Parser Lookups
dataParser =
  do
    seeds <- string "seeds:" *> many1 space *> lineParser
    many' endOfLine
    seedtof <- lookupParser
    soiltof <- lookupParser
    ftow <- lookupParser
    wtol <- lookupParser
    ltot <- lookupParser
    ttoh <- lookupParser
    htol <- lookupParser
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

part1, part2 :: Lookups -> IO ()
part1 lookups =
  let locs = minimum $ seedToLocation lookups <$> fmap (,1) $ seeds lookups
   in print locs

seedRanges :: [Int] -> [[(Int, Int)]]
seedRanges (x : y : rest) =
  [(x, y)] : seedRanges rest
seedRanges _ = []

part2 lookups =
  let locs = seedToLocation lookups <$> seedRanges (seeds lookups)
      m = minimum $ concat locs
   in print m

day05 :: IO ()
day05 = do
  input <- getDataFileName "day05-input.txt" >>= TIO.readFile
  putStrLn "This is what I read from input:"
  TIO.putStrLn input
  lookups <- either fail pure $ parseOnly dataParser input
  part2 lookups