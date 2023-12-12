{-# LANGUAGE TupleSections #-}

module Day07 where

import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)
import Data.List (group, groupBy, nub, partition, sort, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Paths_aoc2023 (getDataFileName)
import Utils (digitsToInt)

data CardType = J | Num Int | T | Q | K | A deriving (Eq, Ord, Show)

fromChars :: [Char] -> [CardType]
fromChars (h : rest) = case h of
  'A' -> A : fromChars rest
  'K' -> K : fromChars rest
  'Q' -> Q : fromChars rest
  'J' -> J : fromChars rest
  'T' -> T : fromChars rest
  n | isDigit n -> Num (digitToInt n) : fromChars rest
  ch -> error ("unexpected char of: " ++ [ch])
fromChars [] = []

data HandCat
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfKind
  | FullHouse
  | FourOfKind
  | FiveOfKind
  deriving (Eq, Ord, Show)

type Hand = [CardType]

type HandWithScore = (Hand, Int)

handParser :: Parser HandWithScore
handParser =
  do
    hndChars <- many1 (choice [digit, letter])
    let hnd = fromChars hndChars
    skipSpace
    digChars <- many1 digit
    let rank = digitsToInt digChars
    return (hnd, rank)

parseAndClassify :: T.Text -> IO ()
parseAndClassify t =
  let chs = T.unpack t
      hnd = fromChars chs
      clf = classifyHand hnd
   in print clf

genHands :: Int -> Hand -> [Hand]
genHands n others =
  nub $ genHands' n others []
  where
    genHands' n (e : rest) beg =
      let rep = replicate (n + 1) e
          ret = beg ++ rep ++ rest
       in ret : genHands' n rest (beg ++ [e])
    genHands' _ [] _ = []

constructAllJokerHands :: Hand -> [Hand]
constructAllJokerHands hnd =
  let (jokers, others) = partition (== J) hnd
   in case length jokers of
        0 -> [hnd]
        1 -> genHands 1 others
        2 -> genHands 2 others
        3 -> genHands 3 others
        4 -> genHands 4 others
        5 -> [replicate 5 A]
        _ -> undefined

classifyHand :: Hand -> HandCat
classifyHand hnd
  | length hnd /= 5 = error "Expected a hand with 5 cards."
  | otherwise =
      let counts = fmap length ((group . sort) hnd)
          sortedCounts = (reverse . sort) counts
       in -- findBestHand (fst <$> sortedCounts)
          findBestHand sortedCounts
  where
    findBestHand (h : rest) = case h of
      5 -> FiveOfKind
      4 -> FourOfKind
      3 -> case rest of
        (2 : _) -> FullHouse
        _ -> ThreeOfKind
      2 -> case rest of
        (2 : _) -> TwoPair
        _ -> OnePair
      _ -> HighCard
    findBestHand _ = error "expected non empty hand."

sortHands :: [((HandCat, Hand), Int)] -> [((HandCat, Hand), Int)]
sortHands = sortBy (\(h1, _) (h2, _) -> compare h1 h2)

rankAndSum :: [((HandCat, Hand), Int)] -> Int
rankAndSum sorted =
  let zipped = zip [1 ..] sorted
   in sum (fmap (\(rank, (_, score)) -> rank * score) zipped)

classifyAndSortHands :: [HandWithScore] -> [((HandCat, Hand), Int)]
classifyAndSortHands hws =
  let classifiedHands = fmap (\(h, s) -> ((classifyHand h, h), s)) hws
   in sortHands classifiedHands

classifyAndSortWithJokers :: [HandWithScore] -> [((HandCat, Hand), Int)]
classifyAndSortWithJokers hws =
  -- [(jHands, origHand, score)]
  let hnds = fmap (\(h, s) -> (constructAllJokerHands h, h, s)) hws
   in -- we want to return the best hand rating with the hand _with_ jokers
      sortHands (fmap (\(j, o, s) -> ((bestHand j o, o), s)) hnds)
  where
    bestHand jHands orig =
      -- needs hand category and original
      let clfHands = fmap (\h -> (classifyHand h, orig)) jHands
       in fst (maximum clfHands)

part1 :: [HandWithScore] -> IO ()
part1 hndsWithScore =
  do
    print hndsWithScore
    let sortByCats = classifyAndSortHands hndsWithScore
    print sortByCats
    let res = rankAndSum sortByCats
    print res

part2 :: [HandWithScore] -> IO ()
part2 hws =
  do
    let sortByCats = classifyAndSortWithJokers hws
    print sortByCats
    let res = rankAndSum sortByCats
    print res

day07 :: IO ()
day07 = do
  inputLines <- T.lines <$> (getDataFileName "day07-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let parsed = traverse (parseOnly handParser) inputLines
  hnds <- either fail pure parsed
  part2 hnds