module Day07 where

import Data.Attoparsec.Text
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)
import Data.List (group, groupBy, sort, sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)
import Utils (digitsToInt)

data CardType = Num Int | T | J | Q | K | A deriving (Eq, Ord, Show)

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

type WinningHandSegment = [CardType]

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

classifyAndSortHands :: [HandWithScore] -> [((HandCat, Hand), Int)]
classifyAndSortHands hws =
  let classifiedHands = fmap (\(h, s) -> ((classifyHand h, h), s)) hws
   in sortBy (\(h1, _) (h2, _) -> compare h1 h2) classifiedHands

part1 :: [HandWithScore] -> IO ()
part1 hndsWithScore =
  do
    print hndsWithScore
    let sortByCats = classifyAndSortHands hndsWithScore
    print sortByCats
    let zipped = zip [1 ..] sortByCats
    print zipped
    let res = sum (fmap (\(rank, (_, score)) -> rank * score) zipped)
    print res

day07 :: IO ()
day07 = do
  inputLines <- T.lines <$> (getDataFileName "day07-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let parsed = traverse (parseOnly handParser) inputLines
  hnds <- either fail pure parsed
  part1 hnds