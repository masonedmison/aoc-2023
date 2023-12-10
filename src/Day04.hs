module Day04 where

import Data.Attoparsec.Text as P
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)
import Utils (digitsToInt)

data CardGame = CardGame Int [Int] [Int]
  deriving (Show)

type CardIdCounter = Map.Map Int Int

inc :: Int -> CardIdCounter -> CardIdCounter
inc = addBy 1

addBy :: Int -> Int -> CardIdCounter -> CardIdCounter
addBy n id =
  Map.alter
    ( \case
        Nothing -> Just $ 0 + n
        Just v -> Just $ v + n
    )
    id

cardParser :: P.Parser CardGame
cardParser =
  let numSpaces = P.sepBy1 (P.many1 P.digit) (P.many1 P.space)
   in do
        P.string "Card"
        P.skipSpace
        id <- P.many1 P.digit
        P.char ':'
        P.skipSpace
        winnings <- numSpaces
        P.many1 P.space
        P.char ('|')
        P.many1 P.space
        mine <- numSpaces
        return (CardGame (digitsToInt id) (digitsToInt <$> winnings) (digitsToInt <$> mine))

-- part1
getPoints :: CardGame -> Int
getPoints (CardGame _ win mine) =
  let c = length $ filter (`elem` mine) win
   in case c of
        0 -> 0
        n -> 2 ^ (n - 1)

calculateCopies :: CardGame -> Map.Map Int Int -> Map.Map Int Int
calculateCopies (CardGame id win mine) m =
  let mc = length $ filter (`elem` mine) win
   in case mc of
        0 -> inc id m
        c ->
          let updated = inc id m
              idCount = Map.findWithDefault 0 id updated
           in let toUpdate = [(id + 1) .. (id + c)]
               in foldr (addBy idCount) updated toUpdate

part1 :: [CardGame] -> IO ()
part1 cards =
  do
    let r = sum (fmap getPoints cards)
    print r

part2 :: [CardGame] -> IO ()
part2 cards =
  let copies = foldl' (flip calculateCopies) Map.empty cards
      counts = snd <$> Map.toList copies
   in print $ sum counts

day04 :: IO ()
day04 = do
  inputLines <- T.lines <$> (getDataFileName "day04-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let cardEithers = mapM (P.parseOnly cardParser) inputLines
  print cardEithers
  cards <- either fail pure cardEithers
  part2 cards