module Day02 where

import qualified Data.Attoparsec.Text as P
import Data.Char (digitToInt)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)

data DrawnSet = DrawnSet
  { blue :: Int,
    red :: Int,
    green :: Int
  } deriving Show
emptyDraw :: DrawnSet
emptyDraw =
  DrawnSet{blue = 0, red = 0, green = 0}
updateDraw :: DrawnSet -> T.Text -> Int -> DrawnSet
updateDraw drawn "green" n = drawn {green = n}
updateDraw drawn "red" n = drawn {red = n}
updateDraw drawn "blue" n = drawn {blue = n}
updateDraw drawn _ _ = drawn

data Game = Game
  { idd :: Int,
    draws :: [DrawnSet]
  } deriving Show

satisfiesPred :: Game -> (Game -> Bool) -> Bool
satisfiesPred g f = f g  

getFewestProduct :: Game -> Int 
getFewestProduct game =
  let fewestDrawn = foldr (\dToCheck accD -> accD {
      red = max (red accD) (red dToCheck), 
      green = max (green accD) (green dToCheck),
      blue = max (blue accD) (blue dToCheck)
      }) emptyDraw (draws game)
      in
        red fewestDrawn * green fewestDrawn * blue fewestDrawn

digitsToInt :: [Char] -> Int
digitsToInt chs = 
  snd $ foldl (\(base, acc) i -> (base * 10, i * base + acc)) (1, 0) (digitToInt <$> reverse chs)

colorParser :: P.Parser (Int, T.Text)
colorParser =
  do
    chs <- P.many1 P.digit
    let n = digitsToInt chs
    P.many1 P.space
    c <- P.choice [P.string "blue", P.string "red", P.string "green"]
    return (n, c)

drawnSetParser :: P.Parser DrawnSet
drawnSetParser =
  fmap (foldl (\drawn (n, c) -> updateDraw drawn c n) emptyDraw) (P.sepBy1 colorParser $ P.string ", ")

gameParser :: P.Parser Game
gameParser =
  do
    P.string "Game "
    id <- digitsToInt <$> P.many1 P.digit
    P.char(':')
    P.many1 P.space
    drawnSets <- P.sepBy1 drawnSetParser $ P.string "; "
    return Game {idd = id, draws = drawnSets}

-- only 12 red cubes, 13 green cubes, and 14 blue cubes?
part1Pred :: Game -> Bool
part1Pred g =
  all (\d -> red d <= 12 && green d <= 13 && blue d <= 14) (draws g)

part1 :: [Game] -> IO ()
part1 games =
  do
    let possible = filter part1Pred games
    putStrLn "Possible games ids:"
    print $ idd <$> possible
    let sumPossibles = sum $ fmap idd possible
    putStrLn "Sum of possibles:"
    print sumPossibles

part2 :: [Game] -> IO ()
part2 games =
  do
    let sumFewest = sum $ getFewestProduct <$> games
    putStrLn "Fewest products are:"
    print sumFewest

day02 :: IO ()
day02 = do
  inputLines <- T.lines <$> (getDataFileName "day02-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let parsed = traverse (P.parseOnly gameParser) inputLines
  games <- either fail pure parsed
  part2 games
  