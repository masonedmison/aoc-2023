module Day01 where

import Control.Applicative (Applicative (liftA2))
import qualified Data.Attoparsec.Text as P
import Data.Char (digitToInt)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)

stringdigits :: [T.Text]
stringdigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

stringdigitsRev :: [T.Text]
stringdigitsRev = map T.reverse stringdigits

stringdigitsToDigit :: Map.Map T.Text Int
stringdigitsToDigit = Map.fromList (zip stringdigits [1 ..])

consumeUntilFirstM :: P.Parser t -> T.Text -> Either String t
consumeUntilFirstM p t =
  case P.parseOnly p t of
    v@(Right _) -> v
    Left _ | T.length t > 0 -> consumeUntilFirstM p (T.tail t)
    Left _ -> Left "Unable to find expression."

digitParser :: Bool -> P.Parser Int
digitParser isRev =
  let dp = fmap digitToInt P.digit
      sp =
        if isRev
          then fmap (\x -> Map.findWithDefault 1 (T.reverse x) stringdigitsToDigit) (P.choice $ map P.string stringdigitsRev)
          else fmap (\x -> Map.findWithDefault 1 x stringdigitsToDigit) (P.choice $ map P.string stringdigits)
   in P.choice [dp, sp]

getDigits :: T.Text -> Int
getDigits t =
  case digprod of
    Right (f, l) ->
      (f * 10) + l
    _ -> error "unexpected input."
  where
    revt = T.reverse t
    fstt = consumeUntilFirstM (digitParser False) t
    lstt = consumeUntilFirstM (digitParser True) revt
    digprod = liftA2 (,) fstt lstt

day01 :: IO ()
day01 = do
  inputLines <- T.lines <$> (getDataFileName "day01-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let res = map getDigits inputLines
  print res
  let s = sum res
  print s
