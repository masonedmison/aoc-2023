module Utils where

import Data.Attoparsec.Text as P
import Data.Char (digitToInt)

digitsToInt :: [Char] -> Int
digitsToInt chs =
  snd $ foldl (\(base, acc) i -> (base * 10, i * base + acc)) (1, 0) (digitToInt <$> reverse chs)

surroundedBy :: P.Parser a -> P.Parser b -> P.Parser a
surroundedBy pa pb =
  do
    pb
    r <- pa
    pb
    return r