module Utils where

import Data.Attoparsec.Text as P
import Data.Char (digitToInt)

digitsToInt :: [Char] -> Int
digitsToInt = read

skipLine :: Parser ()
skipLine = skipWhile (not . P.isEndOfLine) *> endOfLine

surroundedBy :: P.Parser a -> P.Parser b -> P.Parser a
surroundedBy pa pb =
  do
    pb
    r <- pa
    pb
    return r

negNumParser :: P.Parser Int
negNumParser =
  do
    minus <- many' $ char '-'
    chs <- many1 digit
    return (digitsToInt $ minus ++ chs)