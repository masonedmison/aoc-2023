module Utils where

import Data.Attoparsec.Text as P
import Data.Char (digitToInt)
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import Data.List (transpose)
import qualified Data.List as V

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

-- Does not handle non-square arrays, nor
-- those with an outer dimension of zero.
transposeVec :: Vector (Vector a) -> Vector (Vector a)
transposeVec v = Vector.fromList
  [ Vector.fromList
    [ v ! row ! col 
    | row <- [0 .. currRow]
    ]
  | col <- [0 .. currCol] 
  ]
  where
    currRow = Vector.length v - 1 -- 6
    currCol = Vector.length (v ! 0) - 1 -- 8
