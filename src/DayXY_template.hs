module DayXY where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

dayXY :: IO ()
dayXY = do
  inputLines <- T.lines <$> (getDataFileName "dayXY-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  putStrLn "TODO: implement Day XY"
