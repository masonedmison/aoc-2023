module Day08 where

import Data.Attoparsec.Text
import Data.Foldable (Foldable (fold))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Paths_aoc2023 (getDataFileName)

data Instruction = R | L deriving (Show)

fromChar :: Char -> Instruction
fromChar c
  | c == 'R' = R
  | c == 'L' = L
  | otherwise = error "Only supported are R and L instructions."

type Node = String

type Network = M.Map Node (Node, Node)

chooseDir :: Instruction -> Node -> Network -> Node
chooseDir inst node net =
  let toNodes = M.lookup node net
   in case toNodes of
        Nothing -> error "expected a mapped node pair"
        Just pair -> case inst of
          L -> fst pair
          R -> snd pair

traverseNetwork :: String -> [Instruction] -> Network -> Int
traverseNetwork dest insts net =
  traverseNetwork' insts "AAA" 0
  where
    traverseNetwork' insts curr count
      | curr == dest = count
      | otherwise = case insts of
          (nextInst : rest) ->
            let nextNode = chooseDir nextInst curr net
             in traverseNetwork' rest nextNode (count + 1)
          [] -> count

parseInsts :: Parser [Instruction]
parseInsts =
  fmap (fromChar <$>) (many1 (choice [char 'R', char 'L']))

parseNodeAndMappings :: Parser (Node, (Node, Node))
parseNodeAndMappings =
  do
    n <- many1 letter
    skipSpace
    char '='
    skipSpace
    char ('(')
    p1 <- many1 letter
    char (',')
    skipSpace
    p2 <- many1 letter
    char ')'
    return (n, (p1, p2))

parseInput :: Parser ([Instruction], Network)
parseInput =
  do
    insts <- parseInsts
    many1 endOfLine
    tups <- sepBy parseNodeAndMappings endOfLine
    return (insts, M.fromList tups)

part1 :: [Instruction] -> Network -> IO ()
part1 insts net =
  let cycledInsts = cycle insts
      count = traverseNetwork "ZZZ" cycledInsts net
   in print count

day08 :: IO ()
day08 = do
  input <- getDataFileName "day08-input.txt" >>= TIO.readFile
  putStrLn "This is what I read from input:"
  TIO.putStrLn input
  let parsedEither = parseOnly parseInput input
  (insts, network) <- either fail pure parsedEither
  print insts
  print network
  part1 insts network
