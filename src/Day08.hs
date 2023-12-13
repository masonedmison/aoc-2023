{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Data.Attoparsec.Text
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)

data Instruction = R | L deriving (Show)

fromChar :: Char -> Instruction
fromChar c
  | c == 'R' = R
  | c == 'L' = L
  | otherwise = error "Only supported are R and L instructions."

type Node = String

type Network = M.Map Node (Node, Node)

chooseDir :: Instruction -> Network -> Node -> Node
chooseDir inst net node =
  let toNodes = M.lookup node net
   in case toNodes of
        Nothing -> error "expected a mapped node pair"
        Just pair -> case inst of
          L -> fst pair
          R -> snd pair

traverseNetwork :: String -> (String -> Bool) -> [Instruction] -> Network -> Int
traverseNetwork src isDest insts net =
  traverseNetwork' insts src 0
  where
    traverseNetwork' insts curr count
      | isDest curr = count
      | otherwise = case insts of
          (nextInst : rest) ->
            let nextNode = chooseDir nextInst net curr
             in traverseNetwork' rest nextNode (count + 1)
          [] -> count

parseInsts :: Parser [Instruction]
parseInsts =
  fmap (fromChar <$>) (many1 (choice [char 'R', char 'L']))

parseNodeAndMappings :: Parser (Node, (Node, Node))
parseNodeAndMappings =
  do
    n <- many1 (choice [letter, digit])
    skipSpace
    char '='
    skipSpace
    char ('(')
    p1 <- many1 (choice [letter, digit])
    char (',')
    skipSpace
    p2 <- many1 (choice [letter, digit])
    char ')'
    return (n, (p1, p2))

parseInput :: Parser ([Instruction], Network)
parseInput =
  do
    insts <- parseInsts
    many1 endOfLine
    tups <- sepBy parseNodeAndMappings endOfLine
    return (insts, M.fromList tups)

part2 :: [Instruction] -> Network -> Int
part2 insts net =
  foldr1 lcm individualCounts
  where
    cycledInsts = cycle insts
    startingNodes = filter (\s -> last s == 'A') $ M.keys net
    isDest s = last s == 'Z'
    individualCounts = fmap (\n -> traverseNetwork n isDest cycledInsts net) startingNodes

day08 :: IO ()
day08 = do
  input <- getDataFileName "day08-input.txt" >>= TIO.readFile
  putStrLn "This is what I read from input:"
  TIO.putStrLn input
  let parsedEither = parseOnly parseInput input
  (insts, network) <- either fail pure parsedEither
  let count = part2 insts network
  print count
