{-# LANGUAGE TupleSections #-}

module Day19 where

import Data.Attoparsec.Text
import Data.List.Extra (enumerate)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc2023 (getDataFileName)
import System.TimeIt (timeIt)

type Input = [(String, [(Constraint, String)])]

variables :: [Char]
variables = ['x', 'm', 'a', 's']

data Var = X | M | A | S deriving (Eq, Ord, Enum, Bounded)

data Constraint = Constraint {var :: Var, func :: Int -> Bool}

makeConstraints :: [(Char, Char, Int, String)] -> String -> [(Constraint, String)]
makeConstraints conds d = (go <$> conds) ++ [dConstraint]
  where
    dConstraint = (Constraint X (const True), d)
    go (var, op, n, ret) =
      (Constraint asVar (`opFn` n), ret)
      where
        opFn = if op == '<' then (<) else (>)
        asVar = case var of
          'x' -> X
          'm' -> M
          'a' -> A
          's' -> S
          _ -> error "unexpected variable."

type Workflows = M.Map String [(Constraint, String)]

(!^) :: Workflows -> String -> [(Constraint, String)]
(!^) rm k = fromMaybe [] (M.lookup k rm)

traverseRuleMap :: String -> Workflows -> [M.Map Var [Int]]
traverseRuleMap name workflows = go name . M.fromList . map (,[1 .. 4000]) $ enumerate
  where
    go "A" state = [state]
    go "R" _ = []
    go _ state | all null . M.elems $ state = []
    go cur state = tryPaths (workflows !^ cur) state
      where
        tryPaths [] _ = []
        tryPaths ((c, ifTrue) : cs) state = go ifTrue trueRanges ++ tryPaths cs falseRanges
          where
            v = var c
            op = func c
            trueRanges = M.adjust (filter op) v state
            falseRanges = M.adjust (filter (not . op)) v state

condParser :: Parser (Char, Char, Int, String)
condParser = do
  v <- satisfy (`elem` variables)
  op <- choice [char '<', char '>']
  digs <- many1 digit
  let n = read digs
  char ':'
  s <- many1 letter
  return (v, op, n, s)

caseStmtParser :: Parser (String, [(Constraint, String)])
caseStmtParser = do
  name <- many1 letter
  char '{'
  conds <- sepBy condParser $ char ','
  char ','
  d <- many1 letter
  return (name, makeConstraints conds d)

parseInput :: T.Text -> Either String Input
parseInput t =
  mapM (parseOnly caseStmtParser) (T.lines condsT)
  where
    (condsT : _ : _) = T.splitOn "\n\n" t

part2 :: Input -> Int
part2 inp =
  sum
    . map (product . map length . M.elems)
    . traverseRuleMap "in"
    $ M.fromList inp

day19 :: IO ()
day19 = do
  inputT <- getDataFileName "day19-input.txt" >>= TIO.readFile
  stmts <- either fail pure $ parseInput inputT
  let result = part2 stmts
  timeIt $ print result
