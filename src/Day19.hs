module Day19 where

import Control.Applicative (Applicative (liftA2))
import Data.Attoparsec.Text
import Data.List.Extra (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import Paths_aoc2023 (getDataFileName)

variables :: [Char]
variables = ['x', 'm', 'a', 's']

type Input = [Part]

data Part = Part
  {x :: Int, m :: Int, a :: Int, s :: Int}
  deriving (Show)

makePart :: [(Char, Int)] -> Part
makePart assignments = case vals of
  Just (x : m : a : s : _) -> Part x m a s
  _ -> error "Unable to construct Part."
  where
    assnMap = M.fromList assignments
    vals = mapM (`M.lookup` assnMap) variables

sumPart :: Part -> Int
sumPart part = sum $ fmap (\f -> f part) [x, m, a, s]

type RuleMap = M.Map String (Part -> String)

makeRuleMap :: [CaseStmt] -> RuleMap
makeRuleMap =
  foldr step M.empty
  where
    step stmt acc =
      let (name, fn) = toFunction stmt
       in M.insert name fn acc

data CaseStmt = C
  {name :: String, cases :: [(Part -> Bool, String)], d :: String}

makeCaseStmt :: String -> [(Char, Char, Int, String)] -> String -> CaseStmt
makeCaseStmt name conds d = C name (go <$> conds) d
  where
    go (var, op, n, ret) =
      (\p -> opFn (accessor p) n, ret)
      where
        opFn = if op == '<' then (<) else (>)
        accessor = case var of
          'x' -> x
          'm' -> m
          'a' -> a
          's' -> s
          _ -> error "unexpected variable"

toFunction :: CaseStmt -> (String, Part -> String)
toFunction (C n cases d) = (n, fromMaybe d . found)
  where
    found part = snd <$> find (\(f, _) -> f part) cases

accept :: String -> Part -> RuleMap -> Bool
accept start part rules = case go start of
  "R" -> False
  "A" -> True
  _ -> error "failed to terminate"
  where
    go :: String -> String
    go name
      | next `elem` ["A", "R"] = next
      | otherwise = go next
      where
        next = maybe "R" (\f -> f part) $ M.lookup name rules

condParser :: Parser (Char, Char, Int, String)
condParser = do
  v <- satisfy (`elem` variables)
  op <- choice [char '<', char '>']
  digs <- many1 digit
  let n = read digs
  char ':'
  s <- many1 letter
  return (v, op, n, s)

caseStmtParser :: Parser CaseStmt
caseStmtParser = do
  name <- many1 letter
  char '{'
  conds <- sepBy condParser $ char ','
  char ','
  d <- many1 letter
  return $ makeCaseStmt name conds d

kvParser :: Parser (Char, Int)
kvParser = do
  v <- anyChar
  char '='
  digs <- many1 digit
  let n = read digs
  return (v, n)

partParser :: Parser Part
partParser = do
  char '{'
  ps <- sepBy kvParser $ char ','
  return (makePart ps)

parseInput :: T.Text -> Either String ([CaseStmt], [Part])
parseInput t = liftA2 (,) stmts parts
  where
    (condsT : partsT : _) = T.splitOn "\n\n" t
    stmts = mapM (parseOnly caseStmtParser) (T.lines condsT)
    parts = mapM (parseOnly partParser) (T.lines partsT)

part1 :: [CaseStmt] -> [Part] -> Int
part1 stmts parts =
  trace ("accepted parts are: " ++ (show accepted)) sum $ sumPart <$> accepted
  where
    ruleMap = makeRuleMap stmts
    accepted = filter (\p -> accept "in" p ruleMap) parts

day19 :: IO ()
day19 = do
  inputT <- getDataFileName "day19-input.txt" >>= TIO.readFile
  (stmts, parts) <- either fail pure $ parseInput inputT
  let result = part1 stmts parts
  print result
