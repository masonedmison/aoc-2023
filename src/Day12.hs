module Day12 where

import Paths_aoc2023 (getDataFileName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Control.Applicative (empty, Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text
import Utils (digitsToInt)
import System.TimeIt (timeIt)
import qualified Data.List.Safe as LS
import Debug.Trace (trace)
import Data.List.Safe (maximumBy)

data Domain = Var | Damaged | Working deriving (Show, Eq)
fromChar :: Char -> Domain
fromChar ch = case ch of
  '.' -> Working
  '#' -> Damaged
  '?' -> Var
  _ -> error "expected ., #, or ?."

orderDomainValues :: [Domain]
orderDomainValues = [Damaged, Working]

type Row = V.Vector Domain
add :: Int -> Domain -> Row -> Row
add idx d r = V.update r (V.singleton (idx, d))

data CSP = CSP {
  row :: Row,
  variables :: [Int],
  cgs :: [Int]
}
fromRowAndCgs :: Row -> [Int] -> CSP
fromRowAndCgs row cgs =
  let vars = V.toList $ V.findIndices (==Var) row
    in
      CSP row vars cgs

withRow :: CSP -> Row -> CSP
withRow csp row = csp { row = row }

-- a Row must contain contiguous groups of 'Damaged' nodes in constraintGroups where
-- each group is separated by at least one 'Working' node with _no_ remaining
-- Vars
checkConstraint :: CSP -> Bool
checkConstraint (CSP row _ cgs) =
  Var `notElem` row &&
    length damagedGroups == length cgs &&
    all (\(g, l) -> length g == l) (zip damagedGroups cgs)
  where
    damagedGroups = filter (Damaged `elem`) $ V.group row

cspParser :: Parser CSP
cspParser =
  do
    domains' <- many1 $ choice [char '?', char '.', char '#']
    many' (char ' ')
    cgs' <- sepBy (many1 digit) (char ',')
    let cgs = digitsToInt <$> cgs'
    let domains = V.fromList $ fromChar <$> domains'
    return (fromRowAndCgs domains cgs)

maxOrZero :: [Int] -> Int
maxOrZero as =
  if null as then 0
  else maximum as

consistentWithAssignment :: CSP -> Bool
consistentWithAssignment (CSP row vars cgs) =
  firstGroupCheck && generalChecks
  --  && elemsInBetween damagedGroups
  --  && (trace ("damagedgroups:\n\t" ++ show damagedGroups) elemsInBetween damagedGroups)
  where
    groups = V.groupBy (\(_, d1) (_, d2) -> d1 == d2) (V.indexed row)
    damagedGroups = filter (\g -> snd (V.head g) == Damaged) groups
    damagedCounts = V.length <$> damagedGroups
    totalDamagedCount = sum damagedCounts
    totalExpectedDamagedCount = sum cgs
    firstGroupCheck =
      case LS.head damagedGroups of
        Just h ->
          let startIdx = fst $ V.head h
            in
              -- this must be the first group if the starting index is less than
              -- the first cg
              (startIdx >= head cgs) || V.length h <= head cgs
        Nothing -> True
    generalChecks =
      (length damagedGroups /= length cgs ||
        all (\(g, l) -> length g <= l) (zip damagedGroups cgs)) &&
          totalExpectedDamagedCount >= totalDamagedCount &&
          (length vars + totalDamagedCount) >= totalExpectedDamagedCount &&
          maxOrZero damagedCounts <= maximum cgs

dfs :: CSP -> Int
dfs (CSP _ [] _) = 0
dfs csp@(CSP row (vIdx:rest) cgs) =
  sum go + dfs (CSP row rest cgs)
  where
    go =
      do
        choice <- orderDomainValues
        let reducedCspWithAssn = csp {row = add vIdx choice row, variables = rest}
        guard (consistentWithAssignment reducedCspWithAssn)
        return (solve reducedCspWithAssn)

solve :: CSP -> Int
solve csp
  | checkConstraint csp = 1
  | otherwise = dfs csp

part1 :: [CSP] -> Int
part1 csps =
  sum $ solve <$> csps

day12 :: IO ()
day12 = do
  inputLines <- T.lines <$> (getDataFileName "day12-input.txt" >>= TIO.readFile)
  putStrLn "This is what I read from input:"
  TIO.putStrLn $ T.unlines inputLines
  let csps' = parseOnly cspParser <$> inputLines
  csps <- either fail pure $ sequence csps'
  -- let result = part1 csps
  timeIt $ print $ part1 csps
