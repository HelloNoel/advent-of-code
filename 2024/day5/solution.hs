import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set

part1 :: FilePath -> IO ()
part1 = printFromFile totalOfMid

part2 :: FilePath -> IO ()
part2 = printFromFile totalOfMid'

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

rulesAndUpdates :: String -> ([[Int]], [[Int]])
rulesAndUpdates content = (map (map read . splitOn "|") $ lines rules, map (map read . splitOn ",") $ lines updates)
  where
    (rules, updates) = case splitOn "\n\n" content of
      [x, y] -> (x, y)
      _ -> error "Invalid input"

makeMap :: [[Int]] -> Map.Map Int (Set.Set Int)
makeMap = Map.fromList . map createSet
  where
    createSet [_, x] = (x, Set.empty)
    createSet _ = error "Invalid input"

fillMap :: [[Int]] -> Map.Map Int (Set.Set Int) -> Map.Map Int (Set.Set Int)
fillMap [] m = m
fillMap (x : xs) m = fillMap xs $ Map.insert after newSet m
  where
    (before, after) = case x of
      [a, b] -> (a, b)
      _ -> error "Invalid input"
    newSet = Set.insert before (m Map.! after)

correctValues :: Map.Map Int (Set.Set Int) -> [Int] -> Int
correctValues _ [] = 0
correctValues m xs
  | result == xs = result !! (length xs `div` 2)
  | otherwise = 0
  where
    result = sortUpdates m $ Set.fromList xs

wrongValues :: Map.Map Int (Set.Set Int) -> [Int] -> Int
wrongValues _ [] = 0
wrongValues m xs
  | result /= xs = result !! (length xs `div` 2)
  | otherwise = 0
  where
    result = sortUpdates m $ Set.fromList xs

sortUpdates :: Map.Map Int (Set.Set Int) -> Set.Set Int -> [Int]
sortUpdates m candidates
  | Set.null candidates = []
  | otherwise = value : sortUpdates m remaining
  where
    (value, remaining) = findNext (Set.toList candidates) candidates
    findNext (y : ys) allCandidates
      | Map.notMember y m || Set.null (Set.intersection remainingCandidates constraints) = (y, remainingCandidates)
      | otherwise = findNext ys allCandidates
      where
        remainingCandidates = Set.delete y allCandidates
        constraints = m Map.! y
    findNext [] _ = error "No candidate found"

totalOfMid :: String -> Int
totalOfMid content = sum $ map (correctValues ruleSet) updates
  where
    (rules, updates) = rulesAndUpdates content
    ruleSet = fillMap rules $ makeMap rules

totalOfMid' :: String -> Int
totalOfMid' content = sum $ map (wrongValues ruleSet) updates
  where
    (rules, updates) = rulesAndUpdates content
    ruleSet = fillMap rules $ makeMap rules
