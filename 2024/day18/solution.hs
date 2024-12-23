{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set

part1 :: FilePath -> IO ()
part1 = printFromFile $ stepsToExit 1024

part2 :: FilePath -> IO ()
part2 = printFromFile blockedCoor

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

type Pos = (Int, Int)

heuristic :: Pos -> Pos -> Int
heuristic (gx, gy) (x, y) = abs (x - gx) + abs (y - gy)

next :: Set.Set Pos -> Pos -> Pos -> [Pos]
next space (gx, gy) (x, y) = filter (\(x', y') -> x' <= gx && y' <= gy && x' >= 0 && y' >= 0 && Set.notMember (x', y') space) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

astar :: Pos -> Pos -> (Pos -> Int) -> (Pos -> [Pos]) -> Maybe Int
astar startPos endPos heuristicFunc nextFunc = astar' (PQ.singleton (heuristicFunc startPos) (startPos, 0)) Set.empty (Map.singleton startPos 0)
  where
    astar' pq visited costs
      | PQ.null pq = Nothing
      | pos == endPos = Just cost
      | Set.member pos visited = astar' pq' visited costs
      | otherwise = astar' newPQ (Set.insert pos visited) newCosts
      where
        ((_, (pos, cost)), pq') = PQ.deleteFindMin pq
        nextPos = [(p, cost + 1) | p <- nextFunc pos, Set.notMember p visited, Map.notMember p costs || cost + 1 < costs Map.! p]
        newPQ = foldl' (\q (n, c) -> PQ.insert (c + heuristicFunc n) (n, c) q) pq' nextPos
        newCosts = foldl' (\m (n, c) -> Map.insert n c m) costs nextPos

stepsToExit :: Int -> String -> Maybe Int
stepsToExit time content = astar (0, 0) s (heuristic s) (next space s)
  where
    space = Set.fromList . map ((\[x, y] -> (x, y)) . map read . splitOn ",") . take time $ lines content
    s = (70, 70)

search :: Int -> Int -> String -> Int
search l r content
  | r - l > 1 = let mid = (l + r) `div` 2 in if isNothing $ stepsToExit mid content then search l mid content else search mid r content
  | otherwise = r

blockedCoor :: String -> String
blockedCoor content = lines content !! (i - 1)
  where
    i = let len = length . lines $ content in search 0 len content
