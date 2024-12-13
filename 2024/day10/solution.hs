{-# LANGUAGE TupleSections #-}

import Data.Char (digitToInt)
import qualified Data.IntSet as IntSet
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Vector.Unboxed as VU

part1 :: FilePath -> IO ()
part1 = printFromFile totalTrailHeads

part2 :: FilePath -> IO ()
part2 = printFromFile totalRatings

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

type Pos = (Int, Int)

topoMap :: String -> VU.Vector Int
topoMap = VU.fromList . map digitToInt . filter (/= '\n')

getPosToIndex :: Int -> Int -> Pos -> Maybe Int
getPosToIndex width height (x, y)
  | x < 0 || y < 0 || x >= height || y >= width = Nothing
  | otherwise = Just $ x * width + y

getIndexToPos :: Int -> Int -> Pos
getIndexToPos width i = (i `div` width, i `mod` width)

getWidth :: String -> Int
getWidth = length . takeWhile (/= '\n')

getHeight :: String -> Int
getHeight = length . lines

findSource :: VU.Vector Int -> [(Int, Int)]
findSource topo = [(i, 0) | i <- [0 .. VU.length topo - 1], topo VU.! i == 0]

trailHeads :: VU.Vector Int -> Seq.Seq (Int, Int) -> IntSet.IntSet -> Int -> Int -> Int -> Int
trailHeads topo ((i, n) Seq.:<| queue) visited width height acc
  | topo VU.! i == 9 = trailHeads topo queue visited' width height (acc + 1)
  | otherwise = trailHeads topo queue' visited' width height acc
  where
    (x, y) = getIndexToPos width i
    neighbours = mapMaybe (getPosToIndex width height) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    validNeighbours = filter (\ind -> not (IntSet.member ind visited) && topo VU.! ind == n + 1) neighbours
    queue' = queue Seq.>< Seq.fromList (map (,n + 1) validNeighbours)
    visited' = IntSet.union visited (IntSet.fromList validNeighbours)
trailHeads _ _ _ _ _ acc = acc

totalTrailHeads :: String -> Int
totalTrailHeads content = sum $ map (singleTrail . Seq.singleton) (findSource topo)
  where
    topo = topoMap content
    width = getWidth content
    height = getHeight content
    singleTrail x = trailHeads topo x IntSet.empty width height 0

trailRatings :: VU.Vector Int -> Seq.Seq (Int, Int) -> IntSet.IntSet -> Int -> Int -> Int -> Int
trailRatings topo ((i, n) Seq.:<| queue) visited width height acc
  | topo VU.! i == 9 = trailRatings topo queue visited' width height (acc + 1)
  | otherwise = trailRatings topo queue' visited' width height acc
  where
    (x, y) = getIndexToPos width i
    neighbours = mapMaybe (getPosToIndex width height) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    validNeighbours = filter (\ind -> not (IntSet.member ind visited) && topo VU.! ind == n + 1) neighbours
    queue' = queue Seq.>< Seq.fromList (map (,n + 1) validNeighbours)
    visited' = IntSet.insert i visited
trailRatings _ _ _ _ _ acc = acc

totalRatings :: String -> Int
totalRatings content = trailRatings topo (Seq.fromList $ findSource topo) IntSet.empty width height 0
  where
    topo = topoMap content
    width = getWidth content
    height = getHeight content