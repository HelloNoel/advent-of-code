import qualified Data.IntSet as IntSet
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as VU

part1 :: FilePath -> IO ()
part1 = printFromFile fencePrice

part2 :: FilePath -> IO ()
part2 = printFromFile discountedPrice

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

type Pos = (Int, Int)

getPosToIndex :: Int -> Int -> Pos -> Maybe Int
getPosToIndex width height (x, y)
  | x < 0 || y < 0 || x >= width || y >= height = Nothing
  | otherwise = Just $ x + y * width

getIndexToPos :: Int -> Int -> Pos
getIndexToPos width i = (i `mod` width, i `div` width)

parseMap :: String -> VU.Vector Char
parseMap = VU.fromList . filter (/= '\n')

getWidth :: String -> Int
getWidth = length . takeWhile (/= '\n')

getHeight :: String -> Int
getHeight = length . lines

top :: Int -> Int -> (Int, Int) -> Maybe Int
top width height (x, y) = getPosToIndex width height (x, y - 1)

bottom :: Int -> Int -> (Int, Int) -> Maybe Int
bottom width height (x, y) = getPosToIndex width height (x, y + 1)

left :: Int -> Int -> (Int, Int) -> Maybe Int
left width height (x, y) = getPosToIndex width height (x - 1, y)

right :: Int -> Int -> (Int, Int) -> Maybe Int
right width height (x, y) = getPosToIndex width height (x + 1, y)

perimeter :: String -> (Int -> Int -> (Int, Int) -> Maybe Int) -> VU.Vector Char -> Int -> Int -> (Int, Int) -> Set.Set (String, Int) -> Set.Set (String, Int)
perimeter directionStr directionFunc gardenMap width height p perimeterSet = case directionFunc width height p of
  Nothing -> withWall
  Just t -> if gardenMap VU.! i == gardenMap VU.! t then perimeterSet else withWall
  where
    withWall = Set.insert (directionStr, i) perimeterSet
    i = fromJust $ getPosToIndex width height p

checkPerimeter :: VU.Vector Char -> Int -> Int -> Int -> Set.Set (String, Int) -> Set.Set (String, Int)
checkPerimeter gardenMap width height i perimeterSet =
  foldr
    ($)
    perimeterSet
    [ perimeter "top" top gardenMap width height p,
      perimeter "bottom" bottom gardenMap width height p,
      perimeter "left" left gardenMap width height p,
      perimeter "right" right gardenMap width height p
    ]
  where
    p = getIndexToPos width i

allPerimeters :: VU.Vector Char -> Int -> Int -> [IntSet.IntSet] -> [Set.Set (String, Int)]
allPerimeters _ _ _ [] = []
allPerimeters gardenMap width height (a : as) = foldr (checkPerimeter gardenMap width height) Set.empty (IntSet.toList a) : allPerimeters gardenMap width height as

checkArea :: VU.Vector Char -> Int -> Int -> Int -> IntSet.IntSet
checkArea gardenMap width height i = checkArea' IntSet.empty [i]
  where
    checkArea' areaSet [] = areaSet
    checkArea' areaSet (j : js)
      | IntSet.member j areaSet = checkArea' areaSet js
      | otherwise = checkArea' (IntSet.insert j areaSet) (neighbours ++ js)
      where
        neighbours =
          filter (\x -> IntSet.notMember x areaSet && gardenMap VU.! x == gardenMap VU.! j) $
            mapMaybe
              ($ getIndexToPos width j)
              [top width height, bottom width height, left width height, right width height]

allAreas :: VU.Vector Char -> Int -> Int -> IntSet.IntSet -> [IntSet.IntSet]
allAreas gardenMap width height areaSet = allAreas' areaSet (VU.length gardenMap - 1) []
  where
    allAreas' _ (-1) acc = acc
    allAreas' areaSet' i acc
      | IntSet.notMember i areaSet' = allAreas' (IntSet.union areaSet' newArea) (i - 1) (newArea : acc)
      | otherwise = allAreas' areaSet' (i - 1) acc
      where
        newArea = checkArea gardenMap width height i

fencePrice :: String -> Int
fencePrice content = sum $ zipWith (*) numAreas numPerimeters
  where
    width = length . takeWhile (/= '\n') $ content
    height = length . lines $ content
    gardenMap = VU.fromList . filter (/= '\n') $ content
    areas = allAreas gardenMap width height IntSet.empty
    perimeters = allPerimeters gardenMap width height areas
    numAreas = map IntSet.size areas
    numPerimeters = map Set.size perimeters

side :: String -> (Int -> Int -> (Int, Int) -> Maybe Int) -> (Int -> Int -> (Int, Int) -> Maybe Int) -> VU.Vector Char -> Int -> Int -> (Int, Int) -> Set.Set (String, Int) -> Set.Set (String, Int)
side directionStr directionFunc startFunc gardenMap width height p sideSet = case directionFunc width height p of
  Nothing -> checkStart
  Just t -> if iChar == gardenMap VU.! t then sideSet else checkStart
  where
    iChar = gardenMap VU.! i
    withWall = Set.insert (directionStr, i) sideSet
    i = fromJust $ getPosToIndex width height p
    checkStart = case startFunc width height p of
      Nothing -> withWall
      Just u ->
        if iChar /= gardenMap VU.! u
          then withWall
          else case directionFunc width height $ getIndexToPos width u of
            Nothing -> sideSet
            Just v -> if iChar == gardenMap VU.! v then withWall else sideSet

checkSide :: VU.Vector Char -> Int -> Int -> Int -> Set.Set (String, Int) -> Set.Set (String, Int)
checkSide gardenMap width height i sideSet =
  foldr
    ($)
    sideSet
    [ side "top" top left gardenMap width height p,
      side "bottom" bottom right gardenMap width height p,
      side "left" left bottom gardenMap width height p,
      side "right" right top gardenMap width height p
    ]
  where
    p = getIndexToPos width i

allSides :: VU.Vector Char -> Int -> Int -> [IntSet.IntSet] -> [Set.Set (String, Int)]
allSides _ _ _ [] = []
allSides gardenMap width height (a : as) = foldr (checkSide gardenMap width height) Set.empty (IntSet.toList a) : allSides gardenMap width height as

discountedPrice :: String -> Int
discountedPrice content = sum $ zipWith (*) numAreas numSides
  where
    width = length . takeWhile (/= '\n') $ content
    height = length . lines $ content
    gardenMap = VU.fromList . filter (/= '\n') $ content
    areas = allAreas gardenMap width height IntSet.empty
    sides = allSides gardenMap width height areas
    numAreas = map IntSet.size areas
    numSides = map Set.size sides
