import Data.Containers.ListUtils

part1 :: FilePath -> IO ()
part1 = printFromFile countAntinodes

part2 :: FilePath -> IO ()
part2 = printFromFile countAntinodes'

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

countAntinodes :: [Char] -> Int
countAntinodes content = length . nubOrd . concatMap (filter (inBounds xLength yLength)) . getAntinodes . concatMap (filter (\(_, _, a) -> a /= '.')) . zipWith yCoor [0 ..] . map (zipWith xCoor [0 ..]) $ contentList
  where
    xCoor x a = (x, a)
    yCoor y ((x, a) : xs) = (x, y, a) : yCoor y xs
    yCoor _ [] = []
    contentList = lines content
    xLength = length $ head contentList
    yLength = length contentList
    getAntinodes x = [antinodes] <*> x <*> x

antinodes :: (Int, Int, Char) -> (Int, Int, Char) -> [(Int, Int)]
antinodes (x1, y1, a1) (x2, y2, a2)
  | dx == 0 && dy == 0 = []
  | a1 == a2 = [(x2 + dx, y2 + dy), (x1 - dx, y1 - dy)]
  | otherwise = []
  where
    dx = x2 - x1
    dy = y2 - y1

inBounds :: Int -> Int -> (Int, Int) -> Bool
inBounds xLength yLength (x, y) = x >= 0 && x < xLength && y >= 0 && y < yLength

antinodes' :: Int -> Int -> (Int, Int, Char) -> (Int, Int, Char) -> [(Int, Int)]
antinodes' xLength yLength (x1, y1, a1) (x2, y2, a2)
  | dx == 0 && dy == 0 = []
  | a1 == a2 = (x1, y1) : (x2, y2) : harmonics' x2 y2 dx dy ++ harmonics' x1 y1 (-dx) (-dy)
  | otherwise = []
  where
    dx = x2 - x1
    dy = y2 - y1
    harmonics' = harmonics xLength yLength

harmonics :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
harmonics xLength yLength x y dx dy
  | inBounds xLength yLength (x', y') = (x', y') : harmonics xLength yLength x' y' dx dy
  | otherwise = []
  where
    x' = x + dx
    y' = y + dy

countAntinodes' :: [Char] -> Int
countAntinodes' content = length . nubOrd . concat . getAntinodes . concatMap (filter (\(_, _, a) -> a /= '.')) . zipWith yCoor [0 ..] . map (zipWith xCoor [0 ..]) $ contentList
  where
    xCoor x a = (x, a)
    yCoor y ((x, a) : xs) = (x, y, a) : yCoor y xs
    yCoor _ [] = []
    contentList = lines content
    xLength = length $ head contentList
    yLength = length contentList
    getAntinodes x = [antinodes' xLength yLength] <*> x <*> x