import ClassyPrelude (hashNub)
import Data.List (delete)
import qualified Data.Vector as V

part1 :: FilePath -> IO ()
part1 = printFromFile countPos

part2 :: FilePath -> IO ()
part2 = printFromFile countObstructions

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

guardMap :: String -> V.Vector (V.Vector Char)
guardMap = V.fromList . map V.fromList . lines

walk :: V.Vector (V.Vector Char) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
walk m (dx, dy) (x, y) = case nextPos of
  Nothing -> [(x, y)]
  Just '#' -> walk m (turn (dx, dy)) (x, y)
  _ -> (x, y) : walk m (dx, dy) (x', y')
  where
    y' = y + dy
    x' = x + dx
    nextPos = do
      a <- m V.!? y'
      a V.!? x'

turn :: (Int, Int) -> (Int, Int)
turn (dx, dy)
  | dy == 1 = (-1, 0)
  | dy == -1 = (1, 0)
  | otherwise = (dy, dx)

direction :: String -> ((Int, Int), Char)
direction (x : xs)
  | x == '^' = ((0, -1), '^')
  | x == 'v' = ((0, 1), 'v')
  | x == '<' = ((-1, 0), '<')
  | x == '>' = ((1, 0), '>')
  | otherwise = direction xs
direction _ = error "No direction found"

startPos :: V.Vector (V.Vector Char) -> Char -> (Int, Int)
startPos m c = startPos' 0
  where
    startPos' j = case V.elemIndex c (m V.! j) of
      Just i -> (i, j)
      Nothing -> startPos' $ j + 1

listPos :: String -> [(Int, Int)]
listPos content = walk m d s
  where
    m = guardMap content
    (d, c) = direction content
    s = startPos m c

countPos :: String -> Int
countPos = length . hashNub . listPos

checkLoop :: [(Int, Int, Int, Int)] -> Bool
checkLoop list = checkLoop' list list
  where
    checkLoop' (x : xs) (_ : y2 : ys)
      | x == y2 = True
      | otherwise = checkLoop' xs ys
    checkLoop' _ _ = False

dirWalk :: V.Vector (V.Vector Char) -> (Int, Int) -> (Int, Int) -> [(Int, Int, Int, Int)]
dirWalk m (dx, dy) (x, y) = case nextPos of
  Nothing -> [(dx, dy, x, y)]
  Just '#' -> dirWalk m (turn (dx, dy)) (x, y)
  _ -> (dx, dy, x, y) : dirWalk m (dx, dy) (x', y')
  where
    y' = y + dy
    x' = x + dx
    nextPos = do
      a <- m V.!? y'
      a V.!? x'

obstructions :: [(Int, Int)] -> V.Vector (V.Vector Char) -> [V.Vector (V.Vector Char)]
obstructions ((x, y) : xs) m = m' : obstructions xs m
  where
    m' = m V.// [(y, (m V.! y) V.// [(x, '#')])]
obstructions _ _ = []

countObstructions :: String -> Int
countObstructions content = length . filter checkLoop . map listDirPos' $ ms
  where
    obsPos = delete s $ hashNub . listPos $ content
    m = guardMap content
    ms = obstructions obsPos m
    (d, c) = direction content
    s = startPos m c
    listDirPos' x = dirWalk x d s
