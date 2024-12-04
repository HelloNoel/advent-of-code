import Data.List
import Data.Vector qualified as V

part1 :: FilePath -> IO ()
part1 = printFromFile sumXmas

part2 :: FilePath -> IO ()
part2 = printFromFile answer

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

diagonals :: [String] -> [String]
diagonals [] = []
diagonals [row] = map (: []) row
diagonals (row : rows) = extendDiagonals row $ diagonals rows
  where
    extendDiagonals row diags = zipWith (:) row ([] : diags) ++ drop (length row - 1) diags

directions :: [[String] -> [String]]
directions =
  [ id, -- right
    transpose, -- down
    map reverse, -- left
    transpose . reverse, -- up
    diagonals, -- downleft
    diagonals . transpose . reverse, -- downright
    diagonals . reverse . transpose, -- upleft
    diagonals . transpose -- upright
  ]

countXmas :: String -> Int
countXmas (x : m : a : s : xs)
  | [x, m, a, s] == "XMAS" = 1 + countXmas xs
  | otherwise = countXmas (m : a : s : xs)
countXmas _ = 0

sumXmas :: String -> Int
sumXmas x = sum $ map (sum . map countXmas) (directions <*> [lines x])

getNext :: V.Vector (V.Vector Char) -> (Int, Int) -> Maybe (Int, Int)
getNext v (x, y)
  | y >= j = Nothing
  | x < i - 1 = Just (x + 1, y)
  | otherwise = getNext v (-1, y + 1)
  where
    i = V.length $ v V.! 1
    j = V.length v

getX :: V.Vector (V.Vector Char) -> (Int, Int) -> Maybe (String, String)
getX v (x, y) = do
  as <- v V.!? y
  a <- as V.!? x
  ms <- v V.!? (y + 1)
  m1 <- ms V.!? (x + 1)
  ss <- v V.!? (y - 1)
  s1 <- ss V.!? (x - 1)
  m2 <- ms V.!? (x - 1)
  s2 <- ss V.!? (x + 1)
  return ([m1, a, s1], [m2, a, s2])

checkX :: V.Vector (V.Vector Char) -> (Int, Int) -> Int
checkX v (x, y) =
  case getX v (x, y) of
    Nothing -> 0
    Just (xs, ys) -> fromEnum $ (xs == "MAS" || xs == "SAM") && (ys == "MAS" || ys == "SAM")

countMas :: (Int, Int) -> V.Vector (V.Vector Char) -> Int
countMas coor v =
  case getNext v coor of
    Nothing -> checkX v coor
    Just ncoor -> checkX v coor + countMas ncoor v

answer :: String -> Int
answer = countMas (0, 0) . V.fromList . map V.fromList . lines
