import Data.Char (digitToInt)
import Data.List (intersperse)

part1 :: FilePath -> IO ()
part1 = printFromFile $ sum . zipWith (*) [0 ..] . compact . sparseDisk

part2 :: FilePath -> IO ()
part2 = printFromFile $ sum . zipWith (*) [0 ..] . sparseDisk2 . compact2 . addID

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

sparseDisk :: String -> [Int]
sparseDisk = concat . sparseDisk' 0
  where
    sparseDisk' i (file : free : xs) = replicate (digitToInt file) i : replicate (digitToInt free) (-1) : sparseDisk' (i + 1) xs
    sparseDisk' i [file] = [replicate (digitToInt file) i]
    sparseDisk' _ [] = []

compact :: [Int] -> [Int]
compact x = compact' (length x) x (reverse x)
  where
    compact' 0 _ _ = []
    compact' 1 (x : xs) (y : ys)
      | y == -1 && x == -1 = []
      | x == -1 = [y]
      | otherwise = [x]
    compact' i (x : xs) (y : ys)
      | y == -1 = compact' (i - 1) (x : xs) ys
      | x == -1 = y : compact' (i - 2) xs ys
      | otherwise = x : compact' (i - 1) xs (y : ys)

addID :: String -> [(Int, Int)]
addID = zip (intersperse (-1) [0 ..]) . map digitToInt

compact2 :: [(Int, Int)] -> [(Int, Int)]
compact2 x = compact2' x (reverse x)
  where
    compact2' res [] = res
    compact2' res (x : xs)
      | snd x == -1 = compact2' res xs
      | otherwise = compact2' (merge $ tryInsert res x) xs

tryInsert :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
tryInsert [] x = [x]
tryInsert (y@(y1, y2) : ys) x@(x1, x2)
  | x == y = y : ys
  | y1 == -1 = case compare x2 y2 of
      LT -> x : (y1, y2 - x2) : deDup x ys
      EQ -> x : deDup x ys
      GT -> y : tryInsert ys x
  | otherwise = y : tryInsert ys x

deDup :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
deDup _ [] = []
deDup x (y : ys)
  | x == y = (-1, snd x) : ys
  | otherwise = y : deDup x ys

merge :: [(Int, Int)] -> [(Int, Int)]
merge (x@(x1, x2) : y@(y1, y2) : xs)
  | x2 == 0 = merge $ y : xs
  | y2 == 0 = merge $ x : xs
  | x1 == y1 = merge $ (x1, x2 + y2) : xs
  | otherwise = x : merge (y : xs)
merge [x@(x1, x2)]
  | x2 == 0 = []
  | otherwise = [x]
merge [] = []

sparseDisk2 :: [(Int, Int)] -> [Int]
sparseDisk2 = concat . sparseDisk2'
  where
    sparseDisk2' ((-1, x) : xs) = replicate x 0 : sparseDisk2' xs
    sparseDisk2' ((i, x) : xs) = replicate x i : sparseDisk2' xs
    sparseDisk2' [] = []