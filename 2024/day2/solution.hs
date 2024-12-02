part1 :: FilePath -> IO ()
part1 = printFromFile checkSafety

part2 = printFromFile checkSafety'

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

checkSafety :: String -> Int
checkSafety = length . filter id . map (checkList . map read . words) . lines

checkList :: [Int] -> Bool
checkList (x : y : xs)
  | x < y = isSafe (* 1) (x : y : xs)
  | otherwise = isSafe (* (-1)) (x : y : xs)

isSafe :: (Int -> Int) -> [Int] -> Bool
isSafe f (x : y : xs)
  | diff > 0 && diff <= 3 = isSafe f (y : xs)
  | otherwise = False
  where
    diff = f $ y - x
isSafe _ _ = True

checkSafety' :: String -> Int
checkSafety' = length . filter id . map (checkList' . map read . words) . lines

checkList' :: [Int] -> Bool
checkList' (x : y : xs)
  | x < y =
      let i = isSafe' (* 1) 0 (x : y : xs)
       in i == -1 || checkList (deleteAt i (x : y : xs)) || checkList (deleteAt (i + 1) (x : y : xs)) || checkList (deleteAt (i - 1) (x : y : xs))
  | otherwise =
      let i = isSafe' (* (-1)) 0 (x : y : xs)
       in i == -1 || checkList (deleteAt i (x : y : xs)) || checkList (deleteAt (i + 1) (x : y : xs)) || checkList (deleteAt (i - 1) (x : y : xs))

isSafe' :: (Int -> Int) -> Int -> [Int] -> Int
isSafe' f i (x : y : xs)
  | diff > 0 && diff <= 3 = isSafe' f (i + 1) (y : xs)
  | otherwise = i
  where
    diff = f $ y - x
isSafe' _ _ _ = -1

deleteAt :: Int -> [a] -> [a]
deleteAt idx xs = lft ++ rgt
  where
    (lft, _ : rgt) = splitAt idx xs
