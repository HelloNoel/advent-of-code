part1 :: FilePath -> IO ()
part1 = printFromFile checkSafety

part2 :: FilePath -> IO ()
part2 = printFromFile checkSafety'

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

checkSafety :: String -> Int
checkSafety = length . filter id . map (checkList . map read . words) . lines

checkList :: [Int] -> Bool
checkList (x : y : xs)
  | x < y = isSafe id (x : y : xs)
  | otherwise = isSafe negate (x : y : xs)

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
  | x < y = removeIfNotSafe $ isSafe' id 0 (x : y : xs)
  | otherwise = removeIfNotSafe $ isSafe' negate 0 (x : y : xs)
  where
    removeIfNotSafe ind = ind == -1 || removeAndCheck ind || removeAndCheck (ind + 1) || removeAndCheck 0
    removeAndCheck ind = checkList (deleteAt ind (x : y : xs))

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
