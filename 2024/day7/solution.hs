part1 :: FilePath -> IO ()
part1 = printFromFile calibRes

part2 :: FilePath -> IO ()
part2 = printFromFile calibRes'

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

checkRow :: String -> (Int, Bool)
checkRow x = (total, isValid total eqn)
  where
    (t, e) = break (== ':') x
    total = read t
    eqn = reverse . map read . tail . words $ e

isValid :: Int -> [Int] -> Bool
isValid total [x] = total == x
isValid total (x : xs)
  | total < x = False
  | r == 0 = isValid q xs || isValid (total - x) xs
  | otherwise = isValid (total - x) xs
  where
    (q, r) = quotRem total x

calibRes :: String -> Int
calibRes x = sum [a | (a, b) <- map checkRow . lines $ x, b]

isValid' :: Int -> [Int] -> Bool
isValid' total [x] = total == x
isValid' total (x : xs) = checkMul || checkAdd || checkCon
  where
    xlen = length $ show x
    totallen = length $ show total
    checkAdd = (total > x) && isValid' (total - x) xs
    checkMul = (rem total x == 0) && isValid' (total `div` x) xs
    checkCon = totallen > xlen && takeR xlen (show total) == show x && isValid' (read $ take (totallen - xlen) (show total)) xs

takeR :: Int -> String -> String
takeR n l = go (drop n l) l
  where
    go [] r = r
    go (_ : xs) (_ : ys) = go xs ys

checkRow' :: String -> (Int, Bool)
checkRow' x = (total, isValid' total eqn)
  where
    (t, e) = break (== ':') x
    total = read t
    eqn = reverse . map read . tail . words $ e

calibRes' :: String -> Int
calibRes' x = sum [a | (a, b) <- map checkRow' . lines $ x, b]
