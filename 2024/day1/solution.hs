import Data.List

part1 :: FilePath -> IO ()
part1 = printFromFile $ distance . sortedLists

part2 :: FilePath -> IO ()
part2 = printFromFile $ score 0 0 0 . sortedLists

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
    content <- readFile input
    print . f $ content

sortedLists :: String -> ([Int], [Int])
sortedLists = sortBoth . splitLists [] [] . map readInt . words

readInt :: String -> Int
readInt = read

splitLists :: [Int] -> [Int] -> [Int] -> ([Int], [Int])
splitLists x y [] = (x, y)
splitLists x y (a:b:l) = splitLists (a:x) (b:y) l

sortBoth :: ([Int], [Int]) -> ([Int], [Int])
sortBoth (x,y) = (sort x, sort y)

distance :: ([Int], [Int]) -> Int
distance ([], []) = 0
distance (x:xs, y:ys) = abs (x - y) + distance (xs, ys)

score :: Int -> Int -> Int -> ([Int], [Int]) -> Int
score i l r (x:xs, y:ys)
    | x == i = score i (l + 1) r (xs, y:ys)
    | y == i = score i l (r + 1) (x:xs, ys)
    | x > y = i * l * r + score y 0 0 (x:xs, y:ys)
    | otherwise = i * l * r + score x 0 0 (x:xs, y:ys)
score i l r ([], y:ys)
    | y == i = score i l (r + 1) ([], ys)
    | otherwise = i * l * r
score i l r (x:xs, [])
    | x == i = score i (l + 1) r (xs, [])
    | otherwise = i * l * r
score i l r _ = i * l * r
