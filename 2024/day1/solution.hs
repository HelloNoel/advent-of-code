import Data.List

part1 :: FilePath -> IO ()
part1 = printFromFile $ distance . map sort . transpose . map (map read . words) . lines

part2 :: FilePath -> IO ()
part2 = printFromFile $ similarity . transpose . map (map read . words) . lines

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

distance :: [[Int]] -> Int
distance x = sum . map abs $ zipWith (-) l1 l2
  where
    [l1, l2] = x

similarity :: [[Int]] -> Int
similarity x = sum [i * length (filter (== i) l2) | i <- l1]
  where
    [l1, l2] = x
