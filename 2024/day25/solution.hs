import Data.List
import Data.List.Split (splitOn)

part1 :: FilePath -> IO ()
part1 = printFromFile fittingPairs

printFromFile :: (Show a) => (String -> a) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

fittingPairs :: String -> Int
fittingPairs content = foldl' (\acc x -> acc + length (filter id (map (all (<= 7) . zipWith (+) x) keys))) 0 locks
  where
    (l, k) = partition (\x -> head (head x) == '#') . map (transpose . lines) $ splitOn "\n\n" content
    locks = map (map (length . takeWhile (== '#'))) l
    keys = map (map (length . filter (== '#'))) k
