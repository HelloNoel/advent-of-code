import Data.Bits
import Data.Foldable (foldl')
import qualified Data.Map as Map

part1 :: FilePath -> IO ()
part1 = printFromFile $ sum . map ((!! 2000) . iterate nextSecret . read) . lines

part2 :: FilePath -> IO ()
part2 = printFromFile $ Map.foldl' max 0 . foldl' (\m s -> Map.unionWith (+) m $ bestPrice Map.empty s) Map.empty . map (take 2000 . iterate nextSecret . read) . lines

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (.&. 16777215)

step1 :: Int -> Int
step1 sec = prune . mix sec $ sec `shiftL` 6

step2 :: Int -> Int
step2 sec = prune . mix sec $ sec `shiftR` 5

step3 :: Int -> Int
step3 sec = prune . mix sec $ sec `shiftL` 11

nextSecret :: Int -> Int
nextSecret = step3 . step2 . step1

bestPrice :: Map.Map (Int, Int, Int, Int) Int -> [Int] -> Map.Map (Int, Int, Int, Int) Int
bestPrice m (a : b : c : d : e : rest)
  | Map.member key m = bestPrice m (b : c : d : e : rest)
  | otherwise = bestPrice (Map.insert key e' m) (b : c : d : e : rest)
  where
    a' = a `mod` 10
    b' = b `mod` 10
    c' = c `mod` 10
    d' = d `mod` 10
    e' = e `mod` 10
    key = (a' - b', b' - c', c' - d', d' - e')
bestPrice m _ = m
