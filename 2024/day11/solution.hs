import qualified Data.Map as Map

part1 :: FilePath -> IO ()
part1 = printFromFile (sum . blinks Map.empty . input)
  where
    input = map read . words
    blinks m (x : xs) = v : blinks m' xs
      where
        (m', v) = blinkMany m 25 x
    blinks _ [] = []

part2 :: FilePath -> IO ()
part2 = printFromFile (sum . blinks Map.empty . input)
  where
    input = map read . words
    blinks m (x : xs) = v : blinks m' xs
      where
        (m', v) = blinkMany m 75 x
    blinks _ [] = []

printFromFile :: (String -> Int) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

blinkOnce :: Int -> [Int]
blinkOnce x
  | x == 0 = [1]
  | even lx = [read p1, read p2]
  | otherwise = [x * 2024]
  where
    xStr = show x
    lx = length xStr
    (p1, p2) = splitAt (lx `div` 2) xStr

blinkMany :: Map.Map (Int, Int) Int -> Int -> Int -> (Map.Map (Int, Int) Int, Int)
blinkMany m 0 _ = (m, 1)
blinkMany m n x = case Map.lookup (x, n) m of
  Just v -> (m, v)
  Nothing ->
    let ys = go m $ blinkOnce x
        (m', _) = last ys
        v' = sum $ map snd ys
     in (Map.insert (x, n) v' m', v')
  where
    go _ [] = []
    go m' (y : ys) = y' : go m'' ys
      where
        y'@(m'', _) = blinkMany m' (n - 1) y
