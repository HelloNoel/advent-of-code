import GHC.Float (int2Double)
import Text.Regex.TDFA

part1 :: FilePath -> IO ()
part1 = printFromFile safetyFactor

part2 :: FilePath -> IO ()
part2 = printFromFile minVar

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

parseInput :: String -> [Int]
parseInput content = map read . getAllTextMatches $ content =~ "-?[[:digit:]]+"

calcPos :: Int -> [Int] -> [(Int, Int)]
calcPos steps (x : y : vx : vy : ls) = (x3, y3) : calcPos steps ls
  where
    x2 = rem (x + steps * vx) 101
    y2 = rem (y + steps * vy) 103
    x3 = if x2 < 0 then x2 + 101 else x2
    y3 = if y2 < 0 then y2 + 103 else y2
calcPos _ [] = []
calcPos _ _ = error "invalid input"

toQuadrant :: (Int, Int) -> Int
toQuadrant (x, y)
  | x < 50 && y < 51 = 1
  | x < 50 && y > 51 = 2
  | x > 50 && y > 51 = 3
  | x > 50 && y < 51 = 4
  | otherwise = 0

safetyFactor :: String -> Int
safetyFactor content = a * b * c * d
  where
    quadrants = map toQuadrant . calcPos 100 . parseInput $ content
    a = length . filter (== 1) $ quadrants
    b = length . filter (== 2) $ quadrants
    c = length . filter (== 3) $ quadrants
    d = length . filter (== 4) $ quadrants

var :: [Double] -> Double
var xs = average . map ((^ (2 :: Int)) . (-) (average xs)) $ xs
  where
    average = (/) <$> sum <*> realToFrac . length

checkVar :: Int -> [Int] -> Double
checkVar steps content = xVar + yVar
  where
    pos = calcPos steps content
    xVar = var . map (int2Double . fst) $ pos
    yVar = var . map (int2Double . snd) $ pos

minVar :: String -> Int
minVar content = minVar' 1 (-1) 100000
  where
    content' = parseInput content
    minVar' 10000 m _ = m
    minVar' i m v = if currentVar < v then minVar' (i + 1) i currentVar else minVar' (i + 1) m v
      where
        currentVar = checkVar i content'
