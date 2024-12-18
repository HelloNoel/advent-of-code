-- AX1 + BX2 = XT
-- AY1 + BY2 = YT
-- A = (XTY2 - X2YT) / (X1Y2 - Y1X2)
-- B = (YT - AY1) / Y2

import Text.Regex.TDFA

part1 :: FilePath -> IO ()
part1 = printFromFile $ sum . map (\(a, b) -> a * 3 + b) . presses . parseInput

part2 :: FilePath -> IO ()
part2 = printFromFile $ sum . map (\(a, b) -> a * 3 + b) . presses' . parseInput

printFromFile :: (String -> Integer) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

parseInput :: String -> [Integer]
parseInput content = map read . getAllTextMatches $ content =~ "[[:digit:]]+"

presses :: [Integer] -> [(Integer, Integer)]
presses (x1 : y1 : x2 : y2 : xt : yt : ls) = res : presses ls
  where
    (a, rem1) = divMod (xt * y2 - x2 * yt) (x1 * y2 - y1 * x2)
    (b, rem2) = divMod (yt - a * y1) y2
    res = if rem1 /= 0 || rem2 /= 0 then (0, 0) else (a, b)
presses [] = []
presses _ = error "invalid input"

presses' :: [Integer] -> [(Integer, Integer)]
presses' (x1 : y1 : x2 : y2 : x0 : y0 : ls) = res : presses' ls
  where
    xt = x0 + 10000000000000
    yt = y0 + 10000000000000
    (a, rem1) = divMod (xt * y2 - x2 * yt) (x1 * y2 - y1 * x2)
    (b, rem2) = divMod (yt - a * y1) y2
    res = if rem1 /= 0 || rem2 /= 0 then (0, 0) else (a, b)
presses' [] = []
presses' _ = error "invalid input"