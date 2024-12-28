import Data.Char (isDigit)
import Data.Foldable (foldl')
import qualified Data.Map as Map

part1 :: FilePath -> IO ()
part1 = printFromFile $ sum . map (complexity 2) . lines

part2 :: FilePath -> IO ()
part2 = printFromFile $ sum . map (complexity 25) . lines

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

numpad :: Char -> (Int, Int)
numpad c = case c of
  '7' -> (0, 0)
  '8' -> (1, 0)
  '9' -> (2, 0)
  '4' -> (0, 1)
  '5' -> (1, 1)
  '6' -> (2, 1)
  '1' -> (0, 2)
  '2' -> (1, 2)
  '3' -> (2, 2)
  '0' -> (1, 3)
  'A' -> (2, 3)
  _ -> error "Invalid number"

dirpad :: Char -> (Int, Int)
dirpad c = case c of
  '^' -> (1, 0)
  'v' -> (1, 1)
  '<' -> (0, 1)
  '>' -> (2, 1)
  'A' -> (2, 0)
  _ -> error "Invalid direction"

dirMove :: Char -> Char -> String
dirMove a b =
  let (ax, ay) = dirpad a
      (bx, by) = dirpad b
      dx = bx - ax
      dy = by - ay
      moves = replicate (-dx) '<' ++ replicate (abs dy) (if dy > 0 then 'v' else '^') ++ replicate dx '>'
   in if (ax == 0 && by == 0) || (ay == 0 && bx == 0) then reverse ('A' : moves) else moves ++ "A"

numMove :: Char -> Char -> String
numMove a b =
  let (ax, ay) = numpad a
      (bx, by) = numpad b
      dx = bx - ax
      dy = by - ay
      moves = replicate (-dx) '<' ++ replicate (abs dy) (if dy > 0 then 'v' else '^') ++ replicate dx '>'
   in if (ay == 3 && bx == 0) || (ax == 0 && by == 3) then reverse ('A' : moves) else moves ++ "A"

countMoves :: Int -> Map.Map (String, Int) Int -> String -> (Int, Map.Map (String, Int) Int)
countMoves depth originalMemo originalInput =
  let (i, m) = countMoves' depth originalMemo . concat $ zipWith numMove ('A' : originalInput) originalInput
   in (i, Map.insert (originalInput, depth) i m)
  where
    countMoves' d memo input = case memo Map.!? (input, d) of
      Just l -> (l, memo)
      Nothing ->
        if d == 0
          then let l = length input in (l, Map.insert (input, d) l memo)
          else
            let (output, newMemo) =
                  foldl'
                    ( \(i, m) (a, b) ->
                        let (i', m') = countMoves' (d - 1) m (dirMove a b)
                         in (i + i', m')
                    )
                    (0, memo)
                    $ zip ('A' : input) input
             in (output, Map.insert (input, d) output newMemo)

shortestSeq :: Int -> String -> Int
shortestSeq n = fst . countMoves n Map.empty

numericPart :: String -> Int
numericPart = read . takeWhile isDigit

complexity :: Int -> String -> Int
complexity n content = numericPart content * shortestSeq n content
