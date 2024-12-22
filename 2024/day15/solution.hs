{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import qualified Data.IntMap as IntMap
import Data.List.Split (splitOn)
import qualified Data.Set as Set

part1 :: [Char] -> IO ()
part1 = printFromFile f
  where
    f x = sum . map (\(a, _) -> gps width a) . filter (\(_, y) -> y == 'O') . IntMap.toList . fst . moveRobot $ p
      where
        p@(_, _, width, _) = parseInput x

part2 :: FilePath -> IO ()
part2 = printFromFile f
  where
    f x = sum . map (\(a, _) -> gps width a) . filter (\(_, y) -> y == '[') . IntMap.toList . fst . moveRobot2 $ p
      where
        p@(_, _, width, _) = parseInput2 x

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

parseInput :: String -> (IntMap.IntMap Char, String, Int, Int)
parseInput content = (warehouse, movements, row, pos)
  where
    [top, bottom] = splitOn "\n\n" content
    row = length . takeWhile (/= '\n') $ top
    warehouse = IntMap.fromList . filter (\(_, x) -> x /= '.' && x /= '@') . zip [0 ..] . filter (/= '\n') $ top
    movements = filter (/= '\n') bottom
    pos = length . takeWhile (/= '@') . filter (/= '\n') $ top

left :: Int -> Int
left = (-1 +)

right :: Int -> Int
right = (+ 1)

upFunc :: Int -> Int -> Int
upFunc row = (-row +)

downFunc :: Int -> Int -> Int
downFunc row = (+ row)

moveRobot :: (IntMap.IntMap Char, [Char], Int, Int) -> (IntMap.IntMap Char, Int)
moveRobot (warehouse, movements, row, pos) = moveRobot' warehouse movements pos
  where
    up = upFunc row
    down = downFunc row
    moveRobot' w (m : ms) p
      | m == '<' = moveRobot' wleft ms pleft
      | m == '>' = moveRobot' wright ms pright
      | m == '^' = moveRobot' wup ms pup
      | m == 'v' = moveRobot' wdown ms pdown
      | otherwise = error "invalid move"
      where
        (pleft, wleft) = moveTo w p left
        (pright, wright) = moveTo w p right
        (pup, wup) = moveTo w p up
        (pdown, wdown) = moveTo w p down
    moveRobot' w [] p = (w, p)

pushTo :: IntMap.IntMap Char -> Int -> (Int -> Int) -> Maybe (IntMap.IntMap Char)
pushTo w p d = case IntMap.lookup p w of
  Nothing -> Just $ IntMap.insert p 'O' w
  Just 'O' -> pushTo w (d p) d
  _ -> Nothing

moveTo :: IntMap.IntMap Char -> Int -> (Int -> Int) -> (Int, IntMap.IntMap Char)
moveTo w p d = case IntMap.lookup (d p) w of
  Nothing -> (d p, w)
  Just 'O' -> case pushTo (IntMap.delete (d p) w) (d $ d p) d of
    Nothing -> (p, w)
    Just w' -> (d p, w')
  _ -> (p, w)

gps :: Int -> Int -> Int
gps width i = 100 * y + x
  where
    (y, x) = divMod i width

moveRobot2 :: (IntMap.IntMap Char, [Char], Int, Int) -> (IntMap.IntMap Char, Int)
moveRobot2 (warehouse, movements, row, pos) = moveRobot2' warehouse movements pos
  where
    up = upFunc row
    down = downFunc row
    moveRobot2' w (m : ms) p
      | m == '<' = moveRobot2' wleft ms pleft
      | m == '>' = moveRobot2' wright ms pright
      | m == '^' = moveRobot2' wup ms pup
      | m == 'v' = moveRobot2' wdown ms pdown
      | otherwise = error "invalid move"
      where
        (pleft, wleft) = moveHorizontal w p left
        (pright, wright) = moveHorizontal w p right
        (pup, wup) = moveVertical w p up
        (pdown, wdown) = moveVertical w p down
    moveRobot2' w [] p = (w, p)

pushVertical :: IntMap.IntMap Char -> [(Int, Char)] -> Set.Set (Int, Char) -> (Int -> Int) -> Maybe (IntMap.IntMap Char)
pushVertical w [] pNext d = if Set.null pNext then Just w else pushVertical w (Set.toList pNext) Set.empty d
pushVertical w ((p, x) : pCurr) pNext d = case IntMap.lookup p w of
  Nothing -> pushVertical (IntMap.insert p x w) pCurr pNext d
  Just '[' ->
    let r = filter (\(e, _) -> e == right p) pCurr
     in if null r
          then
            pushVertical (IntMap.delete (right p) $ IntMap.insert p x w) pCurr (Set.insert (d $ right p, ']') $ Set.insert (d p, '[') pNext) d
          else
            pushVertical (IntMap.insert (right p) (snd . head $ r) $ IntMap.insert p x w) (filter (\(e, _) -> e /= right p) pCurr) (Set.insert (d $ right p, ']') $ Set.insert (d p, '[') pNext) d
  Just ']' ->
    let r = filter (\(e, _) -> e == left p) pCurr
     in if null r
          then
            pushVertical (IntMap.delete (left p) $ IntMap.insert p x w) pCurr (Set.insert (d $ left p, '[') $ Set.insert (d p, ']') pNext) d
          else
            pushVertical (IntMap.insert (left p) (snd . head $ r) $ IntMap.insert p x w) (filter (\(e, _) -> e /= left p) pCurr) (Set.insert (d $ left p, '[') $ Set.insert (d p, ']') pNext) d
  _ -> Nothing

moveVertical :: IntMap.IntMap Char -> Int -> (Int -> Int) -> (Int, IntMap.IntMap Char)
moveVertical w p d = case IntMap.lookup (d p) w of
  Nothing -> (d p, w)
  Just '[' -> case pushVertical (IntMap.delete (d p) $ IntMap.delete (d $ right p) w) [(d $ d p, '['), (d . d $ right p, ']')] Set.empty d of
    Nothing -> (p, w)
    Just w' -> (d p, w')
  Just ']' -> case pushVertical (IntMap.delete (d p) $ IntMap.delete (d $ left p) w) [(d $ d p, ']'), (d . d $ left p, '[')] Set.empty d of
    Nothing -> (p, w)
    Just w' -> (d p, w')
  _ -> (p, w)

pushHorizontal :: IntMap.IntMap Char -> Int -> (Int -> Int) -> Char -> Maybe (IntMap.IntMap Char)
pushHorizontal w p d x = case IntMap.lookup p w of
  Nothing -> Just $ IntMap.insert p x w
  Just y -> if y == '[' || y == ']' then pushHorizontal (IntMap.insert p x w) (d p) d y else Nothing

moveHorizontal :: IntMap.IntMap Char -> Int -> (Int -> Int) -> (Int, IntMap.IntMap Char)
moveHorizontal w p d = case IntMap.lookup (d p) w of
  Nothing -> (d p, w)
  Just y ->
    if y == '[' || y == ']'
      then case pushHorizontal (IntMap.delete (d p) w) (d $ d p) d y of
        Nothing -> (p, w)
        Just w' -> (d p, w')
      else (p, w)

parseInput2 :: String -> (IntMap.IntMap Char, String, Int, Int)
parseInput2 content = (warehouse, movements, row, pos)
  where
    [oldTop, bottom] = splitOn "\n\n" content
    top = newWarehouse oldTop
    row = length . takeWhile (/= '\n') $ top
    warehouse = IntMap.fromList . filter (\(_, x) -> x /= '.' && x /= '@') . zip [0 ..] . filter (/= '\n') $ top
    movements = filter (/= '\n') bottom
    pos = length . takeWhile (/= '@') . filter (/= '\n') $ top
    newWarehouse (x : xs)
      | x == '#' || x == '.' = x : x : newWarehouse xs
      | x == 'O' = '[' : ']' : newWarehouse xs
      | x == '@' = '@' : '.' : newWarehouse xs
      | otherwise = x : newWarehouse xs
    newWarehouse [] = []
