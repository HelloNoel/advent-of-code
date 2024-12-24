{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Function (on)
import Data.List (foldl', sortBy, stripPrefix)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set

part1 :: FilePath -> IO ()
part1 = printFromFile $ validDesigns . parseInput

part2 :: FilePath -> IO ()
part2 = printFromFile $ allValidDesigns . parseInput

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

combination :: Set.Set String -> String -> String -> (Set.Set String, Bool)
combination memo [] [] = (memo, True)
combination memo _ [] = (memo, False)
combination memo [] t@(x : xs)
  | Set.member t memo = (memo, True)
  | otherwise = combination memo [x] xs
combination memo s t@(x : xs)
  | Set.member t memo =
      let s' = reverse s
          (memo', found) = combination memo [] s'
       in if found then (Set.insert (s' ++ t) memo', True) else combination memo' (x : s) xs
  | otherwise = combination memo (x : s) xs

parseInput :: String -> ([String], [String])
parseInput content = (towels, designs)
  where
    [top, bottom] = splitOn "\n\n" content
    towels = splitOn ", " top
    designs = filter (/= "") $ splitOn "\n" bottom

validDesigns :: ([String], [String]) -> Int
validDesigns (towels, designs) = validDesigns' (Set.fromList towels) designs 0
  where
    validDesigns' memo (d : ds) acc =
      let (memo', found) = combination memo [] d
       in validDesigns' memo' ds (if found then acc + 1 else acc)
    validDesigns' _ [] acc = acc

allCombinations :: Map.Map String Int -> [String] -> String -> (Map.Map String Int, Int)
allCombinations memo _ [] = (memo, 0)
allCombinations memo towels design = case memo Map.!? design of
  Just count -> (memo, count)
  Nothing ->
    let (memo', count) =
          foldl'
            ( \(m, c) d ->
                let (m', c') = allCombinations m towels d
                 in (m', c + c')
            )
            (memo, 0)
            $ mapMaybe (`stripPrefix` design) towels
     in (Map.insert design count memo', count)

allValidDesigns :: ([String], [String]) -> Int
allValidDesigns (towels, designs) = allValidDesigns' (setupMemo towels) designs 0
  where
    allValidDesigns' memo (d : ds) acc =
      let (memo', acc') = allCombinations memo towels d
       in allValidDesigns' memo' ds (acc + acc')
    allValidDesigns' _ [] acc = acc

setupMemo :: [String] -> Map.Map String Int
setupMemo towels = setupMemo' Map.empty $ sortBy (compare `on` length) towels
  where
    setupMemo' memo [] = memo
    setupMemo' memo (x : xs) =
      let (memo', c) = allCombinations memo towels x
       in setupMemo' (Map.insert x (c + 1) memo') xs
