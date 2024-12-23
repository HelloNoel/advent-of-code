{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits (Bits (xor))
import Data.List (intercalate)
import qualified Data.Vector as V
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

part1 :: FilePath -> IO ()
part1 = printFromFile $ intercalate "," . map show . runProgram

part2 :: FilePath -> IO ()
part2 = printFromFile parseAndFind

runProgram :: String -> [Int]
runProgram content = compute a b c 0 (V.fromList prog) []
  where
    (a : b : c : prog) = map read . getAllTextMatches $ content =~ "[[:digit:]]+"

parseAndFind :: String -> Maybe Int
parseAndFind content = findA . drop 3 . map read . getAllTextMatches $ content =~ "[[:digit:]]+"

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

compute :: Int -> Int -> Int -> Int -> V.Vector Int -> [Int] -> [Int]
compute a b c p prog out
  | p >= V.length prog = reverse out
  | otherwise = case prog V.! p of
      0 ->
        let a' = a `div` (2 ^ combo)
         in compute a' b c (p + 2) prog out
      1 ->
        let b' = xor b lit
         in compute a b' c (p + 2) prog out
      2 ->
        let b' = combo `mod` 8
         in compute a b' c (p + 2) prog out
      3 ->
        let p' = if a == 0 then p + 2 else lit
         in compute a b c p' prog out
      4 ->
        let b' = xor b c
         in compute a b' c (p + 2) prog out
      5 ->
        let out' = combo `mod` 8 : out
         in compute a b c (p + 2) prog out'
      6 ->
        let b' = a `div` (2 ^ combo)
         in compute a b' c (p + 2) prog out
      7 ->
        let c' = a `div` (2 ^ combo)
         in compute a b c' (p + 2) prog out
      _ -> error "invalid program"
  where
    combo = case prog V.! (p + 1) of
      4 -> a
      5 -> b
      6 -> c
      7 -> error "invalid program"
      x -> x
    lit = prog V.! (p + 1)

findA :: [Int] -> Maybe Int
findA prog = findA' [0] [] $ reverse prog
  where
    progVec = V.fromList prog
    findA' candidates _ []
      | null candidates = Nothing
      | otherwise = Just $ minimum candidates
    findA' candidates acc (p : ps) =
      let acc' = p : acc
          candidates' = [y | x <- candidates, y <- [x * 8 .. x * 8 + 7], compute y 0 0 0 progVec [] == acc']
       in findA' candidates' acc' ps
