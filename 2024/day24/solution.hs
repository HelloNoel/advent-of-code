{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Bits (xor)
import Data.List (foldl', intercalate, sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

part1 :: FilePath -> IO ()
part1 = printFromFile parseAndOutput

part2 :: FilePath -> IO ()
part2 = printFromFile $ intercalate "," . sort . checkOutputs 44 . parseConnections Map.empty . (!! 1) . splitOn "\n\n"

printFromFile :: (Show a) => (String -> a) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

type Op = Bool -> Bool -> Bool

data Wire = CONST Bool | GATE String Op String

type Circuit = Map.Map String Wire

evalWire :: Circuit -> String -> (Circuit, Bool)
evalWire c wire = case c Map.! wire of
  CONST x -> (c, x)
  GATE s1 op s2 ->
    let (c1, v1) = evalWire c s1
        (c2, v2) = evalWire c1 s2
        res = v1 `op` v2
     in (Map.insert wire (CONST res) c2, res)

fromBin :: [Bool] -> Int
fromBin = foldl' (\acc x -> acc * 2 + fromEnum x) 0

parseValues :: Circuit -> String -> Circuit
parseValues c str = foldl' (\c' [k, v] -> Map.insert k (CONST . toEnum $ read v) c') c xs
  where
    xs = map (splitOn ": ") $ lines str

parseGates :: Circuit -> String -> Circuit
parseGates c str = foldl' (\c' [x1, op, x2, _, k] -> Map.insert k (GATE x1 (toOp op) x2) c') c xs
  where
    xs = map words $ lines str

toOp :: String -> Bool -> Bool -> Bool
toOp op = case op of
  "AND" -> (&&)
  "OR" -> (||)
  "XOR" -> xor
  _ -> error "invalid operator"

decOutput :: Circuit -> Int
decOutput c = fromBin . snd $ foldl' (\(cOld, bs) k -> let (cNew, b) = evalWire cOld k in (cNew, b : bs)) (c, []) outputs
  where
    outputs = filter (\k -> head k == 'z') $ Map.keys c

parseAndOutput :: String -> Int
parseAndOutput content = decOutput . (`parseGates` g) $ parseValues Map.empty v
  where
    [v, g] = splitOn "\n\n" content

data AdderOut = RES (Set.Set String) String (Set.Set (Set.Set String)) | ERR [(Set.Set String, String)]

type Connections = Map.Map (Set.Set String) String

parseConnections :: Connections -> String -> Connections
parseConnections m str = foldl' (\m' [x1, op, x2, _, out] -> Map.insert (Set.fromList [x1, op, x2]) out m') m xs
  where
    xs = map words $ lines str

checkHalfAdder :: Connections -> String -> String -> String -> AdderOut
checkHalfAdder m x y z1
  | z1 == z2 = RES kcOut cOut (Set.singleton kz2)
  | otherwise = ERR [(kz2, z2)]
  where
    kz2 = Set.fromList [x, "XOR", y]
    z2 = m Map.! kz2
    kcOut = Set.fromList [x, "AND", y]
    cOut = m Map.! kcOut

checkFullAdder :: Connections -> String -> String -> String -> Set.Set String -> String -> AdderOut
checkFullAdder m x y z1 kcIn cIn = case mz2 of
  Nothing -> ERR [(kxXORy, xXORy)]
  Just z2 ->
    if z1 /= z2
      then ERR [(kz2, z2)]
      else case mcANDxy of
        Nothing -> ERR [(kcIn, cIn)]
        Just cANDxy -> case mcOut of
          Nothing -> ERR [(kxANDy, xANDy), (kcANDxy, cANDxy)]
          Just cOut -> RES kcOut cOut (Set.fromList [kcIn, kxXORy, kz2, kcANDxy, kxANDy])
  where
    kxXORy = Set.fromList [x, "XOR", y]
    xXORy = m Map.! kxXORy
    kz2 = Set.fromList [cIn, "XOR", xXORy]
    mz2 = m Map.!? kz2
    kcANDxy = Set.fromList [cIn, "AND", xXORy]
    mcANDxy = m Map.!? kcANDxy
    kxANDy = Set.fromList [x, "AND", y]
    xANDy = m Map.! kxANDy
    kcOut = Set.fromList [xANDy, "OR", fromJust mcANDxy]
    mcOut = m Map.!? kcOut

checkOutputs :: Int -> Connections -> [String]
checkOutputs bitSize m = case checkHalfAdder m "x00" "y00" "z00" of
  RES kc00 c00 v -> checkOutputs' m "01" v kc00 c00
  ERR es ->
    let (res1, res2, newM) = findSwapFromList m es Set.empty "" Set.empty "00" (Map.toList m)
     in res1 : res2 : checkOutputs bitSize newM
  where
    checkOutputs' m' adderNum visited kc1 c1
      | read adderNum > bitSize = []
      | otherwise = case checkFullAdder m' ('x' : adderNum) ('y' : adderNum) ('z' : adderNum) kc1 c1 of
          RES kc2 c2 v -> checkOutputs' m' (nextNum adderNum) (Set.union visited v) kc2 c2
          ERR es ->
            let (res1, res2, newM) = findSwapFromList m' es kc1 c1 visited adderNum (Map.toList m')
             in res1 : res2 : checkOutputs' newM adderNum visited kc1 c1

nextNum :: String -> String
nextNum s
  | n < 10 = '0' : show n
  | otherwise = show n
  where
    n :: Int
    n = read s + 1

findSwapFromList :: Connections -> [(Set.Set String, String)] -> Set.Set String -> String -> Set.Set (Set.Set String) -> String -> [(Set.Set String, String)] -> (String, String, Connections)
findSwapFromList m ((k, v) : es) kc c visited adderNum xs = case findSwap m k v kc c visited adderNum xs of
  Just x -> x
  Nothing -> findSwapFromList m es kc c visited adderNum xs
findSwapFromList _ [] _ _ _ _ _ = error "no valid swap found"

findSwap :: Connections -> Set.Set String -> String -> Set.Set String -> String -> Set.Set (Set.Set String) -> String -> [(Set.Set String, String)] -> Maybe (String, String, Connections)
findSwap m k1 v1 kc1 c1 visited adderNum ((k2, v2) : xs)
  | k1 == k2 || Set.member k2 visited = findSwap m k1 v1 kc1 c1 visited adderNum xs
  | Set.null kc1 = case checkHalfAdder m' ('x' : adderNum) ('y' : adderNum) ('z' : adderNum) of
      RES {} -> Just (v1, v2, m')
      ERR es -> if v2 `elem` map snd es then findSwap m k1 v1 kc1 c1 visited adderNum xs else Just (v1, v2, m')
  | otherwise = case checkFullAdder m' ('x' : adderNum) ('y' : adderNum) ('z' : adderNum) kc1 c1 of
      RES {} -> Just (v1, v2, m')
      ERR es -> if v2 `elem` map snd es then findSwap m k1 v1 kc1 c1 visited adderNum xs else Just (v1, v2, m')
  where
    m' = Map.insert k1 v2 $ Map.insert k2 v1 m
findSwap _ _ _ _ _ _ _ [] = Nothing
