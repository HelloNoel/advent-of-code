import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Tuple (swap)
import qualified Data.Vector.Unboxed as VU

part1 :: FilePath -> IO ()
part1 = printFromFile allCheats

part2 :: FilePath -> IO ()
part2 = printFromFile allCheats2

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

type Pos = (Int, Int)

type Dir = Pos -> Pos

up :: Dir
up (x, y) = (x, y - 1)

down :: Dir
down (x, y) = (x, y + 1)

right :: Dir
right (x, y) = (x + 1, y)

left :: Dir
left (x, y) = (x - 1, y)

parseInput :: String -> (Int, Int, VU.Vector Char)
parseInput content = (width, height, track)
  where
    width = length $ takeWhile (/= '\n') content
    height = length . lines $ content
    track = VU.fromList $ filter (/= '\n') content

posToInd :: Int -> Int -> Pos -> Maybe Int
posToInd width height p = case checkPos width height p of
  Just (x, y) -> Just $ x + y * width
  Nothing -> Nothing

indToPos :: Int -> Int -> Int -> Maybe Pos
indToPos width height i = checkPos width height . swap $ divMod i width

checkPos :: Int -> Int -> Pos -> Maybe Pos
checkPos width height (x, y)
  | x >= 0 && y >= 0 && x < width && y < height = Just (x, y)
  | otherwise = Nothing

neighbours :: Int -> Int -> Int -> [Int]
neighbours width height ind = mapMaybe (posToInd width height) n
  where
    pos = fromJust $ indToPos width height ind
    n = [up pos, down pos, left pos, right pos]

findRoute :: Int -> Int -> VU.Vector Char -> IntMap.IntMap Int
findRoute width height track = findRoute' 0 end $ IntMap.singleton end 0
  where
    start = fromJust $ VU.findIndex (== 'S') track
    end = fromJust $ VU.findIndex (== 'E') track
    findRoute' dis ind route
      | ind == start = route
      | otherwise =
          let nextInd = fromJust . find (\x -> let t = track VU.! x in (t == '.' || t == 'S') && IntMap.notMember x route) $ neighbours width height ind
           in findRoute' (dis + 1) nextInd (IntMap.insert nextInd (dis + 1) route)

cheats :: Int -> Int -> Int -> [Int]
cheats width height ind = let pos = fromJust $ indToPos width height ind in mapMaybe (posToInd width height) $ getCheats pos
  where
    getCheats :: Pos -> [Pos]
    getCheats p = [up $ up p, down $ down p, left $ left p, right $ right p]

findCheats :: Int -> Int -> IntMap.IntMap Int -> [(Int, Int)]
findCheats width height routes = findCheats' (IntMap.toList routes) []
  where
    findCheats' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    findCheats' [] res = res
    findCheats' ((i, d) : xs) res =
      let posCheats = cheats width height i
          newDist = map (routes IntMap.!?) posCheats
          newCheats = map (\(i', _) -> (i, i')) . filter (\(_, d') -> isJust d' && d - fromJust d' > 100) $ zip posCheats newDist
       in findCheats' xs (newCheats ++ res)

allCheats :: String -> Int
allCheats content = length $ findCheats width height routes
  where
    (width, height, track) = parseInput content
    routes = findRoute width height track

distance :: Int -> Int -> Int -> Int -> Int
distance width height i j = abs (ix - jx) + abs (iy - jy)
  where
    (ix, iy) = fromJust $ indToPos width height i
    (jx, jy) = fromJust $ indToPos width height j

findCheats2 :: Int -> Int -> IntMap.IntMap Int -> [(Int, Int, Int, Int)]
findCheats2 width height routes = findCheats2' routeList
  where
    routeList = IntMap.toList routes
    findCheats2' ((i, d) : rs) =
      let c = filter (\(_, _, d1, d2) -> d1 - d2 >= 100 && d2 <= 20) $ map (\(ind, dis) -> (i, ind, d - dis, distance width height i ind)) routeList
       in c ++ findCheats2' rs
    findCheats2' [] = []

allCheats2 :: String -> Int
allCheats2 content = length $ findCheats2 width height routes
  where
    (width, height, track) = parseInput content
    routes = findRoute width height track