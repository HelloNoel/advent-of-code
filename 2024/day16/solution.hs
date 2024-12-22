import Data.List (foldl', partition)
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as Set
import Data.Tuple (swap)
import qualified Data.Vector.Unboxed as VU

part1 :: FilePath -> IO ()
part1 = printFromFile lowestScore

part2 :: FilePath -> IO ()
part2 = printFromFile bestTiles

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

type Pos = (Int, Int)

type Dir = Pos -> Pos

type Node = (Pos, Char)

type Step = (Node, Int)

north :: Dir
north (x, y) = (x, y - 1)

south :: Dir
south (x, y) = (x, y + 1)

east :: Dir
east (x, y) = (x + 1, y)

west :: Dir
west (x, y) = (x - 1, y)

dirToFunc :: Char -> Dir
dirToFunc c
  | c == 'N' = north
  | c == 'S' = south
  | c == 'E' = east
  | c == 'W' = west
  | otherwise = error "invalid direction"

posToInd :: Int -> Pos -> Int
posToInd width (x, y) = x + y * width

indToPos :: Int -> Int -> Pos
indToPos width i = swap $ divMod i width

clockwise :: Char -> Char
clockwise d
  | d == 'N' = 'E'
  | d == 'S' = 'W'
  | d == 'E' = 'S'
  | d == 'W' = 'N'
  | otherwise = error "invalid direction"

anticlockwise :: Char -> Char
anticlockwise d
  | d == 'N' = 'W'
  | d == 'S' = 'E'
  | d == 'E' = 'N'
  | d == 'W' = 'S'
  | otherwise = error "invalid direction"

heuristic :: Pos -> Node -> Int
heuristic (gx, gy) ((x, y), d)
  | y == gy && d == 'N' = abs $ x - gx
  | x == gx && d == 'E' = abs $ y - gy
  | d == 'N' || d == 'E' = abs (x - gx) + abs (y - gy) + 1000
  | otherwise = abs (x - gx) + abs (y - gy) + 2000

next :: VU.Vector Char -> Int -> Node -> [Step]
next maze width (pos, dir)
  | maze VU.! posToInd width nextpos == '#' = turns
  | otherwise = ((nextpos, dir), 1) : turns
  where
    dirFunc = dirToFunc dir
    nextpos = dirFunc pos
    turns = [((pos, clockwise dir), 1000), ((pos, anticlockwise dir), 1000)]

astar :: Node -> Pos -> (Node -> Int) -> (Node -> [Step]) -> Maybe Int
astar startNode endPos heuristicFunc nextFunc = astar' (PQ.singleton (heuristicFunc startNode) (startNode, 0)) Set.empty (Map.singleton startNode 0)
  where
    astar' pq visited costs
      | PQ.null pq = Nothing
      | fst node == endPos = Just cost
      | Set.member node visited = astar' pq' visited costs
      | otherwise = astar' newPQ (Set.insert node visited) newCosts
      where
        ((_, (node, cost)), pq') = PQ.deleteFindMin pq
        nextNodes = [(n, cost + c) | (n, c) <- nextFunc node, Set.notMember n visited, Map.notMember n costs || cost + c < costs Map.! n]
        newPQ = foldl' (\q (n, c) -> PQ.insert (c + heuristicFunc n) (n, c) q) pq' nextNodes
        newCosts = foldl' (\m (n, c) -> Map.insert n c m) costs nextNodes

lowestScore :: String -> Maybe Int
lowestScore content = astar (startPos, 'E') endPos (heuristic endPos) (next maze width)
  where
    width = length $ takeWhile (/= '\n') content
    filteredContent = filter (/= '\n') content
    maze = VU.fromList filteredContent
    startPos = indToPos width $ length $ takeWhile (/= 'S') filteredContent
    endPos = indToPos width $ length $ takeWhile (/= 'E') filteredContent

dijkstra :: Node -> Pos -> (Node -> [Step]) -> Map.Map Node (Set.Set Node)
dijkstra startNode endPos nextFunc = dijkstra' (PQ.singleton 0 startNode) Set.empty (Map.singleton startNode 0) Map.empty
  where
    dijkstra' pq visited costs prev
      | PQ.null pq = prev
      | fst node == endPos || Set.member node visited = dijkstra' pq' visited costs prev
      | otherwise = dijkstra' newPQ (Set.insert node visited) newCosts newPrev
      where
        ((cost, node), pq') = PQ.deleteFindMin pq
        candidateNodes = [(n, cost') | (n, c) <- nextFunc node, Set.notMember n visited, let cost' = cost + c, Map.notMember n costs || cost' <= costs Map.! n]
        (improvedNodes, equalNodes) = partition (\(n, c) -> Map.notMember n costs || cost + c < costs Map.! n) candidateNodes
        newPQ = foldl' (\q (n, c) -> PQ.insert c n q) pq' improvedNodes
        newCosts = foldl' (\m (n, c) -> Map.insert n c m) costs improvedNodes
        improvedPrev = foldl' (\p (n, _) -> Map.insert n (Set.singleton node) p) prev improvedNodes
        newPrev = foldl' (\p (n, _) -> Map.adjust (Set.insert node) n p) improvedPrev equalNodes

findPaths :: Map.Map Node (Set.Set Node) -> [Node] -> Set.Set Node -> Set.Set Pos
findPaths _ [] acc = Set.map fst acc
findPaths m (n : ns) acc
  | Set.member n acc = findPaths m ns acc
  | otherwise = findPaths m (prev ++ ns) $ Set.insert n acc
  where
    prev = maybe [] Set.toList (m Map.!? n)

bestTiles :: String -> Int
bestTiles content = Set.size $ findPaths prevNodes [(endPos, 'N'), (endPos, 'E')] Set.empty
  where
    width = length $ takeWhile (/= '\n') content
    filteredContent = filter (/= '\n') content
    maze = VU.fromList filteredContent
    startPos = indToPos width $ length $ takeWhile (/= 'S') filteredContent
    endPos = indToPos width $ length $ takeWhile (/= 'E') filteredContent
    prevNodes = dijkstra (startPos, 'E') endPos (next maze width)
