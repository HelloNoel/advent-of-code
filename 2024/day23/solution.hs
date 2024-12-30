import Data.Foldable (foldl', maximumBy)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Ord (comparing)
import qualified Data.Set as Set

part1 :: FilePath -> IO ()
part1 = printFromFile $ Set.size . interconneted . neighbours . map (\x -> let (a, b) = break (== '-') x in (a, tail b)) . lines

part2 :: FilePath -> IO ()
part2 = printFromFile $ intercalate "," . Set.toList . maxClique . neighbours . map (\x -> let (a, b) = break (== '-') x in (a, tail b)) . lines

printFromFile :: (Show b) => (String -> b) -> FilePath -> IO ()
printFromFile f input = do
  content <- readFile input
  print . f $ content

neighbours :: [(String, String)] -> Map.Map String (Set.Set String)
neighbours = foldl' (\m (a, b) -> Map.insertWith Set.union a (Set.singleton b) $ Map.insertWith Set.union b (Set.singleton a) m) Map.empty

interconneted :: Map.Map String (Set.Set String) -> Set.Set (Set.Set String)
interconneted lanparty = Map.foldlWithKey' (\ss k s -> Set.union ss $ Set.foldl' (allTriangles k s) Set.empty s) Set.empty $ Map.filterWithKey (\k _ -> head k == 't') lanparty
  where
    triangles a b s x
      | x /= a && x /= b = Set.insert (Set.fromList [a, b, x]) s
      | otherwise = s
    allTriangles k s ss' str = Set.union ss' . Set.foldl' (triangles k str) Set.empty . Set.intersection s $ lanparty Map.! str

maxClique :: Map.Map String (Set.Set String) -> Set.Set String
maxClique lanparty = maximumBy (comparing Set.size) . Set.toList . Set.map (Set.map snd) $ bronKerbosch Set.empty neighbourSet Set.empty
  where
    mapWithSize = Map.mapWithKey (\k v -> (Set.size v, k)) lanparty
    neighbourMap = Map.map (Set.map (mapWithSize Map.!)) lanparty
    neighbourSet = Map.foldl' (flip Set.insert) Set.empty mapWithSize
    bronKerbosch r p x
      | Set.null p && Set.null x = Set.singleton r
      | otherwise =
          let u = Set.findMax $ Set.union p x
              nu = neighbourMap Map.! snd u
              (_, _, res) =
                Set.foldl'
                  ( \(p', x', s') v ->
                      let nv = neighbourMap Map.! snd v
                       in (Set.delete v p', Set.insert v x', Set.union s' $ bronKerbosch (Set.insert v r) (Set.intersection p nv) (Set.intersection x nv))
                  )
                  (p, x, Set.empty)
                  (Set.difference p nu)
           in res
