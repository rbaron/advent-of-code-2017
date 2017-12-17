import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace


type Direction = String

{- Solution #1 -}

type Coord = (Int, Int)

step :: Coord -> Direction -> Coord
step (x, y) dir
  | dir == "n"  = (x    , y - 2)
  | dir == "s"  = (x    , y + 2)
  | dir == "nw" = (x - 1, y - 1)
  | dir == "sw" = (x - 1, y + 1)
  | dir == "ne" = (x + 1, y - 1)
  | dir == "se" = (x + 1, y + 1)

walk :: Coord -> [Direction] -> Coord
walk coord [] = coord
walk origin (d:ds) = walk (step origin d) ds

bfs :: [(Int, Coord)] -> Int
bfs ((dist, x):rest)
  | x == (0, 0) = dist
  | otherwise   =
    let neighbors = map (step x) ["n", "s", "nw", "sw", "ne", "se"]
      in bfs $ rest ++ zip (cycle [dist + 1]) neighbors

{- Solution #2 -}

type CubeCoord = (Int, Int, Int)

stepCube :: CubeCoord -> Direction -> CubeCoord
stepCube (x, y, z) dir
  | dir == "n"  = (x + 0, y + 1, z - 1)
  | dir == "ne" = (x + 1, y + 0, z - 1)
  | dir == "se" = (x + 1, y - 1, z + 0)
  | dir == "s"  = (x + 0, y - 1, z + 1)
  | dir == "sw" = (x - 1, y + 0, z + 1)
  | dir == "nw" = (x - 1, y + 1, z + 0)

walkCube :: (Int, CubeCoord) -> [Direction] -> (Int, CubeCoord)
walkCube (maxDist, coord) [] = (maxDist, coord)
walkCube (maxDist, origin) (d:ds) =
  let newPos = stepCube origin d
      newMax = maximum [maxDist, (distCube newPos)]
    in walkCube (newMax, newPos) ds

distCube :: CubeCoord -> Int
distCube (x, y, z) = maximum $ map abs $ [x, y, z]

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- path = words "se sw se sw sw"

main :: IO ()
main = do
  input <- readFile "input.txt"
  let line   = head $ lines $ input
      path = wordsWhen (== ',') line
    in do

      -- Solution #1 - using BFS - slow and wasteful
      -- let pos = walk (0, 0) path
      --   in do print pos
      --         print $ bfs [(0, pos)]

      -- Solution #2 - using cube coordinates
      let (maxDist, finalPos) = walkCube (0, (0, 0, 0)) path
        in do print finalPos
              print $ distCube finalPos
              print $ maxDist
