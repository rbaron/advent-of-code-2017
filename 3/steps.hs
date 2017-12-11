import qualified Data.Map as Map
import qualified Data.List as List

nextDir :: String -> String
nextDir dir
  | dir == "up"    = "left"
  | dir == "left"  = "down"
  | dir == "down"  = "right"
  | dir == "right" = "up"

currentSquareSize :: Int -> Int
currentSquareSize pos =
  let side = sqrt $ fromIntegral pos
      in floor side

nextPos :: String -> (Int, Int) -> (Int, Int)
nextPos dir (x, y)
  | dir == "up"    = (x, y+1)
  | dir == "right" = (x+1, y)
  | dir == "left"  = (x-1, y)
  | dir == "down"  = (x, y-1)

isPerfectSquare :: Int -> Bool
isPerfectSquare n =
  let s = sqrt (fromIntegral (n))
      f = fromIntegral $ floor s
  in abs(s - f) < 10^^(-6)

getCenter :: Int -> (Float, Float)
getCenter side
  | rem side 2 == 0 = (0  , 0)
  | otherwise       = (0.5, 0.5)

isCorner :: (Float, Float) -> (Int, Int) -> Bool
isCorner (cx, cy) (px, py) =
  let fx = fromIntegral px
      fy = fromIntegral py
  in abs((abs (cx - fx) - abs (cy - fy))) < (10 ^^ (-6))

nextCell :: String -> Int -> (Int, Int) -> (String, Int, Int)
nextCell dir pos (x, y) =
  if isPerfectSquare pos

  then let (newX, newY) = nextPos dir (x, y)
       in (dir, newX, newY)

  else let side     = currentSquareSize pos
           center   = getCenter side
           in if isCorner center (x, y)
              then let newDir = nextDir dir
                       (newX, newY) = nextPos newDir (x, y)
                       in (newDir, newX, newY)
              else let (newX, newY) = nextPos dir (x, y)
                   in (dir, newX, newY)

makeGrid :: String -> Int -> (Int, Int) -> [(Int, String, Int, Int)]
makeGrid dir pos (x, y) =
  let (newDir, newX, newY) = nextCell dir pos (x, y)
      in (pos, dir, x, y) : makeGrid newDir (pos + 1) (newX, newY)

directions :: String -> Int -> [String]
directions dir count =
  let next  = nextDir dir
      nnext = nextDir next
      in replicate count dir ++ replicate count next ++ directions nnext (count + 1)

makeGrid' :: (Int, Int) -> [String] -> [(Int, Int)]
makeGrid' (x, y) (dir:dirs) = (x, y) : makeGrid' (nextPos dir (x, y)) dirs

makeSumGrid :: Map.Map (Int, Int) Int -> [(Int, Int)] -> [Int]
makeSumGrid m ((x, y) : gridTail) =
  let neighbors   = [(x+i, y+j) | i <- [-1, 0, 1], j <- [-1, 0, 1]]
      neighborSum = sum $ map (\coords -> Map.findWithDefault 0 coords m) neighbors
      newMap      = Map.insert (x, y) neighborSum m
      in neighborSum : makeSumGrid newMap gridTail

-- pos = 12
-- pos = 1024
pos = 265149


main = do

  -- Part #1

  -- Method #1 - stupid
  let lst              = makeGrid "right" 1 (0, 0)
      (idx, dir, x, y) = lst !! (pos - 1)
      in putStrLn $ show $ abs(x) + abs(y)

  -- Method #2 - less stupid
  let dirs   = directions "right" 1
      grid   = makeGrid' (0, 0) dirs
      (x, y) = grid !! (pos - 1)
      in putStrLn $ show $ abs(x) + abs(y)

  -- Part #2

  let dirs           = directions "right" 1
      grid           = makeGrid' (0, 0) dirs
      initialSumGrid = Map.fromList [((0, 0), 1)]
      sumGrid        = makeSumGrid initialSumGrid grid
      in putStrLn $ show $ List.find (> pos) sumGrid
