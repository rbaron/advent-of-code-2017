import qualified Data.Map as Map


type Vector    = (Integer, Integer)
type Position  = Vector
type Direction = Vector
type Grid      = Map.Map Position Char


turnLeft :: Direction -> Direction
turnLeft d
  | d == (0, -1) = (-1, 0)
  | d == (-1, 0) = (0, 1)
  | d == (0, 1) = (1, 0)
  | d == (1, 0) = (0, -1)

turnRight :: Direction -> Direction
turnRight = turnLeft .  turnLeft . turnLeft

move :: Position -> Direction -> Position
move (px, py) (dx, dy) = (px+dx, py+dy)

walkAndCountInfections :: Integer -> Grid -> Position -> Direction -> Integer -> Integer
walkAndCountInfections 0 _ _ _ count = count
walkAndCountInfections n grid pos dir inf =
  case Map.findWithDefault '.' pos grid of
    '.' -> let newDir = turnLeft dir
               newPos = move pos newDir
             in walkAndCountInfections (n-1) (Map.insert pos '#' grid) newPos newDir (inf + 1)

    '#' -> let newDir = turnRight dir
               newPos = move pos newDir
             in walkAndCountInfections (n-1) (Map.insert pos '.' grid) newPos newDir inf

walkAndCountInfections2 :: Integer -> Grid -> Position -> Direction -> Integer -> Integer
walkAndCountInfections2 0 _ _ _ count = count
walkAndCountInfections2 n grid pos dir inf =
  case Map.findWithDefault '.' pos grid of
    '.' -> let newDir = turnLeft dir
               newPos = move pos newDir
             in walkAndCountInfections2 (n-1) (Map.insert pos 'w' grid) newPos newDir inf

    'w' -> let newDir = dir
               newPos = move pos newDir
             in walkAndCountInfections2 (n-1) (Map.insert pos '#' grid) newPos newDir (inf + 1)

    '#' -> let newDir = turnRight dir
               newPos = move pos newDir
             in walkAndCountInfections2 (n-1) (Map.insert pos 'f' grid) newPos newDir inf

    'f' -> let newDir = (turnRight . turnRight) dir
               newPos = move pos newDir
             in walkAndCountInfections2 (n-1) (Map.insert pos '.' grid) newPos newDir inf


main = do
  content <- readFile "input.txt"
  let ls     = zip [0..] $ lines content
      lst    = map (\(y,l) -> map (\(x,c) -> ((x,y), c)) (zip [0..] l)) ls
      grid   = Map.fromList $ concat lst
      middle = fromIntegral $ (length lst - 1) `div` 2
      pos    = (middle, middle)
      dir    = (0, -1)
    in do
      print $ walkAndCountInfections 10000 grid pos dir 0
      print $ walkAndCountInfections2 10000000 grid pos dir 0
