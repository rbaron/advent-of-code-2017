type Map = [[Char]]
type Pos = (Int, Int)
type Dir = Pos


nextPos :: Map -> Pos -> Dir -> (Pos, Dir, Bool)
nextPos m (x, y) (dx, dy) =
  let possibleDirs = [ (dx, dy), (0, 1), (0, -1), (1, 0), (-1, 0) ]
      validDir:: Dir -> Bool
      validDir (pDx, pDy) =
        let (maybeX, maybeY) = (x + pDx, y + pDy)
        in if (maybeY >= 0) && (maybeY < (length m)) &&
              (maybeX >= 0) && (maybeX < (length $ m !! maybeY)) &&
              ((m !! maybeY) !! maybeX /= ' ') && not (pDx == -dx && pDy == -dy)
           then True
           else False
      availableDirs = filter validDir possibleDirs
  in if not $ null availableDirs
     then let (newDx, newDy) = head availableDirs
          in ((x + newDx, y + newDy), (newDx, newDy), False)
     else ((0, 0), (0, 0), True)

walk :: Map -> Pos -> Dir -> Integer -> ([Char], Integer)
walk m pos dir steps =
  let (newPos, newDir, halt) = nextPos m pos dir
  in if halt
     then ([], steps)
     else let (x, y) = newPos
              c      = m !! y !! x
              (newVisited, newSteps) = walk m newPos newDir steps
          in if c `elem` " |-+"
             then (newVisited, newSteps + 1)
             else (c : newVisited, newSteps + 1)

main = do
  input <- readFile "input.txt"
  let m          = lines input
      initialPos = (length (head m) - 1, 0)
      initialDir = (0, 1)
    in do
      print $ walk m initialPos initialDir 1
