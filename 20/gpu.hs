import qualified Data.List  as L
import qualified Text.Regex as R


type Vector = (Integer, Integer, Integer)
data MetaPoint = MetaPoint Vector Vector Vector deriving (Show)

fromListMeta :: [[Char]] -> MetaPoint
fromListMeta ls =
  let ints = map (\cs -> read cs :: Integer) ls
      (px:py:pz:vx:vy:vz:ax:ay:az:_) = ints
    in MetaPoint (px, py, pz) (vx, vy, vz) (ax, ay, az)

n  = "(-?[0-9]+)"
n3 = "<" ++ n ++ "," ++ n ++ "," ++ n ++ ">"
inputRegex  = R.mkRegex $ "p=" ++ n3 ++", v=" ++ n3 ++ ", a=" ++ n3

parseLine :: String -> MetaPoint
parseLine s =
  case R.matchRegex inputRegex s of
    Just points -> fromListMeta points
    Nothing     -> error "Invalid input format"

norm :: Vector -> Float
norm (x, y, z) = sqrt $ fromIntegral $ x^2 + y^2 + z^2

sumVectors :: Vector -> Vector -> Vector
sumVectors (a, b, c) (x, y, z) = (a+x, b+y, c+z)

update :: MetaPoint -> MetaPoint
update (MetaPoint p v a) =
  let newV = sumVectors v a
      newP = sumVectors p newV
    in MetaPoint newP newV a

filterOutCollisions :: [MetaPoint] -> [MetaPoint]
filterOutCollisions metaPoints =
  let groups = L.groupBy (\(MetaPoint p1 _ _) (MetaPoint p2 _ _) -> p1 == p2) metaPoints
      valid  = filter (\g -> length g == 1) groups
    in concat valid

step :: Integer -> [MetaPoint] -> [MetaPoint]
step 0 pts = pts
step i pts =
  let valid = filterOutCollisions pts
    in step (i - 1) (map update valid)

main = do
  content <- readFile "input.txt"
  let metaPoints    = map parseLine $ lines content
      metaWithIndex = zip [0..] metaPoints

      sortingFn :: (Integer, MetaPoint) -> (Integer, MetaPoint) -> Ordering
      sortingFn (i0, (MetaPoint p1 v1 a1)) (i1, (MetaPoint p2 v2 a2)) = compare (norm a1) (norm a2)
    in do
      -- Part #1
      print $ L.minimumBy sortingFn metaWithIndex

      -- Part #2
      print $ length $ step 1000 metaPoints
