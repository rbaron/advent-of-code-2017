import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Map as Map

type Canvas = [String]
type Moves = Map.Map Canvas Canvas

flipH :: Canvas -> Canvas
flipH c = map reverse c

flipV :: Canvas -> Canvas
flipV c = List.transpose $ map reverse $ List.transpose c

rotate :: Canvas -> Canvas
rotate = flipH . List.transpose

variations :: Canvas -> [Canvas]
variations c = [
    c,
    flipH c,
    flipV c,
    rotate c,
    flipH $ rotate c,
    flipV $ rotate c,
    rotate $ rotate c,
    flipH $ rotate $ rotate c,
    flipV $ rotate $ rotate c,
    rotate $ rotate $ rotate c,
    flipH $ rotate $ rotate $ rotate c,
    flipV $ rotate $ rotate $ rotate c
  ]

next :: Canvas -> Moves -> Canvas
next canvas moves =
  let vars = variations canvas
      from = head $ filter (\c -> Map.member c moves) vars
    in moves Map.! from

runTimes :: Int -> Canvas -> Moves -> Canvas
runTimes 0 canvas _ = canvas
runTimes n canvas moves =
  let canvases    = split canvas
      newCanvases = map (\l -> map (\c -> next c moves) l) canvases
    in runTimes (n - 1) (join newCanvases) moves

parseLine :: String -> (Canvas, Canvas)
parseLine line =
  let ws            = words line
      (from:_:to:_) = ws
    in (Split.splitOn "/" from, Split.splitOn "/" to)

split :: Canvas -> [[Canvas]]
split canvas =
  let size       = length $ head canvas
      n          = if size `rem` 2 == 0 then 2 else 3
      lines      = Split.chunksOf n canvas
      transLines = map List.transpose lines
      chunkLines = map (Split.chunksOf n) transLines
      canvases   = map (\l -> map (\c -> List.transpose c) l) chunkLines
    in canvases

join :: [[Canvas]] -> Canvas
join cs = concat $ map joinLine cs

joinLine :: [Canvas] -> Canvas
joinLine canvases =
  let trans = map List.transpose canvases
    in List.transpose $ concat trans

countSetBits :: Canvas -> Int
countSetBits canvas = sum $ map sumLine canvas
  where sumLine :: String -> Int
        sumLine l = sum $ map (\c -> if c == '#' then 1 else 0) l

main = do
  contents <- readFile "input.txt"
  let moves  = Map.fromList $ map parseLine $ lines contents
      lst    = ".#./..#/###"
      canvas = Split.splitOn "/" lst
    in do
      -- Part #1
      print $ countSetBits $ runTimes 5 canvas moves

      -- Part #2
      print $ countSetBits $ runTimes 18 canvas moves
