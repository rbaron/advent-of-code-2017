import qualified Data.Map as Map
import Debug.Trace

type Layer = Integer
type Range = Integer
type Time  = Integer

parseLine :: String -> (Layer, Range)
parseLine line =
  let layer:range:_ = words line
    in (read (init layer), read range)

calculateSeverity :: Time -> Layer -> Map.Map Layer Range -> Integer
calculateSeverity currTime currLayer ranges =
  let maxLayer = maximum $ Map.keys ranges
    in if currLayer > maxLayer
    then 0
    else case Map.lookup currLayer ranges of
      Nothing    -> calculateSeverity (currTime + 1) (currLayer + 1) ranges
      Just range ->
        if (currTime `rem` (2*range - 2)) == 0
        then currLayer * range + calculateSeverity (currTime + 1) (currLayer + 1) ranges
        else calculateSeverity (currTime + 1) (currLayer + 1) ranges

caughtAtLayer0 :: Time -> Map.Map Layer Range -> Bool
caughtAtLayer0 delay ranges =
  case Map.lookup 0 ranges of
    Nothing    -> False
    Just range -> delay `rem` (2*range - 2) == 0

caught :: Time -> Layer -> Map.Map Layer Range -> Bool
caught time layer ranges =
  let maxLayer = maximum $ Map.keys ranges
    in if layer > maxLayer
       then False
       else case Map.lookup layer ranges of
         Nothing    -> caught (time + 1) (layer + 1) ranges
         Just range -> if (time `rem` (2*range - 2)) == 0
                       then True
                       else caught (time + 1) (layer + 1) ranges

findDelay :: Time -> Map.Map Layer Range -> Time
findDelay t ranges =
  if caught t 0 ranges
  then findDelay (t + 1) ranges
  else t

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = map parseLine $ lines input
      ranges = Map.fromList parsed
    in do
      -- Part #1
      print $ calculateSeverity 0 0 ranges

      -- Part #2
      print $ findDelay 0 ranges
