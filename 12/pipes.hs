import qualified Data.Map as Map
import qualified Data.Set as Set

maybeStripComma :: String -> String
maybeStripComma s =
  if last s == ','
  then init s
  else s

parseLine :: String -> (String, [String])
parseLine str = case words str of
  (origin:_:dest:[]) -> (origin, [dest])
  (origin:_:dests) -> (origin, map maybeStripComma dests)

buildMap :: [(String, [String])] -> Map.Map String [String]
buildMap conns = foldl (\m (from, tos) -> Map.insert from tos m) Map.empty conns

collect :: String -> Map.Map String [String] -> Set.Set String -> Set.Set String
collect origin conns visited =
  if Set.member origin visited
  then visited
  else case Map.lookup origin conns of
    Just children ->
      foldl
      (\vis child -> Set.union vis $ collect child conns vis)
      (Set.insert origin visited)
      children
    Nothing       -> visited


main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsedLines = map parseLine (lines input)
      connections = buildMap parsedLines
      -- Part #1
      visited     = collect "0" connections Set.empty

      -- Part #2
      allOrigins  = Map.keys connections
      groups      = map (\origin -> collect origin connections Set.empty) allOrigins

    in do print $ Set.size visited
          print $ Set.size $ Set.fromList groups

