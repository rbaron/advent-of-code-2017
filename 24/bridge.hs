import qualified Data.List.Split as Split
import qualified Data.List as List

import Debug.Trace

type Port = Integer
data Component = Component Port Port deriving (Eq, Show)

parseLine :: String -> Component
parseLine s =
  let from:to:_ = Split.splitOn "/" s
    in Component (read from :: Integer) (read to :: Integer)

otherPort :: Port -> Component -> Port
otherPort port (Component p1 p2) = if port == p1 then p2 else p1

allBridges :: Port -> [Component] -> [[Component]]
allBridges port comps =
  let nextAvailableComps = filter (\(Component p1 p2) -> p1 == port || p2 == port) comps
      nextAvailableBridges = map (\c -> allBridges (otherPort port c) (List.delete c comps)) nextAvailableComps
      nextBridges = map (\(c, bs) -> map (\b -> c:b) bs) $ zip nextAvailableComps nextAvailableBridges
    in []:(concat nextBridges)

strength :: [Component] -> Integer
strength [] = 0
strength ((Component from to):rest) = from + to + (strength rest)

main = do
  contents <- readFile "input.txt"
  let components = map parseLine $ lines contents
      bridges    = allBridges 0 components
      sorted     = List.sortBy (\b1 -> \b2 -> compare (length b2, strength b2) (length b1, strength b1)) bridges
    in do
      print $ maximum $ map strength bridges
      print $ strength $ head sorted
