import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace


readWeight :: [Char] -> Int
readWeight str = read (tail $ init str)

parseLine :: [Char] -> ([Char], Int, [[Char]])
parseLine l =
  let ws = words l
      in case ws of
        n:w:_:children  -> (n, readWeight w, map maybeStripTrailingComma children)
        n:w:_           -> (n, readWeight w, [])

maybeStripTrailingComma :: [Char] -> [Char]
maybeStripTrailingComma str =
  if last str == ','
  then init str
  else str

buildParentByNode :: [([Char], Int, [[Char]])] -> Map.Map [Char] [Char] -> Map.Map [Char] [Char]
buildParentByNode [] parentByNode = parentByNode
buildParentByNode ((parent, weight, children):nodes) parentByNode =
    let newParentByNode = foldr (\el acc -> Map.insert el parent acc) parentByNode children
      in buildParentByNode nodes newParentByNode

buildChildrenByNode :: [([Char], Int, [[Char]])] -> Map.Map [Char] [[Char]] -> Map.Map [Char] [[Char]]
buildChildrenByNode [] childrenByNode = childrenByNode
buildChildrenByNode ((node, _, children):nodes) childrenByNode =
      buildChildrenByNode nodes (Map.insert node children childrenByNode)

buildWeightByNode :: [([Char], Int, [[Char]])] -> Map.Map [Char] Int -> Map.Map [Char] Int
buildWeightByNode [] weightByNode = weightByNode
buildWeightByNode ((node, weight, children):nodes) weightByNode =
      buildWeightByNode nodes (Map.insert node weight weightByNode)

traverseUp :: Map.Map [Char] [Char] -> [Char] -> [Char]
traverseUp parentByNode node =
  if Map.notMember node parentByNode
  then node
  else case Map.lookup node parentByNode of
    Just parent -> traverseUp parentByNode parent
    Nothing     -> []

fullWeight :: [Char] -> Map.Map [Char] Int -> Map.Map [Char] [[Char]] -> Int
fullWeight node weightByNode childrenByNode =
  let originWeight    = Map.findWithDefault 0 node weightByNode
      children        = Map.findWithDefault [] node childrenByNode
      -- !b = trace(show children) 1
      childrenWeights = fmap (\c -> fullWeight c weightByNode childrenByNode) children
      in originWeight + sum childrenWeights

findUnbalanced :: [Char] -> Int -> Map.Map [Char] Int -> Map.Map [Char] [[Char]] -> Maybe Int
findUnbalanced origin increment weightByNode childrenByNode = do
  originWeight <- Map.lookup origin weightByNode
  children     <- Map.lookup origin childrenByNode
  let childrenWeights = map (\c -> fullWeight c weightByNode childrenByNode) children
      in if Set.size (Set.fromList childrenWeights) == 1
      then Just $ originWeight + increment
      else let (idx, newIncrement) = findWeightIncrement childrenWeights
               -- !b = trace( "weight inc" ++ show (idx, newIncrement ) ) 1
               newOrigin           = children !! idx
               in findUnbalanced newOrigin newIncrement weightByNode childrenByNode


findWeightIncrement :: [Int] -> (Int, Int)
findWeightIncrement weights =
  let counter = foldr (\el acc -> Map.insert el (Map.findWithDefault 0 el acc + 1) acc) Map.empty weights
      -- !c = trace (show counter) 1
      (wrongW, wrongCount)     = head $ filter (\(w, c) -> c == 1) $ Map.toList counter
      (correctW, correctCount) = head $ filter (\(w, c) -> c > 1)  $ Map.toList counter
      (wrongIdx, _)            = head $ filter (\(i, w) -> w == wrongW) $ zip [0..] weights
      in (wrongIdx, correctW - wrongW)

main = do
  contents <- readFile "input.txt"
  let ls             = lines contents
      nodes          = map parseLine $ ls
      childrenByNode = buildChildrenByNode nodes Map.empty
      parentByNode   = buildParentByNode nodes Map.empty
      weightByNode   = buildWeightByNode nodes Map.empty
      someRandomNode = head $ Map.keys parentByNode
      topNode        = traverseUp parentByNode someRandomNode
            -- Part #1
      in do print topNode
            -- Part #2
            print $ findUnbalanced topNode 0 weightByNode childrenByNode
