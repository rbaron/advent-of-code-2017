import qualified Data.Map as Map


parseLine :: [Char] -> ([Char], [[Char]])
parseLine l =
  let ws = words l
      in case ws of
        n:_:_:children  -> (n, map maybeStripTrailingComma children)
        n:_             -> (n, [])

maybeStripTrailingComma :: [Char] -> [Char]
maybeStripTrailingComma str =
  if last str == ','
  then init str
  else str

buildParentByNode :: [([Char], [[Char]])] -> Map.Map [Char] [Char] -> Map.Map [Char] [Char]
buildParentByNode [] parentByNode = parentByNode
buildParentByNode ((parent, children):nodes) parentByNode =
    let newParentByNode = foldr (\el acc -> Map.insert el parent acc) parentByNode children
      in buildParentByNode nodes newParentByNode

traverseUp :: Map.Map [Char] [Char] -> [Char] -> [Char]
traverseUp parentByNode node =
  if Map.notMember node parentByNode
  then node
  else case Map.lookup node parentByNode of
    Just parent -> traverseUp parentByNode parent
    Nothing     -> []

main = do
  contents <- readFile "input.txt"
  let ls             = lines contents
      nodes          = map parseLine $ ls
      parentByNode   = buildParentByNode nodes Map.empty
      someRandomNode = head $ Map.keys parentByNode
      in do print $ traverseUp parentByNode someRandomNode
