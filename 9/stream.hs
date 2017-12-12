

data State = InGarbage | OutGarbage | IgnoreNext

getScore :: (Int, Int, Int) -> State -> [Char] -> (Int, Int)
getScore (score, nOpenGroups, garbageCount) _ [] = (score, garbageCount)

getScore (score, nOpenGroups, garbageCount) OutGarbage (x:xs)
  | x == '{'  = getScore (score, nOpenGroups + 1, garbageCount) OutGarbage xs
  | x == '}'  = getScore (score + nOpenGroups, nOpenGroups - 1, garbageCount) OutGarbage xs
  | x == '<'  = getScore (score, nOpenGroups, garbageCount) InGarbage xs
  | otherwise = getScore (score, nOpenGroups, garbageCount) OutGarbage xs

getScore (score, nOpenGroups, garbageCount) InGarbage (x:xs)
  | x == '>'  = getScore (score, nOpenGroups, garbageCount) OutGarbage xs
  | x == '!'  = getScore (score, nOpenGroups, garbageCount) IgnoreNext xs
  | otherwise = getScore (score, nOpenGroups, garbageCount + 1) InGarbage xs

getScore (score, nOpenGroups, garbageCount) IgnoreNext (x:xs) =
  getScore (score, nOpenGroups, garbageCount) InGarbage xs

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let stream                = head $ lines contents
      (score, garbageCount) = getScore (0, 0, 0) OutGarbage stream
          -- Part #1
    in do print score
          -- Part #2
          print garbageCount
