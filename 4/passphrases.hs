import qualified Data.Set as Set


isValid :: String -> Bool
isValid line =
  let ws = words line
      s  = Set.fromList ws
      in Set.size s == length ws

isValid2 :: String -> Bool
isValid2 line =
  let ws = map Set.fromList $ words line
      s  = Set.fromList ws
      in Set.size s == length ws

main = do
  content  <- readFile "input.txt"

  let ls = lines content
      in do
            -- Part #1
            putStrLn $ show $ length $ filter isValid ls
            -- Part #2
            putStrLn $ show $ length $ filter isValid2 ls
