import Data.Char
import System.IO


checksumLine :: String -> Int
checksumLine line =
  let ws = words line
      ns = map read ws :: [Int]
      max  = maximum ns
      min  = minimum ns
  in max - min

checksumLine2 :: String -> Int
checksumLine2 line =
  let ws     = words line
      ns     = map read ws :: [Int]
      combs  = [ (i, j) | i <- ns, j <- ns, i /= j && i `rem` j == 0]
      (i, j) = head combs
      in i `div` j

main = do
  content  <- readFile "input.txt"
  let ls = lines content
      in do print $ sum $ map checksumLine ls
            print $ sum $ map checksumLine2 ls
