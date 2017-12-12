import Data.Char
import System.IO


getSum :: Int -> [Int] -> Int
getSum _ [] = 0
getSum lastChar (c:cs) = if lastChar == c
  -- then digitToInt lastChar + getSum c cs
  then lastChar + getSum c cs
  else getSum c cs

getSum2 :: [Int] -> Int
getSum2 lst =
  let shifted = drop (length lst `div` 2) $ cycle lst
      in sum $ map (\(x, y) -> if x == y then x else 0) $ zip lst shifted

main = do
  handle   <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let stripped = map digitToInt $ filter (/= '\n') contents in do
    putStrLn $ show $ getSum (last $ stripped) stripped
    print $ getSum2 stripped
  hClose handle
