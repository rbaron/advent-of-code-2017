import Data.Char
import System.IO


getSum :: Char -> [Char] -> Int
getSum _ [] = 0
getSum lastChar (c:cs) = if lastChar == c
  then digitToInt lastChar + getSum c cs
  else getSum c cs

main = do
  handle   <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let stripped = filter (/= '\n') contents in
    putStrLn $ show $ getSum (last $ stripped) stripped
  hClose handle
