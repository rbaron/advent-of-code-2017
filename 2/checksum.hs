import Data.Char
import System.IO


checksumLine :: String -> Int
checksumLine line =
  let ws = words line
      ns = map read ws :: [Int]
      max  = maximum ns
      min  = minimum ns
  in max - min

checksum :: [String] -> Int
checksum ls = sum $ map checksumLine ls

main = do
  content  <- readFile "input.txt"
  let ls = lines content
      in putStrLn $ show $ checksum ls
