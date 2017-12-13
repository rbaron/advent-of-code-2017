import Data.Bits
import Data.Char
import Debug.Trace
import Text.Printf


twist :: Int -> Int -> [Int] -> [Int]
twist pos len lst
  | pos + len < length lst =
    (take pos lst) ++ (reverse $ take len $ drop pos lst) ++ (drop (pos + len) lst)
  | otherwise =
      let wrappedAmount = (pos + len) `rem` length lst
          unwrappedAmount = length lst - pos
          reversed = reverse $ take len $ drop pos $ cycle lst
          middle   = take (length lst - len) $ drop wrappedAmount lst
        in drop unwrappedAmount reversed ++ middle ++ take unwrappedAmount reversed

hash :: Int -> Int -> [Int] -> [Int] -> ([Int], Int, Int)
hash pos skip lst [] = (lst, pos, skip)
hash pos skip lst (len:restLens) =
  let newPos  = (pos + len + skip) `rem` length lst
      newLen  = pos + skip
      newSkip = skip + 1
    in hash newPos newSkip (twist pos len lst) restLens

encode :: [Int] -> [Char]
encode lst = foldl (\acc el -> acc ++ printf "%02x" el) "" lst

decode :: [Char] -> [Int]
decode = map ord

runRounds :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
runRounds 0 _ _ lst _ = lst
runRounds rounds pos skip lst lens =
  let (lst2, pos2, skip2) = hash pos skip lst lens
    in runRounds (rounds - 1) pos2 skip2 lst2 lens

denseHash :: [Int] -> [Int]
denseHash lst =
  let blocks      = [ take 16 $ drop (i*16) lst | i <- [0..15] ]
      xoredBlocks = map (\b -> foldl (\acc el -> acc `xor` el) 0 b) blocks
    in xoredBlocks

main :: IO ()
main = do
  content <- readFile "input.txt"
  let line         = head $ lines content
      lensPt1      = read $ "[" ++ line ++ "]" :: [Int]
      (lst1, _, _) = hash 0 0 [0..255] lensPt1

      lensPt2      = decode line ++ [17, 31, 73, 47, 23]
      lst2         = runRounds 64 0 0 [0..255] lensPt2

    in do

    -- Part #1
    print $ product $ take 2 $ lst1

    -- Part #2
    print $ encode $ denseHash lst2
