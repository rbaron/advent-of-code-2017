import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Bits
import Data.Char
import Debug.Trace
import Text.Printf


--  From 10/know.hs

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

-- Solution

runHash :: String -> String
runHash str =
  let ints = runRounds 64 0 0 [0..255] ((map ord str) ++ [17, 31, 73, 47, 23])
    in encode $ denseHash ints

hashToBits :: String -> Integer -> Integer
hashToBits [] res = res
hashToBits (c1:cs) res =
  let b1 = fromIntegral (digitToInt c1)
    in hashToBits cs ((res `shiftL` 4) .|. b1)

getPos :: Integer -> Int -> Int
getPos bitmap pos = if bitmap .&. (0x1 `shiftL` (127 - pos)) /= 0 then 1 else 0

sumUsed :: Integer -> Int
sumUsed bitmap = sum $ map (getPos bitmap) [0..127]

showBeginning :: Integer -> String
showBeginning bitmap =
  let poss  =  map (\p -> getPos bitmap p) $ [0..7]
      chars = map (\c -> if c == 1 then '#' else '.') poss
    in chars

type Coord     = (Int, Int)
type UnionFind = Map.Map Coord Coord

getOldNeighbors :: Coord -> [Coord]
getOldNeighbors (x, y) = filter (\(x, y) -> x >=0 && y >=0) [(x-1, y), (x, y-1)]

posLine :: Integer -> [Int]
posLine bitmap = map (getPos bitmap) [0..127]

linePosMap :: Int -> Integer -> Map.Map Coord Int
linePosMap y bitmap =
  let posXY = zip (zip [0..] (cycle [y])) (posLine bitmap)
    in Map.fromList posXY

linesPosMap :: [Integer] -> Map.Map Coord Int
linesPosMap bitmaps =
  foldl (\acc (y, s) -> Map.union acc (linePosMap y s)) Map.empty (zip [0..] bitmaps)

findLeader' :: Map.Map Coord Coord -> Coord -> Coord
findLeader' leaders coord =
  let parent = leaders Map.! coord
    in if parent == coord
       then coord
       else findLeader' leaders parent

getGroups :: [Coord] -> Map.Map Coord Int -> Map.Map Coord Coord -> Map.Map Coord Coord
getGroups [] pos leaders = leaders
getGroups (coord:cs) pos leaders =
  if pos Map.! coord == 0
  then getGroups cs pos leaders
  else let occupiedNeighbors = filter (\c -> pos Map.! c == 1) $ getOldNeighbors coord
           ls                = map (findLeader' leaders) occupiedNeighbors
           newMap =  foldl (\acc l -> Map.insert l coord acc) (Map.insert coord coord leaders) ls
         in getGroups cs pos newMap

countGroups :: Map.Map Coord Coord -> Int
countGroups leaders =
  let xys = Map.keys leaders
    in Set.size $ Set.fromList $ map (findLeader' leaders) xys

--input = "flqrgnkx"
input = "ljoxqyyw"

main :: IO ()
main = do
  let baseHash = runHash input
      inputs   = map (\line -> input ++ "-" ++ show line) [0..127]
      bitmaps  = map (\i -> hashToBits (runHash i) 0) inputs
      res = map (\bm -> showBeginning bm) bitmaps


      posMaps = linesPosMap bitmaps
      xys     = [(x, y) | x <- [0..127], y <-[0..127]]
      groups  = getGroups xys posMaps Map.empty
      count   = countGroups groups
    in do
      -- Part #1
      print $ sum $ map sumUsed bitmaps

      -- Part #2
      print $ count
