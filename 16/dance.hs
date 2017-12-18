import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import Data.Function
import Data.List
import Debug.Trace

type State = Map.Map Char Int
type Move  = (State -> State)

findKeyWithValue :: (Eq b) => Map.Map a b -> b -> a
findKeyWithValue m v =
  let (k, _) = head $ filter (\(k, v') -> v' == v) $ Map.toList m
    in k

spin :: Int -> State -> State
spin amount state =
  let size = Map.size state
    in Map.map (\pos -> (pos + amount) `rem` size) state

exchange :: Int -> Int -> State -> State
exchange posA posB state =
  let a = findKeyWithValue state posA
      b = findKeyWithValue state posB
    in Map.insert a posB $ Map.insert b posA state

partner :: Char -> Char -> State -> State
partner a b state =
  let posA = state Map.! a
      posB = state Map.! b
    in Map.insert a posB $ Map.insert b posA state

parseLine :: String -> (State -> State)
parseLine l
  | l !! 0 == 's' = spin $ (read (tail l) :: Int)
  | l !! 0 == 'x' =
    let (a, b) = break (== '/') $ tail l
      in exchange (read a :: Int) (read (tail b) :: Int)
  | l !! 0 == 'p' = partner (l !! 1) (l !! 3)

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

applyMoves :: [Move] -> State -> State
applyMoves moves state = foldl' (\acc m -> m acc) state moves

countTillRepetition :: [Move] -> State -> Int
countTillRepetition moves originalState = counterInner originalState 0
  where counterInner state n = let nextState = applyMoves moves state
                                 in if nextState == originalState
                                      then (n + 1)
                                      else counterInner nextState (n + 1)

pprint :: State -> String
pprint state =
  let sorted = sortBy (compare `on` snd) $ Map.toList state
    in map fst sorted

main = do
  input <- readFile "input.txt"
  let state = Map.fromList $ zip ['a'..'p'] [0..]
      moves = map parseLine $ wordsWhen (== ',') $ head $ lines input
      final = applyMoves moves state

      -- How many times do we need to apply all the moves before getting
      -- back to the original state
      n     = countTillRepetition moves state
      times = 1000000000 `rem` n
    in do
      -- Part #1
      print $ pprint final

      -- Part #2
      print $ pprint $ foldl (\s _ -> applyMoves moves s) state [1..times]
