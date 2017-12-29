import qualified Data.List.Split as Split
import qualified Data.Map as Map

import Text.Regex.Posix

import Debug.Trace

type State = String
type Pos   = Integer
type Tape  = Map.Map Integer Bool

-- If tape[pos] == False then (write, move, newState) else (write, move, newState)
data Rule = Rule State (Bool, Integer, State) (Bool, Integer, State) deriving (Show)

type RuleBook = Map.Map State Rule

condPatt = ".*value is (.): .*value (.)\\. .*to the (left|right)\\. .*state (.)\\."
patt = "In state (.):" ++ condPatt ++ condPatt

parseRule :: [String] -> Rule
parseRule ls =
  let s   = unlines ls
      res = (filter (/= '\n') s) =~ patt :: [[String]]
      (state:_:w1:m1:s1:_:w2:m2:s2:_) = tail $ head res
    in Rule state ((w1 == "1"), (if m1 == "right" then 1 else -1), s1)
                  ((w2 == "1"), (if m2 == "right" then 1 else -1), s2)

parseInput :: String -> (State, Integer, RuleBook)
parseInput content =
  let (l1:l2:rs) = lines content
      ruleChunks = Split.chunksOf 10 rs
      rules      = map parseRule ruleChunks
      ruleBook   = Map.fromList $ map (\(Rule state a b) -> (state, (Rule state a b))) rules
      initialSt  = init $ last $ words l1
      nSteps     = read (words l2 !! 5) :: Integer
    in (initialSt, nSteps, ruleBook)

step :: State -> Pos -> Tape -> RuleBook -> (State, Pos, Tape)
step state pos tape ruleBook =
  let (Rule s (w1, m1, s1) (w2, m2, s2)) = ruleBook Map.! state
    in case Map.findWithDefault False pos tape of
       False -> (s1, pos + m1, Map.insert pos w1 tape)
       True  -> (s2, pos + m2, Map.insert pos w2 tape)

run :: State -> Pos -> Integer -> RuleBook -> Tape -> Tape
run _ _ 0 _ tape = tape
run state pos steps ruleBook tape =
  -- Making it eager
  let !res = step state pos tape ruleBook
      (newState, newPos, newTape) = res
    in run newState newPos (steps - 1) ruleBook newTape

checkSum :: Tape -> Integer
checkSum tape = sum $ map (\v -> if v then 1 else 0) $ Map.elems tape

main = do
  content <- readFile "input.txt"
  let (initialState, nSteps, ruleBook) = parseInput content
      initialPos                       = 0
      tape                             = Map.empty
      finalTape                        = run initialState initialPos nSteps ruleBook tape
    in do
      print $ checkSum finalTape
