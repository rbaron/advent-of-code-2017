import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.IntMap as IntMap

lookup'' i m = case IntMap.lookup i m of
                    Just n  -> n
                    Nothing -> 0

findMaxIdx :: IntMap.IntMap Int -> (Int, Int)
findMaxIdx state =
  let lst = IntMap.toAscList state
      max = maximum [ v | (k, v) <- lst]
      res = List.find (\(i, v) -> v == max) lst
      in case res of
        Just (i, v) -> (i, v)
        Nothing     -> (-1, -1)

distributeBlocks :: IntMap.IntMap Int -> Int -> Int -> IntMap.IntMap Int
distributeBlocks state idx n
  | n == 0 = state
  | otherwise =
    let newState = IntMap.insert idx (lookup'' idx state + 1) state
        newIdx   = if idx == IntMap.size state - 1 then 0 else idx + 1
        newN     = n - 1
      in distributeBlocks newState newIdx newN

nextState :: IntMap.IntMap Int -> IntMap.IntMap Int
nextState state =
  let (maxIdx, maxVal) = findMaxIdx state
      emptyState       = IntMap.insert maxIdx 0 state
      nextIdx          = if maxIdx == IntMap.size state - 1 then 0 else maxIdx + 1
      newState         = distributeBlocks emptyState nextIdx maxVal
      in newState

countStepsTillLoop :: Set.Set (IntMap.IntMap Int) -> IntMap.IntMap Int ->  Int -> (Int, IntMap.IntMap Int)
countStepsTillLoop states state n
  | Set.member state states = (n, state)
  | otherwise               =
    let newStates = Set.insert state states
        newState  = nextState state
        newCount  = n + 1
        in countStepsTillLoop newStates newState newCount

countTillRepeat :: IntMap.IntMap Int -> IntMap.IntMap Int ->  Int -> Int
countTillRepeat needle current n =
  let next  = nextState current
      count = n + 1
    in if next == needle
       then count
       else countTillRepeat needle next count

test = [0, 2, 7, 0]

main = do
  content  <- readFile "input.txt"
  let line   = head $ lines content
      -- state  = IntMap.fromList $ zip [0..] test
      state          = IntMap.fromList $ zip [0..] $ map (\n -> read n :: Int) (words line)
      states         = Set.empty
      (n1, seenState) = countStepsTillLoop states state 0
      n2              = countTillRepeat seenState seenState 0
      -- Part #1, Part #2
      in putStrLn $ show $ (n1, n2)
