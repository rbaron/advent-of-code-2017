import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Sequence

lookupSure i m = case IntMap.lookup i m of
                    Just n  -> n
                    Nothing -> 0

execJmp :: ((IntMap.IntMap Int, Int) -> Int) -> (IntMap.IntMap Int, Int) -> (IntMap.IntMap Int, Int)
execJmp increment (jmps, idx) =
  let newIdx  = idx + lookupSure idx jmps
      newJmps = IntMap.insert idx (increment (jmps, idx)) jmps
      in (newJmps, newIdx)

countJmps :: ((IntMap.IntMap Int, Int) -> Int) -> (IntMap.IntMap Int, Int) -> Int -> Int
countJmps increment (jmps, idx) n
  | IntMap.notMember idx jmps = n
  | otherwise = let (newJmps, newIdx) = execJmp increment (jmps, idx)
                    -- Tip: using $! actually makes this tail recursive optimized. Otherwise we'll
                    -- carry the trunk of (n + 1) around
                    in countJmps increment (newJmps, newIdx) $! (n + 1)

incrementPart1 :: (IntMap.IntMap Int, Int) -> Int
incrementPart1 (jmps, idx) = (lookupSure idx jmps) + 1

incrementPart2 :: (IntMap.IntMap Int, Int) -> Int
incrementPart2 (jmps, idx) =
  let val = lookupSure idx jmps
      in if val >= 3
         then val - 1
         else val + 1

--test = [0, 3,  0,  1,  -3]

main = do
  content  <- readFile "input.txt"
  --let jmps = zip [0..] test
  let jmps = map (\(i, n) -> (i :: Int, read n :: Int)) $ zip [0..] (lines content)
      m    = IntMap.fromList jmps
      in do
        -- Part #1
        --putStrLn $ show $ m
        putStrLn $ show $ countJmps incrementPart1 (m, 0) 0

        -- Part #2
        putStrLn $ show $ countJmps incrementPart2 (m, 0) 0
