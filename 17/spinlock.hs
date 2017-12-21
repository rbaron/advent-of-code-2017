import Debug.Trace


insert :: [Int] -> Int -> Int -> [Int]
insert lst val 0   = val:lst
insert lst val pos = (head lst) : insert (tail lst) val (pos - 1)

spin :: [Int] -> Int -> Int -> Int -> (Int, [Int])
spin lst val pos step =
  let realPos = ((pos + step) `rem` (length lst) + 1)
    in (realPos, insert lst val realPos)

findIt :: [Int] -> Int -> Int
findIt (x:[])   needle = -1
findIt (x:xs) needle = if x == needle then head xs else findIt xs needle

-- For part #2, we basically only look for moviments that would case the
-- position 1 to change. Note that at iteration i, the list has length i.
trackPos1 :: [Int] -> Int -> Int -> Int -> Int
trackPos1 [] pos step lastInsert = lastInsert
trackPos1 (i:is) pos step lastInsert =
  let realPos = ((pos + step) `rem` i) + 1
    in if realPos == 1
       then filter' is realPos step i
       else filter' is realPos step lastInsert

main = do
  let lst   = [0]
      --step  = 3
      step  = 386

      -- Part #1
      final = foldl (\(pos, lst) val -> spin lst val pos step) (0, lst) [1..2017]

      -- Part #2
      final2 = trackPos1 [1..50000000] 0 step 0

    in do
      print $ findIt (snd final) 2017
      print $ final2
