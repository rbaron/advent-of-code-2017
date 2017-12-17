import Data.Bits

-- Generators for part #1
gen :: Integer -> Integer -> [Integer]
gen init factor = genInner(init)
  where genInner val = let nn = (val * factor) `rem` 2147483647
                        in nn : (genInner $! nn)

matchLast16bits :: (Integer, Integer) -> Integer
matchLast16bits (a, b) =
  if a .&. 0xffff == b .&. 0xffff
  then 1
  else 0

-- A strict, tail recursive sum implementation, otherwise the stack overflows
sum' :: [Integer] -> Integer -> Integer
sum' [] acc = acc
sum' (x:xs) acc = sum' xs $! (acc+x)

main = do
  let genA = gen 516 16807
      genB = gen 190 48271
      gen2A = filter (\n -> n `rem` 4 == 0) genA
      gen2B = filter (\n -> n `rem` 8 == 0) genB
    in do
      -- Part #1
      print $ sum' (take 40000000 $ map matchLast16bits $ zip genA genB) 0
      -- Part 2
      print $ sum' (take 5000000 $ map matchLast16bits $ zip gen2A gen2B) 0
