import qualified Data.Map  as Map
import qualified Text.Read as Text

import Debug.Trace

type Op       = String
type Register = String
data Source = SourceRegister Register | SourceImmediate Integer deriving (Show)
data Instr  = Instr2 Op Source | Instr3 Op Source Source deriving (Show)
type Instrs = Map.Map Integer Instr
type State  = Map.Map Register Integer

parseSource :: String -> Source
parseSource s =
  case Text.readMaybe s :: Maybe Integer of
    Just i  -> SourceImmediate i
    Nothing -> SourceRegister s

parseInstr :: String -> Instr
parseInstr l =
  let ws = words l
    in case ws of
      (o:r:[])   -> Instr2 o (parseSource r)
      (o:r:s:[]) -> Instr3 o (parseSource r) (parseSource s)

lookupVal :: Source -> State -> Integer
lookupVal (SourceImmediate v) _ = v
lookupVal (SourceRegister r)  state = Map.findWithDefault 0 r state

lookupRegister :: Source -> Register
lookupRegister (SourceImmediate v) = error "Invalid"
lookupRegister (SourceRegister r)  = r

runInstr :: State -> Instrs -> Integer -> Integer -> (State, Integer, Integer, Bool)
runInstr state instrs instructionPointer lastPlayedFreq =
  case instrs Map.! instructionPointer of
    Instr2 op source -> case op of
      "snd" -> (state, instructionPointer + 1, lookupVal source state, False)
      "rcv" -> let x = lookupVal source state
                   r = lookupRegister source
                in if x > 0
                   then (Map.insert r lastPlayedFreq state, instructionPointer + 1, lastPlayedFreq, True)
                   else (state, instructionPointer + 1, lastPlayedFreq, False)
    Instr3 op dest source -> case op of
      "set" -> (Map.insert (lookupRegister dest) (lookupVal source state) state,
                instructionPointer + 1,
                lastPlayedFreq,
                False)
      "add" -> let oldVal = lookupVal dest state
                   inc    = lookupVal source state
                in (Map.insert (lookupRegister dest) (oldVal + inc) state,
                    instructionPointer + 1,
                    lastPlayedFreq,
                    False)
      "mul" -> let oldVal = lookupVal dest state
                   factor = lookupVal source state
                in (Map.insert (lookupRegister dest) (oldVal * factor) state,
                    instructionPointer + 1,
                    lastPlayedFreq,
                    False)
      "mod" -> let oldVal = lookupVal dest state
                   factor = lookupVal source state
                in (Map.insert (lookupRegister dest) (oldVal `rem` factor) state,
                    instructionPointer + 1,
                    lastPlayedFreq,
                    False)
      "jgz" -> let x = lookupVal dest state
                   y = lookupVal source state
                in if x > 0
                   then (state, instructionPointer + y, lastPlayedFreq, False)
                   else (state, instructionPointer + 1, lastPlayedFreq, False)

runInstrs :: State -> Instrs -> Integer -> Integer -> Integer
runInstrs state instrs instructionPointer lastPlayedFreq =
  let (newState, newIP, newLastPlayedFreq, halt) = runInstr state instrs instructionPointer lastPlayedFreq
    in if halt
         then lastPlayedFreq
         else runInstrs newState instrs newIP newLastPlayedFreq

-- Part #2

type Queue = [Integer]

pop :: Queue -> Integer
pop = last

rest :: Queue -> Queue
rest = init

push :: Integer -> Queue -> Queue
push a q = a:q

isStuck :: Instrs -> Integer -> Bool
isStuck instrs ip =
  case instrs Map.! ip of
    Instr2 "rcv" _ -> True
    otherwise      -> False

runInstrs2 :: (State, State) -> (Queue, Queue) -> (Integer, Integer) -> Integer -> Instrs-> Integer -> Integer
runInstrs2 (stateA, stateB) (qA, qB) (ipA, ipB) p instrs counter =
  if ipA < 0 || ipA > (fromIntegral $ Map.size instrs) || ipB < 0 || ipB > (fromIntegral $ Map.size instrs)
  then counter
  else let ip = if p == 0 then ipA else ipB
           -- !b = trace("State B: " ++ show stateB ++ " P " ++ show p ++ " qB" ++ show qB ++ show (instrs Map.! ipA)) 1
           -- !b = trace("p: " ++ show p ++ " instr " ++ show (instrs Map.! ipA)) 1
           -- !b = trace("p: " ++ show p ) 1
        in case instrs Map.! ip of
          Instr2 op source -> case op of
            "snd" -> if p == 0
                     then runInstrs2
                        (stateA, stateB)
                        (qA, push (lookupVal source stateA) qB)
                        (ipA + 1, ipB)
                        p
                        instrs
                        counter
                     else runInstrs2
                        (stateA, stateB)
                        (push (lookupVal source stateB) qA, qB)
                        (ipA, ipB + 1)
                        p
                        instrs
                        counter + 1

            "rcv" -> if p == 0
                     then if null qA
                          then if null qB && (isStuck instrs ipB)
                               then counter
                               else runInstrs2
                                  (stateA, stateB)
                                  (qA, qB)
                                  (ipA, ipB)
                                  1
                                  instrs
                                  counter
                          else runInstrs2
                             (Map.insert (lookupRegister source) (pop qA) stateA, stateB)
                             ((rest qA), qB)
                             (ipA + 1, ipB)
                             p
                             instrs
                             counter
                     else if null qB
                          then if null qA && (isStuck instrs ipA)
                               then counter
                               else runInstrs2
                                  (stateA, stateB)
                                  (qA, qB)
                                  (ipA, ipB)
                                  0
                                  instrs
                                  counter
                          else runInstrs2
                             (stateA, Map.insert (lookupRegister source) (pop qB) stateB)
                             (qA, (rest qB))
                             (ipA, ipB + 1)
                             p
                             instrs
                             counter

            --"rcv" -> let x = lookupVal source state
            --             r = lookupRegister source
            --          in if x > 0
            --             then (Map.insert r lastPlayedFreq state, instructionPointer + 1, lastPlayedFreq, True)
            --             else (state, instructionPointer + 1, lastPlayedFreq, False)
          --otherwise -> counter
          Instr3 op dest source -> case op of
            "set" -> if p == 0
                     then runInstrs2
                            (Map.insert (lookupRegister dest) (lookupVal source stateA) stateA, stateB)
                            (qA, qB)
                            (ipA + 1, ipB)
                            p
                            instrs
                            counter
                     else runInstrs2
                            (stateA, Map.insert (lookupRegister dest) (lookupVal source stateB) stateB)
                            (qA, qB)
                            (ipA, ipB + 1)
                            p
                            instrs
                            counter
            "add" -> if p == 0
                     then runInstrs2
                            (Map.insert (lookupRegister dest) ((lookupVal source stateA) + (lookupVal dest stateA)) stateA, stateB)
                            (qA, qB)
                            (ipA + 1, ipB)
                            p
                            instrs
                            counter
                     else runInstrs2
                            (stateA, Map.insert (lookupRegister dest) ((lookupVal source stateB) + (lookupVal dest stateB)) stateB)
                            (qA, qB)
                            (ipA, ipB + 1)
                            p
                            instrs
                            counter
            "mul" -> if p == 0
                     then runInstrs2
                            (Map.insert (lookupRegister dest) ((lookupVal source stateA) * (lookupVal dest stateA)) stateA, stateB)
                            (qA, qB)
                            (ipA + 1, ipB)
                            p
                            instrs
                            counter
                     else runInstrs2
                            (stateA, Map.insert (lookupRegister dest) ((lookupVal source stateB) * (lookupVal dest stateB)) stateB)
                            (qA, qB)
                            (ipA, ipB + 1)
                            p
                            instrs
                            counter
            "mod" -> if p == 0
                     then runInstrs2
                            (Map.insert (lookupRegister dest) ((lookupVal dest stateA) `rem` (lookupVal source stateA)) stateA, stateB)
                            (qA, qB)
                            (ipA + 1, ipB)
                            p
                            instrs
                            counter
                     else runInstrs2
                            (stateA, Map.insert (lookupRegister dest) ((lookupVal dest stateB) `rem` (lookupVal source stateB)) stateB)
                            (qA, qB)
                            (ipA, ipB + 1)
                            p
                            instrs
                            counter
            "jgz" -> if p == 0
                     then runInstrs2
                            (stateA, stateB)
                            (qA, qB)
                            (ipA + (if (lookupVal dest stateA) > 0 then (lookupVal source stateA) else 1), ipB)
                            p
                            instrs
                            counter
                     else runInstrs2
                            (stateA, stateB)
                            (qA, qB)
                            (ipA, ipB + (if (lookupVal dest stateB) > 0 then (lookupVal source stateB) else 1))
                            p
                            instrs
                            counter
            --otherwise -> counter
--    Instr3 op dest source -> case op of
--      "set" -> (Map.insert (lookupRegister dest) (lookupVal source state) state,
--                instructionPointer + 1,
--                lastPlayedFreq,
--                False)
--      "add" -> let oldVal = lookupVal dest state
--                   inc    = lookupVal source state
--                in (Map.insert (lookupRegister dest) (oldVal + inc) state,
--                    instructionPointer + 1,
--                    lastPlayedFreq,
--                    False)
--      "mul" -> let oldVal = lookupVal dest state
--                   factor = lookupVal source state
--                in (Map.insert (lookupRegister dest) (oldVal * factor) state,
--                    instructionPointer + 1,
--                    lastPlayedFreq,
--                    False)
--      "mod" -> let oldVal = lookupVal dest state
--                   factor = lookupVal source state
--                in (Map.insert (lookupRegister dest) (oldVal `rem` factor) state,
--                    instructionPointer + 1,
--                    lastPlayedFreq,
--                    False)
--      "jgz" -> let x = lookupVal dest state
--                   y = lookupVal source state
--                in if x > 0
--                   then (state, instructionPointer + y, lastPlayedFreq, False)
--                   else (state, instructionPointer + 1, lastPlayedFreq, False)

main = do
  input <- readFile "input.txt"
  --input <- readFile "input-test-2.txt"
  let ls       = lines input
      instrLst = map parseInstr ls
      instrs   = Map.fromList $ zip [0..] instrLst
      state    = Map.empty

      -- Part #2
      res2 = runInstrs2 (Map.insert "p" 0 state, Map.insert "p" 1 state) ([], []) (0, 0) 0 instrs 0
    in do
      --print $ runInstrs state instrs 0 0

      print $ res2
