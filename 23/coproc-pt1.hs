import qualified Data.Map  as Map
import qualified Text.Read as Text

import Debug.Trace

type Op       = String
type Register = String
data Source = SourceRegister Register | SourceImmediate Integer deriving (Show)
data Instr  = Instr Op Source Source deriving (Show)
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
      (o:r:s:[]) -> Instr o (parseSource r) (parseSource s)

lookupVal :: Source -> State -> Integer
lookupVal (SourceImmediate v) _ = v
lookupVal (SourceRegister r)  state = Map.findWithDefault 0 r state

lookupRegister :: Source -> Register
lookupRegister (SourceImmediate v) = error "Invalid"
lookupRegister (SourceRegister r)  = r

runInstr :: State -> Instrs -> Integer -> Integer -> (State, Integer, Integer)
runInstr state instrs instructionPointer mulCount =
  case instrs Map.! instructionPointer of
    Instr op dest source -> case op of
      "set" -> (Map.insert (lookupRegister dest) (lookupVal source state) state,
                instructionPointer + 1,
                mulCount)
      "sub" -> let oldVal = lookupVal dest state
                   inc    = lookupVal source state
                in (Map.insert (lookupRegister dest) (oldVal - inc) state,
                    instructionPointer + 1,
                    mulCount)
      "mul" -> let oldVal = lookupVal dest state
                   factor = lookupVal source state
                in (Map.insert (lookupRegister dest) (oldVal * factor) state,
                    instructionPointer + 1,
                    mulCount + 1)
      "jnz" -> let x = lookupVal dest state
                   y = lookupVal source state
                 in if x /= 0
                    then (state, instructionPointer + y, mulCount)
                    else (state, instructionPointer + 1, mulCount)

runInstrs :: State -> Instrs -> Integer -> Integer -> (State, Integer)
runInstrs state instrs instructionPointer mulCount =
  if (instructionPointer < 0) || (instructionPointer >= (fromIntegral $ Map.size instrs))
  then (state, mulCount)
  else let (newState, newIP, newMulCount) = runInstr state instrs instructionPointer mulCount
         in runInstrs newState instrs newIP newMulCount

main = do
  input <- readFile "input.txt"
  let ls                     = lines input
      instrLst               = map parseInstr ls
      instrs                 = Map.fromList $ zip [0..] instrLst
      state                  = Map.empty
      (finalState, mulCount) = runInstrs state instrs 0 0
    in do
      print $ mulCount
