import qualified Data.Map as Map
import Debug.Trace

type State        = Map.Map String Integer
type Register     = [Char]
type Op           = [Char]
type Condition    = (Register, Op, Integer)
type Instruction  = (Register, Op, Integer, Condition)

initialState :: State
initialState = Map.empty

generateStates :: State -> [Instruction] -> [State]
generateStates _ [] = []
generateStates state (instr:restInstr) = state : generateStates (execInstr state instr) restInstr

execInstr :: State -> Instruction -> State
execInstr state (target, op, amount, condition) =
  if conditionHolds state condition
  then runOp state target op amount
  else state

conditionHolds :: State -> Condition -> Bool
conditionHolds state (target, op, val) =
  let currVal = Map.findWithDefault 0 target state
      in case op of
        "==" -> currVal == val
        "!=" -> currVal /= val
        ">"  -> currVal >  val
        ">=" -> currVal >= val
        "<"  -> currVal <  val
        "<=" -> currVal <= val

runOp :: State -> Register -> Op -> Integer -> State
runOp state target op amount =
  let currVal = Map.findWithDefault 0 target state
      in case op of
        "inc" -> Map.insert target (currVal + amount) state
        "dec" -> Map.insert target (currVal - amount) state

parseLine :: String -> Instruction
parseLine line =
  let (target:op:incAmount:_:condReg:condOp:condAmount:_)  = words line
      in (target, op, read incAmount :: Integer,
          (condReg, condOp , read condAmount :: Integer))

main = do
  contents <- readFile "input.txt"
  let instrs     = map parseLine $ lines contents
      states     = generateStates initialState instrs
      maxFinal   = maximum $ Map.elems $ last states
      maxOverall = maximum [v | s <- states, v <- Map.elems s]
      in do
        -- Part #1
        print maxFinal

        -- Part #2
        print maxOverall

