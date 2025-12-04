-- stack script problems/t10.hs --resolver lts-22.18 --package containers --package regex-tdfa --package regex-base --package array

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Regex.Base.RegexLike (AllTextSubmatches (..))
import Text.Regex.TDFA

data Instr
  = Move
      { from :: String,
        lowTo :: String,
        highTo :: String
      }
  | Init {uid :: String, val :: Int}
  deriving (Show)

type PinState = Map String [Int]

main :: IO ()
main = do
  content <- readFile "../data/t10.txt"
  let instrs = map parseInstr $ lines content
      finalState = run instrs
      r = Map.filter (\v -> v == [17, 61]) finalState
      r2 = foldl1 (*) $ concatMap (\k -> finalState Map.! k) ["output0", "output1", "output2"]
  print r
  print r2

parseInstr :: String -> Instr
parseInstr s =
  case getAllTextSubmatches (s =~ pattern :: AllTextSubmatches [] String) of
    [_, v, n, "", "", "", "", ""] -> Init {uid = "bot" ++ n, val = read v}
    [_, "", "", n1, kind, n2, kind2, n3] -> Move {from = "bot" ++ n1, lowTo = kind ++ n2, highTo = kind2 ++ n3}
    otherwise -> error $ "Not parsed: " ++ s
  where
    pattern = "value ([0-9]+) goes to bot ([0-9]+)|bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)"

run :: [Instr] -> PinState
run instrs = applyMoves moves initState
  where
    initState = getInitialState instrs
    moves = Map.fromList [(from i, i) | i@Move {} <- instrs]

applyMoves :: Map String Instr -> PinState -> PinState
applyMoves moves state = go state readyPins
  where
    readyPins = Map.keys $ Map.filter (\vs -> length vs == 2) state
    go state [] = state
    go state activePins =
      let (state', pins) = foldl (applyMove moves) (state, []) activePins
       in go state' pins

applyMove :: Map String Instr -> (PinState, [String]) -> String -> (PinState, [String])
applyMove moves (state, pins) pin = (newState, newPins')
  where
    [low, high] = state Map.! pin
    Move {lowTo, highTo} = moves Map.! pin
    newState = resolveState (resolveState state lowTo low) highTo high
    has2 k = length (newState Map.! k) == 2
    newPins = if has2 lowTo then lowTo : pins else pins
    newPins' = if has2 highTo then highTo : newPins else newPins

getInitialState :: [Instr] -> PinState
getInitialState instrs = foldl applyInit Map.empty inits
  where
    inits = [i | i@Init{}  <- instrs]
    applyInit state init = resolveState state (uid init) (val init)

resolveState :: PinState -> String -> Int -> PinState
resolveState state key value = Map.insertWith upd key [value] state
  where
    upd [v] [v2]
      | v < v2 = [v, v2]
      | otherwise = [v2, v]
