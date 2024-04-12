module Read (
        readAlph,
        getOne,
        readStates,
        findState,
        makeOne,
        getOneTr,
        helpTr,
        readTrans,
        cut,
        readDFA,
        setTr
) where

import Types

-- converts string to alphabet
readAlph :: String -> [Symbol]
readAlph [] = []
readAlph (s:ss) = if s /= '\r' then Symbol s : readAlph ss else []

-- get one state from transition
getOne :: String -> Bool -> State
getOne s f = State {stateName = takeWhile (/= ']') (dropWhile (== '[') s) , isStart = f, isFinal = head (tail s) == '['}

-- get states in transitions
readStates :: [String] -> Bool -> [State]
readStates [] _ = []
readStates (s:ss) f = if length (words s) == 1 then getOne s f : readStates ss False else []

-- find state in dfa
findState :: String -> [State] -> State
findState _ [] = State {stateName = "", isStart = False, isFinal = False}
findState s (x:xs) = if takeWhile (/= ']') (dropWhile (== '[') s) == stateName x  then x else findState s xs

-- make state from string in transition
makeOne :: String -> DFA -> State
makeOne s d = findState s (states d)

-- get one transition from string
getOneTr :: String -> DFA -> (State, Symbol, State)
getOneTr s dfa = (makeOne (head (words s)) dfa, Symbol (head (words s !! 1)), makeOne (head (tail (tail (words s)))) dfa)

-- returns a list of a transitions
helpTr :: [String] -> DFA -> [(State, Symbol, State)]
helpTr [] _ = []
helpTr (s:ss) dfa = getOneTr s dfa : helpTr ss dfa

-- get transitions
readTrans :: [String] -> DFA -> Transition
readTrans l d = Transition (helpTr l d)

-- returns only strings with transitions
cut :: [String] -> [String]
cut s = takeWhile (\b -> length (words b) /= 0) (dropWhile (\a -> length (words a) == 1) s)

-- read DFA
readDFA :: String -> DFA
readDFA s = DFA {
    states = readStates (tail (lines s)) True,
    alphabet = readAlph (head (lines s)),
    transitions = Transition []
}

-- sets the transitions in dfa
setTr :: DFA -> String -> DFA
setTr dfa s = dfa {transitions = readTrans (cut (tail (lines s))) dfa}
