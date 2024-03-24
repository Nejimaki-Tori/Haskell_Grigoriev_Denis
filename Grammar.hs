-- Alphabet symbol
newtype Symbol = Symbol Char deriving (Show)

-- DFA transition
newtype Transition = Transition [(State, Symbol, State)] deriving (Show)

-- DFA state
data State = State {
    stateID :: Int,
    isStart :: Bool,
    isFinal :: Bool
} deriving (Show)

-- DFA
data DFA = DFA {
    states :: [State],               
    alphabet :: [Symbol],            
    transitions :: Transition 
} deriving (Show)

-- NonTErminals for grammar
newtype NonTerminal = NonTerminal Char deriving (Show)

-- Product of the transition
data Production = Production {
    productSymbols :: [Symbol],
    nonTerminal :: Maybe NonTerminal
} deriving (Show)

-- Transition
data TransitionGr = TransitionGr {
    transitionSymbol :: NonTerminal,    -- Non Terminal (left side)
    transitionProductions :: [Production]  -- Products (right side)
} deriving (Show)

-- Regular grammar
data RegularGrammar = RegularGrammar {
    startSymbolGr :: NonTerminal,     -- Starting symbol
    alphabetGr :: [Symbol],
    transitionsGr :: [TransitionGr]      -- list of transitions
} deriving (Show)

-- read DFA
readDFA :: String -> DFA
readDFA _ = DFA {
    states = [],
    alphabet = [Symbol 'a'],
    transitions = Transition []
}

-- Check state in DFA
hasState :: DFA -> Bool
hasState _ = False

-- get initial state
getInitial :: DFA -> State
getInitial _ = State {
    stateID = 0,
    isStart = True,
    isFinal = False
}

-- Get final states
getFinals :: DFA -> [State]
getFinals _ = []

-- Get all states
getStates :: DFA -> [State]
getStates _ = []

-- Checks if DFA has transition
hasTrans :: DFA -> Bool
hasTrans _ = True

-- Get exact transition
getTrans :: DFA -> (State, Symbol, State)
getTrans _ = (State {stateID = 0, isStart = True, isFinal = False}, 
              Symbol 'a', 
              State {stateID = 0, isStart = True, isFinal = False})

--Get NonTerm in product in grammar
getNonTerm :: TransitionGr -> NonTerminal
getNonTerm _ = NonTerminal 'S'

-- Printing
printDFA :: DFA -> String
printDFA _ = ""

printGrammar :: RegularGrammar -> String
printGrammar _ = ""

main :: IO ()
main = do
    print "a" -- for checking the compilation
