module Types (
        Symbol(..),
        Transition(..),
        State(..),
        DFA(..),
        NonTerminal(..),
        Production(..),
        TransitionGr(..),
        RegularGrammar(..)
) where
        
-- Alphabet symbol
newtype Symbol = Symbol Char deriving (Show, Eq, Ord)

-- DFA transition
newtype Transition = Transition [(State, Symbol, State)] deriving (Show, Eq)

-- DFA state
data State = State {
    stateName :: String,
    isStart :: Bool,
    isFinal :: Bool
} deriving (Show, Eq, Ord)

-- DFA
data DFA = DFA {
    states :: [State],
    alphabet :: [Symbol],
    transitions :: Transition
} deriving (Show)

-- NonTErminals for grammar
newtype NonTerminal = NonTerminal String deriving (Show, Eq)

-- Product of the transition
data Production = Production {
    productSymbols :: Symbol,
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
    allNonTerms :: [NonTerminal],
    alphabetGr :: [Symbol],
    transitionsGr :: [TransitionGr]      -- list of transitions
} deriving (Show)
