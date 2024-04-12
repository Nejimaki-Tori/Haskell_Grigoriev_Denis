module TransformToGrammar(
        makeGrammar,
        getInitial,
        getFinals
) where

import Types

import Data.List (nub)

-- returns start state
findIn :: [State] -> State
findIn [] = State {stateName = "", isStart = False, isFinal = False}
findIn (x:xs) = if isStart x then x else findIn xs

-- get initial state
getInitial :: DFA -> State
getInitial d = findIn (states d)

-- find all final states
helpF :: [State] -> [State]
helpF [] = []
helpF (x:xs) = if isFinal x then x : helpF xs else helpF xs

-- Get final states
getFinals :: DFA -> [State]
getFinals dfa = helpF (states dfa)

-- dfa transition to grammar transition
helpGr :: [(State, Symbol, State)] -> [(NonTerminal, [Production])]
helpGr transitionAuto =
    let nonTerminals = nub [stateName startState | (startState, _, _) <- transitionAuto]
        productions = map (\nonTerm -> (NonTerminal nonTerm,
            [ Production { productSymbols = symbol, nonTerminal = Just (NonTerminal (stateName endState)) } |
                 (startState, symbol, endState) <- transitionAuto, stateName startState == nonTerm])) nonTerminals
    in productions

-- checks if transiton with the state already exists
check1 :: State -> [(NonTerminal, [Production])] -> Maybe NonTerminal
check1 _ [] = Nothing
check1 s ((x,_):xs) = if (NonTerminal (stateName s)) == x then Just x else check1 s xs

-- adds new transition to already existing one
trAdd :: State -> [(NonTerminal, [Production])] -> Maybe NonTerminal -> [(NonTerminal, [Production])] -> [(NonTerminal, [Production])]
trAdd _ ((_, _):_) Nothing _ = []
trAdd _ [] _ acc = acc
trAdd s ((nonT, l):xs) (Just n) acc = if n == nonT then (nonT, (Production {nonTerminal = Nothing, productSymbols = Symbol '_'} : l)):acc ++ xs else trAdd s xs (Just n) ((nonT, l):acc)

-- adds final rules for final states
addF :: [State] -> [(NonTerminal, [Production])] -> [(NonTerminal, [Production])]
addF [] l = l
addF (x:xs) l = do
                let ch = check1 x l
                if ch /= Nothing then addF xs (trAdd x l ch []) else addF xs ((NonTerminal (stateName x),[Production{nonTerminal = Nothing, productSymbols = Symbol '_'}]) : l)


-- converts dfa transitions to grammar transitions
transitionsToGrammar :: Transition -> [(NonTerminal, [Production])]
transitionsToGrammar (Transition transitionGr) = helpGr transitionGr

-- adds final rules for final states (state -> empty)
addFinal :: DFA -> [(NonTerminal, [Production])] -> [(NonTerminal, [Production])]
addFinal d = addF (getFinals d)

-- transforms different type to actual grammar rules
transGr :: [(NonTerminal, [Production])] -> [TransitionGr]
transGr = map
      (\ x
         -> TransitionGr
              {transitionSymbol = fst x, transitionProductions = snd x})

-- converts states to nonterminals
getAllNonTerms :: [State] -> [NonTerminal]
getAllNonTerms = map (NonTerminal . stateName) 

-- DFA to Regular right grammar
makeGrammar :: DFA -> RegularGrammar
makeGrammar d = RegularGrammar {
    startSymbolGr = NonTerminal (stateName (getInitial d)),
    allNonTerms = getAllNonTerms (states d),
    alphabetGr = alphabet d,
    transitionsGr = transGr (addFinal d (transitionsToGrammar (transitions d)))
}
