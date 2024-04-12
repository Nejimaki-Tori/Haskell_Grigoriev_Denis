module NfaToDfa (
        buildDFA
) where

import Types

import TransformToGrammar(getInitial)

import qualified Data.Set as Set

-- checks if a transition exists
hasTrans :: Transition -> State -> State -> Symbol -> Bool
hasTrans (Transition []) _ _ _ = False
hasTrans (Transition ((s1, a, s2):xs)) st1 st2 symb = (s1 == st1) && (s2 == st2) && (a == symb) || hasTrans (Transition xs) st1 st2 symb

-- checks if half of a transition exists
hasTrans1 :: Transition -> State -> Symbol -> Bool
hasTrans1 (Transition []) _ _ = False
hasTrans1 (Transition ((s, a, _):xs)) st symb = (s == st) && (a == symb) || hasTrans1 (Transition xs) st symb

-- calculating all states where we can go from our set
move :: DFA -> Set.Set State -> Symbol -> Set.Set State
move dfa currStates symbol = Set.unions 
        [Set.fromList [endState | endState <- states dfa, hasTrans (transitions dfa) state endState symbol] | 
                state <- Set.toList currStates, hasTrans1 (transitions dfa) state symbol]

-- creating transition from one set to another
createTransition :: Set.Set State -> Set.Set State -> Symbol -> (State, Symbol, State)
createTransition p q c = (State {stateName = concateStates p, isStart = False, isFinal = conF p}, c, State {stateName = concateStates q, isStart = False, isFinal = conF q})
        where
                concateStates :: Set.Set State -> String
                concateStates = foldr (\state acc -> acc ++ (stateName state)) ""
                conF :: Set.Set State -> Bool
                conF = foldr (\state acc -> acc || (isFinal state)) False

-- concatenate all states names into one string
concatenateStates :: Set.Set State -> State
concatenateStates stateSet = State {stateName = foldr (\state acc -> acc ++ (stateName state)) "" (Set.toList stateSet), isStart = False, isFinal = makeF stateSet} 
        where
                makeF :: Set.Set State -> Bool
                makeF = foldr (\state acc -> acc || (isFinal state)) False

check :: Set.Set State -> Bool
check l = length (Set.toList l) == 1 && (isStart (head (Set.toList l)))

-- make a normal state out of set of states
makeState :: [Set.Set State] -> [State]
makeState [] = []
makeState (x:xs) = if check x then makeState xs else concatenateStates x : makeState xs

-- for gettings elements from tuple
fHelp :: (a, b, c) -> a
fHelp (a,_,_) = a

sHelp :: (a, b, c) -> b
sHelp (_,b,_) = b

tHelp :: (a, b, c) -> c
tHelp (_,_,c) = c

-- one iteration for all alphabet symbols
allIter :: DFA -> [Symbol] -> Set.Set State -> Set.Set (Set.Set State) -> Set.Set (Set.Set State) -> Set.Set (State, Symbol, State) 
        -> (Set.Set (State, Symbol, State), Set.Set (Set.Set State), Set.Set (Set.Set State))
allIter nfa alph pd pB qB tr
                | alph == [] = (tr, pB, qB)  
                | otherwise = do
                        let c = head alph
                        let qd = move nfa pd c
                        let newTr = createTransition pd qd c
                        let newTransitions = if (stateName (tHelp newTr)) /= "" then Set.singleton newTr else Set.empty
                        let queue'' = if Set.member qd qB then pB else Set.insert qd pB 
                        let newQSet = if Set.member qd qB then qB else Set.insert qd qB
                        allIter nfa (tail alph) pd queue'' newQSet (Set.union tr newTransitions)
                        
-- NFA to DFA
buildDFA :: DFA -> DFA
buildDFA nfa = transform (Set.singleton (Set.singleton (getInitial nfa))) Set.empty Set.empty
        where
                transform :: Set.Set (Set.Set State) -> Set.Set (State, Symbol, State) -> Set.Set (Set.Set State) -> DFA
                transform queue transitionsNew qSet
                        | Set.null queue = DFA ((getInitial nfa) : (makeState (Set.toList qSet))) (alphabet nfa) (Transition (Set.toList transitionsNew)) -- return DFA
                        | otherwise = do
                                let currStates = Set.elemAt 0 queue -- first elem from queue
                                let queue' = Set.deleteAt 0 queue -- delete from queue
                                let oneIter = allIter nfa (alphabet nfa) currStates queue' qSet transitionsNew
                                let fr = fHelp oneIter
                                let sn = sHelp oneIter
                                let th = tHelp oneIter
                                transform sn (Set.union transitionsNew fr) th
