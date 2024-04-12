module Print(
        printDFA,
        printGrammar
) where

import Types

import Data.List (intercalate)

findIn1 :: [State] -> String
findIn1 [] = []
findIn1 (x:xs) = if isStart x then if isFinal x then "[[" ++ stateName x ++ "]]" else "[" ++ stateName x ++ "]" else findIn1 xs

printOthers :: [State] -> String
printOthers [] = []
printOthers (x:xs) = if isFinal x then "[[" ++ stateName x ++ "]]" ++ "\n" ++ printOthers xs else "[" ++ stateName x ++ "]" ++ "\n" ++ printOthers xs

printStates :: [State] -> String
printStates l = findIn1 l ++ "\n" ++ printOthers (filter (\y -> (isStart y == False)) l)

helpPrint :: [(State, Symbol, State)] -> String
helpPrint [] = []
helpPrint ((s, Symbol c, e): xs) = "[" ++ stateName s ++ "] " ++ [c] ++ " [" ++ stateName e ++ "]" ++ "\n" ++ helpPrint xs

printTransition :: Transition -> String
printTransition (Transition t) = helpPrint t

printDFA :: DFA -> String
printDFA (DFA states1 alphabet1 transitions1) = getAlph alphabet1 ++ "\n" ++ printStates states1 ++ printTransition transitions1

-- converts start nonterminal to string
getStart :: NonTerminal -> String
getStart (NonTerminal n) = n

--converts alphabet to string
getAlph :: [Symbol] -> String
getAlph [] = []
getAlph ((Symbol x) :xs) = x : getAlph xs

-- converts nonterm to str
nonTerminalToString :: NonTerminal -> String
nonTerminalToString (NonTerminal nt) = nt

-- converts symbols to string
symbolToString :: Symbol -> String
symbolToString (Symbol s) = [s]

--converts production of the grammar to string
productionToString :: Production -> String
productionToString (Production symbols maybeNonTerm) =
    let symbolsString = symbolToString symbols
        nonTermString = maybe "" nonTerminalToString maybeNonTerm -- if no nonterminal then empty space
    in symbolsString ++ nonTermString

-- one transition to string
transitionGrToString :: TransitionGr -> String
transitionGrToString (TransitionGr nt prods) =
    let ntString = nonTerminalToString nt
        prodsString = map productionToString prods
    in ntString ++ " -> " ++ intercalate " | " prodsString -- insert "\" between rules


--converts all transitions in grammar to string
getTransStr :: [TransitionGr] -> String
getTransStr [] = []
getTransStr (x: xs) = transitionGrToString x ++ "\n" ++ getTransStr xs

-- converts all nonterms to string
nonTermStr :: [NonTerminal] -> String
nonTermStr [] = []
nonTermStr ((NonTerminal x):xs) = x ++ "\n" ++ nonTermStr xs 

--converts grammar to string
printGrammar :: RegularGrammar -> String
printGrammar g = "Start Symbol: " ++ getStart (startSymbolGr g) ++ "\n" 
        ++ "Alphabet: " ++ getAlph (alphabetGr g) ++ "\n" 
                ++ "All nonterminals: " ++ "\n" ++ nonTermStr (allNonTerms g) ++ "\n" ++ getTransStr (transitionsGr g)
