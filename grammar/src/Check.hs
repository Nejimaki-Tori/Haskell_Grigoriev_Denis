module Check(
        checkAll,
        checkStr
) where

import Types

import TransformToGrammar()

import Read(cut)

checkTrans :: [String] -> Bool
checkTrans [] = True
checkTrans (x:xs) = if length (words x) /= 3 then False else checkTrans xs

--checks if there are any final states
checkFinal :: [State] -> Bool
checkFinal = foldr (\x acc -> isFinal x || acc) False

-- checks if there is one and only one initial state
checkInitial :: [State] -> Bool -> Bool
checkInitial [] f = f
checkInitial (x:xs) f = if (isStart x) && (not f) then checkInitial xs True else 
        if (isStart x) && f then False else checkInitial xs f

-- checks if all transitions contain symbol from the alphabet 
checkAlph1 :: [Symbol] -> [(State, Symbol, State)] -> Bool
checkAlph1 _ [] = True
checkAlph1 l ((_, s, _):xs) = if s `elem` l then checkAlph1 l xs else False

checkAlph :: [Symbol] -> Transition -> Bool
checkAlph l (Transition ls) = checkAlph1 l ls

-- all of the checks
checkAll :: DFA -> String
checkAll nfa = do       
        let symbCh = not (checkAlph (alphabet nfa) (transitions nfa))
        let finCh = not (checkFinal (states nfa))
        let inCh = not (checkInitial (states nfa) False)
        if symbCh then "Wrong symbol in the transition!" else
                if finCh then "No final states!" else
                        if inCh then "No initial states!" else ""   
    
-- checks the transitions                    
checkStr :: String -> Bool
checkStr s = not (checkTrans ((cut (tail (lines s)))))  
        
        
        
        
