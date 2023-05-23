\section{Implementation of $\epsilon$-NFA-s}\label{sec:ENFA}

Here we modify our implementation of NFA-s to account for epsilon transitions. This requires 
some technical apparatus. First we give the type structure and 
define some helper functions to calculate $\epsilon$-closure of states.

\begin{code}
module ENFA where

import Data.List
import Data.Maybe()
import DFA
data ENFA = ENFA { statesENF :: [State]
                , alphabetENF:: [Symbol]
                , deltaENF :: Symbol -> State -> [State]
                , epTrans:: [(State, State)]
                , startENF:: State
                , acceptstateENF:: [State]}

-- Taken from https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
trClose :: Eq a => [(a, a)] -> [(a, a)]
trClose closure 
  | closure == closureUntilNow = closure
  | otherwise                  = trClose closureUntilNow
  where closureUntilNow = 
          nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, b == b']


refClose:: Eq a => [a] -> [(a,a)] -> [(a,a)]
refClose as ps = nub (ps ++ [(x,x) | x <- as])

rtClose:: ENFA -> State -> [State]
rtClose nf q = [p | p <- statesENF nf , (q, p) `elem` trClose rcl ] where
                                            rcl = refClose (statesENF nf) (epTrans nf)
\end{code}

We implement a basic evaluation function that upon input from the string, evaluates if the string is in the 
language.

\begin{code}
unionL:: Eq a => [[a]] -> [a]
unionL = foldr union []

evaluateENF:: ENFA -> String -> Bool
evaluateENF nf st = all (`elem` alphabetENF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateENF nf) (stateArrENF' [startENF nf] st)  where
                stateArrENF':: [State] -> String -> [State]  
                stateArrENF' qs [] = qs
                stateArrENF' [] _ = []
                stateArrENF' [q] (x:xs) | q == -1 = []
                                        | otherwise = stateArrENF' (unionL (map (deltaENF nf x) (rtClose nf q))) xs
                stateArrENF' (q:qs) (x:xs) = stateArrENF' [q] (x : xs) `union` stateArrENF' qs (x : xs) --recursion on statespace
\end{code}

