\section{Implementation of $\epsilon$-NFA-s}\label{sec:ENFA}

Here we modify our implementation of NFA-s to account for epsilon transitions. This requires 
some technical apparatus. First we give the type structure and 
define some helper functions to calculate $\epsilon$-closure of states. For this we use the blog answer
\cite{CS:30073}.

\begin{code}
module ENFA where

import Data.List
import Data.Maybe()
import DFA
data ENFA a = ENFA { statesENF :: [a]
                , alphabetENF:: [Symbol]
                , deltaENF :: Symbol -> a -> [a]
                , epTrans:: [(a, a)]
                , startENF:: a
                , acceptstateENF:: [a]}

-- Taken from https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
trClose :: Eq a => [(a, a)] -> [(a, a)]
trClose closure 
  | closure == closureUntilNow = closure
  | otherwise                  = trClose closureUntilNow
  where closureUntilNow = 
          nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, b == b']


refClose:: Eq a => [a] -> [(a,a)] -> [(a,a)]
refClose as ps = nub (ps ++ [(x,x) | x <- as])

rtClose:: Eq a => ENFA a -> a -> [a]
rtClose nf q = [p | p <- statesENF nf , (q, p) `elem` trClose rcl ] where
                                            rcl = refClose (statesENF nf) (epTrans nf)
\end{code}

Finally, we modify the the evaluation function. 

\begin{code}
unionL:: Eq a => [[a]] -> [a]
unionL = foldr union []

evaluateENF:: Eq a => ENFA a -> String -> Bool
evaluateENF nf st = all (`elem` alphabetENF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateENF nf) (stateArrENF' [startENF nf] st)  where
                stateArrENF' qs [] = qs
                stateArrENF' [] _ = []
                stateArrENF' [q] (x:xs)  = stateArrENF' (unionL (map (deltaENF nf x) (rtClose nf q))) xs
                stateArrENF' (q:qs) (x:xs) = stateArrENF' [q] (x : xs) `union` stateArrENF' qs (x : xs) --recursion on statespace
\end{code}

