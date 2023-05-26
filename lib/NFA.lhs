\section{NFA Implementation}\label{sec:NFA}

This describes our implementation of Nondeterministic Finite State Automata. As in the definition, an NFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. 

\begin{code}
module NFA where

import Data.List
import Data.Maybe()
import DFA
data NFA a= NFA { statesNF :: [a]
                , alphabetNF:: [Symbol]
                , deltaNF :: Symbol -> a -> [a]
                , startNF:: a
                , acceptstateNF:: [a]}
\end{code}

We also implement a similar, but more complicated transition function suitable for NFA-s.

\begin{code}
evaluateNF:: Eq a => NFA a-> String -> Bool
evaluateNF nf st = all (`elem` alphabetNF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateNF nf) (stateArrNF' [startNF nf] st)  where
                stateArrNF'  qs [] = qs
                stateArrNF'  [] _ = []
                stateArrNF'  [q] (x:xs) = stateArrNF' (deltaNF nf x q) xs --recursion on string
                stateArrNF'  (q:qs) (x:xs) = stateArrNF' [q] (x : xs) `union` stateArrNF' qs (x : xs) --recursion on statespace
\end{code}


