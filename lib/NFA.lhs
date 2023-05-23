\section{NFA Implementation}\label{sec:NFA}

This describes our implementation of Non-deterministic Finite State Automata. As in the definition, an NFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. 

\begin{code}
module NFA where

import Data.List
import Data.Maybe()
import DFA
data NFA = NFA { statesNF :: [State]
                , alphabetNF:: [Symbol]
                , deltaNF :: Symbol -> State -> [State]
                , startNF:: State
                , acceptstateNF:: [State]}
\end{code}

We implement a basic evaluation function that upon input from the string, evaluates if the string is in the 
language.

\begin{code}
evaluateNF:: NFA -> String -> Bool
evaluateNF nf st = all (`elem` alphabetNF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateNF nf) (stateArrNF' [startNF nf] st)  where
                stateArrNF':: [State] -> String -> [State]  
                stateArrNF' qs [] = qs
                stateArrNF' [] _ = []
                stateArrNF' [q] (x:xs) | q == -1 = []
                                       | otherwise = stateArrNF' (deltaNF nf x q) xs --recursion on string
                stateArrNF' (q:qs) (x:xs) = stateArrNF' [q] (x : xs) `union` stateArrNF' qs (x : xs) --recursion on statespace
\end{code}

