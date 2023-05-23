
\section{DFA Implementation}\label{sec:DFA}

This describes our implementation of Deterministic Finite State Automata. As in the definition, a DFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. 

\begin{code}
module DFA where

type State = Int
type Symbol = Char
data DFA = DFA { states :: [[State]] -- For ease with subset construct, represent by set of states.
                , alphabet:: [Symbol]
                , delta :: Symbol -> [State] -> [State]
                ,  start:: [State]
                , acceptstate:: [[State]]}
\end{code}

We implement a basic evaluation function that upon input from the string, evaluates if the string is in the 
language.

\begin{code}
evaluate:: DFA -> String -> Bool
evaluate df st = all (`elem` alphabet df) st && -- captures that all element of string in alphabet
                stateArr (start df) st `elem` acceptstate df where 
                    stateArr:: [State] -> String -> [State] -- recursively go to state at end of string
                    stateArr q [] = q
                    stateArr q (x:xs) = stateArr (delta df x q) xs
\end{code}

We write a toy example DFA on the alphabet $\Sigma = \{0, 1\}$ computing the language of words starting with a $0$.

\begin{code}
zeroStart:: DFA
zeroStart = DFA [[0],[1],[2]] ['0', '1'] deltazero [0] [[1]] where
    deltazero:: Symbol -> [State] -> [State]
    deltazero char st
     | st == [0] && char == '0' = [1]
     | st == [0] && char == '1' = [2]
     | st == [1] = [1]
     | st == [2] = [2]
     | otherwise = [-1]
\end{code}

