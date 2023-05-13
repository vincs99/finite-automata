
\section{DFA Implementation}\label{sec:DFA}

This describes our implementation of Deterministic Finite State Automata. As in the definition, a DFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. 

\begin{code}
module DFA where

import Data.List()
import Data.Maybe()
type State = Int
type Symbol = Char
data DFA = DFA { states :: [State]
                , alpabet:: [Symbol]
                , delta :: State -> Symbol -> State
                ,  start:: State
                , acceptstate:: [State]}
\end{code}

We implement a basic evaluation function that upon input from the string, evaluates if the string is in the 
language.

\begin{code}
evaluate:: DFA -> String -> Bool
evaluate df st = and (map ((flip $ elem) (alpabet df)) st) && (stateArr (start df) st) `elem` acceptstate df where
    stateArr:: State -> String -> State
    stateArr q [] = q
    stateArr q (x:xs) = stateArr (delta df q x) xs 
\end{code}

We write a toy example DFA on the alphabet $\Sigma = \{0, 1\}$ computing the language of words starting with a $0$.

\begin{code}
zeroStart:: DFA
zeroStart = DFA [0,1,2] ['0', '1'] deltazero 0 [1] where
    deltazero:: State -> Symbol -> State
    deltazero st char
     | st == 0 && char == '0' = 1
     | st == 0 && char == '1' = 2
     | st == 1 = 1
     | st == 2 = 2
     | otherwise = -1
\end{code}

This outputs: 
\begin{showCode}
ghci> evaluate zeroStart "010001"
True

ghci> evaluate zeroStart "1000110"
False

ghci> evaluate zeroStart []
False

ghci> evaluate zeroStart "0lakddkdsljkdjak"
False
\end{showCode}

Current implementation outputs 'false' for words containing characters outside the alphabet. 