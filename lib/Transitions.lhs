\section{Transition functions}\label{sec:Trans}

Here we implement several transition functions between objects.
Powerset construction: 

\begin{code}
module Transitions where
import Data.List ( intersect, subsequences )
import NFA
import DFA
import ENFA



transNtoD:: NFA -> DFA
transNtoD (NFA sts alph del strt ac) = 
  DFA (subsequences sts) alph del' [strt] [st | st <- subsequences sts,  intersect st ac /= []] where
    del':: Symbol -> [State] -> [State]
    del' sy ls = unionL [del sy l | l <- ls] 
\end{code}

Powerset construction: 

\begin{code}
transENtoD:: ENFA -> DFA
transENtoD (ENFA sts alph del eps strt ac) = let nf = ENFA sts alph del eps strt ac in 
  DFA (subsequences sts) alph del' (rtClose nf strt) [st | st <- subsequences sts,  intersect st ac /= []] where
    del':: Symbol -> [State] -> [State]
    del' sy ls = let nf = ENFA sts alph del eps strt ac in  unionL (map (rtClose nf) lis) where
                                                              lis = unionL [del sy l | l <- ls] 
\end{code}