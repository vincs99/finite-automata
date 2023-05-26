\section{Transition functions}\label{sec:Trans}

Here we implement several transition functions between objects.
First the powerset construction between NFA-s and DFA-s.

\begin{code}
module Transitions where
import Data.List ( intersect, subsequences )
import NFA
import DFA
import ENFA



transNtoD:: Eq a => NFA a -> DFA [a]
transNtoD (NFA sts alph del strt ac) = 
  DFA (subsequences sts) alph del' [strt] [st | st <- subsequences sts,  intersect st ac /= []] where
    del' sy ls = unionL [del sy l | l <- ls] 
\end{code}

We extend the powerset construction for $\epsilon$-NFA-s.

\begin{code}
transENtoD:: Eq a => ENFA a -> DFA [a]
transENtoD (ENFA sts alph del eps strt ac) = let nf = ENFA sts alph del eps strt ac in 
  DFA (subsequences sts) alph del' (rtClose nf strt) [st | st <- subsequences sts,  intersect st ac /= []] where
    del' sy ls = let nf = ENFA sts alph del eps strt ac in  unionL (map (rtClose nf) lis) where
                                                              lis = unionL [del sy l | l <- ls] 
\end{code}