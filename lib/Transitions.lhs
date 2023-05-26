\section{Transition functions}\label{sec:Trans}

Here we implement several transition functions between objects.
First the powerset construction between NFA-s and DFA-s.

\begin{code}
module Transitions where
import Data.List ( intersect, subsequences, elemIndex )
import NFA
import DFA
import ENFA
import RegExp
import Data.Maybe (fromJust)





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


The translation between DFAs and regular expressions is based on Sipser \ref{sipser13}.

For translating a DFA to a regular expression, we need another type of automaton, i.e.\ 
the generalized nondeterministic finite automaton, or \emph{GNFA} for short.
This automaton is like a regular epsilon-NFA, but it has regular expressions as labels (over symbols), 
and exactly one accepting state. 

\begin{code}
-- Define GNFA
data GNFA a = GNFA { statesGNF :: [a]                
                , deltaGNF :: RegExp -> a -> a
                , startGNF:: a
                , acceptstateGNF:: a}
\end{code}

The first step is to convert a DFA to a GNFA. We make the GNFA of type Int for convenience.
It will get a fresh starting and accepting state.
The new starting state will get just an epsilon arrow to the state corresponding to the old starting state.
The old accepting states will get epsilon arrows to the new accepting state.
Furthermore, the labels of the GNFA will be the old labels converted to a regular expression. 
\begin{code}

-- ENFA to GNFA
transDFAtoGNFA :: Eq a => DFA a -> GNFA Int
transDFAtoGNFA dfa = GNFA sts delta' newStart newAccept where
  sts = [0..length (states dfa) +1]
  delta' (R [s]) k = fromJust (elemIndex (delta dfa s (states dfa !! k)) (states dfa))
  delta' Epsilon k | k == newStart = fromJust (elemIndex (start dfa) (states dfa))
                 | k `elem` [ fromJust (elemIndex s (states dfa)) | s <- acceptstate dfa ] = newAccept
  delta' _ _ = -1
  newStart = length (states dfa)
  newAccept = length (states dfa) + 1


-- for testing
gnfa :: GNFA Int
gnfa = transDFAtoGNFA zeroStart

\end{code}

The next step is to reduce the obtained GNFA to a GNFA with just a starting state and an accepting state, 
so that the resulting label from the arrow from the starting state to the accepting state is exactly the regular expression we need.
To make the proces easier, we need exactly one arrow between all states (that are not the starting or accepting state).

\begin{code}



\end{code}

