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


AFL-notes version:

First we rename the states so that they are now integers $1\dots n$ where $n$ is the amount of states.
The starting state will be 1. For that we use the following function:
\begin{code}
-- tests to add: new start state is indeed 1. They recognize the same language 
makeIntDFA :: Eq a => DFA a -> DFA Int
makeIntDFA dfa = DFA sts alph delt strt acceptst 
  where sts = [1..length (states dfa)]
        alph = alphabet dfa
        delt sym i  = indX $ delta dfa sym (states dfa !! (i-1))
        strt = indX (start dfa)
        acceptst = [indX s | s <- acceptstate dfa]
        indX s = fromJust (elemIndex s (states dfa)) + 1



-- simplify to make the result a bit more readable 
transDFAtoRegExp :: Eq a => DFA a -> RegExp
transDFAtoRegExp dfa = simplify $ regExpUnion [rijk dfaInt (start dfaInt) f (length $ states dfa) | f <- acceptstate dfaInt ]
  where dfaInt = makeIntDFA dfa

-- Here is the magic from the notes:
rijk :: DFA Int -> Int -> Int -> Int -> RegExp
rijk dfa i j 0 | i == j  = regExpUnion $ Epsilon : labels
               | null labels = Empty
               | length labels == 1 = head labels
               | otherwise = foldr Union (head labels) (tail labels)          
  where labels =  [R [x] | x <- alphabet dfa, delta dfa x i == j]
rijk dfa i j k = Union (rijk dfa i j (k-1)) (Con (rijk dfa i k (k-1)) (Con (Star $ rijk dfa k k (k-1)) (rijk dfa k j (k-1))))

\end{code}

