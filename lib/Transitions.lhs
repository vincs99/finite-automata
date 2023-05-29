\section{Transition functions}\label{sec:Trans}

Here we implement several transition functions between objects.
First the powerset construction between NFA-s and DFA-s.

\begin{code}
module Transitions where
import Data.List 
import NFA
import DFA
import ENFA
import RegExp
import Data.Maybe (fromJust)

nfToDf:: Eq a => NFA a -> DFA [a]
nfToDf (NFA sts alph del strt ac) = 
  DFA (subsequences sts) alph del' [strt] [st | st <- subsequences sts,  intersect st ac /= []] where
    del' sy ls = unionL [del sy l | l <- ls] 
\end{code}

We extend the powerset construction for $\epsilon$-NFA-s.

\begin{code}
enfToDf:: Eq a => ENFA a -> DFA [a]
enfToDf (ENFA sts alph del eps strt ac) = let nf = ENFA sts alph del eps strt ac in 
  DFA (subsequences sts) alph del' (rtClose nf strt) [st | st <- subsequences sts,  intersect st ac /= []] where
    del' sy ls = let nf = ENFA sts alph del eps strt ac in  unionL (map (rtClose nf) lis) where
                                                              lis = unionL [del sy l | l <- ls] 
\end{code}

Now we implement transition from RegExp to ENFA.
\begin{code}
-- RegExp to ENFA
regExpToENFA :: RegExp -> ENFA Int
regExpToENFA Empty   = ENFA [0] [] (\_ _ -> []) [] 0 []
regExpToENFA Epsilon = ENFA [0] [] (\_ _ -> []) [] 0 [0]
regExpToENFA (R xs) = ENFA [0..length xs -1] (nub xs) delta2 [] 0 [length xs -1] where
  delta2 symbol state | xs !! state == symbol = [state + 1]
                     | otherwise = []
regExpToENFA (Union r1 r2) = regExpToENFA r1 `unionENFA` makeDisjoint (regExpToENFA r1) (regExpToENFA r2)
regExpToENFA (Star r) = starENFA (regExpToENFA r)
regExpToENFA (Con r1 r2) = regExpToENFA r1 `concatENFA` makeDisjoint (regExpToENFA r1) (regExpToENFA r2)
regExpToENFA (Plus r) = regExpToENFA r `concatENFA` starENFA (makeDisjoint (regExpToENFA r) (regExpToENFA r))


-- function that takes two ENFAs and outputs a relabeling of the second ENFA such that the states of both become disjoint
makeDisjoint :: ENFA Int -> ENFA Int -> ENFA Int
makeDisjoint n1 n2 = ENFA states' alphabet' delta' epT start' accept where
  add = maximum (statesENF n1)
  states' = map (+ add) (statesENF n2)
  alphabet' = alphabetENF n2
  delta' sym state = deltaENF n2 sym (state + add)
  epT = [(s + add, t + add) | (s,t) <- epTrans n2 ] 
  start' = start' + add
  accept = map (+ add) (acceptstateENF n2)


unionENFA :: ENFA Int -> ENFA Int -> ENFA Int-- Use only if states are disjoint
unionENFA n1 n2 = ENFA states'' alphabet2 delta'' epT start'' accept where
  states'' = -1 : statesENF n1 ++ statesENF n2
  alphabet2 = alphabetENF n1 `union` alphabetENF n2
  delta'' sym st = deltaENF n1 sym st ++ deltaENF n2 sym st
  epT = epTrans n1 ++ epTrans n2 ++ [(-1, startENF n1), (-1, startENF n2)]
  start'' = -1
  accept = acceptstateENF n1 ++ acceptstateENF n2

starENFA :: ENFA Int -> ENFA Int
starENFA n = ENFA (statesENF n) (alphabetENF n) (deltaENF n) ep (startENF n) (acceptstateENF n)  where
  ep = epTrans n ++ [(s, startENF n) | s <- acceptstateENF n]

concatENFA :: ENFA Int -> ENFA Int -> ENFA Int -- Use only if states are disjoint again
concatENFA n1 n2 = ENFA states4 alphabet'' delta3 epT start3 accept where
  states4 = statesENF n1 ++ statesENF n2
  alphabet'' = alphabetENF n1 `union` alphabetENF n2
  delta3 sym st = deltaENF n1 sym st ++ deltaENF n2 sym st
  epT = epTrans n1 ++ epTrans n2 ++ [(s, startENF n2) | s <- acceptstateENF n1]
  start3 = startENF n1
  accept = acceptstateENF n2
\end{code}

Finally, we turn to implement DFA to RegExp. AFL-notes version:

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

