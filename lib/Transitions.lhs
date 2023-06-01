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



nfaToDFA:: (Eq a, Ord a) => NFA a -> DFA [a]
nfaToDFA (NFA sts alph del strt ac) = 
  cutDFA $ DFA sortedStates alph del' (sort [strt]) [st | st <- sortedStates,  intersect st ac /= []] where
    del' sy ls = unionL [del sy l | l <- ls] 
    sortedStates = map sort $ subsequences sts
\end{code}

We extend the powerset construction for $\epsilon$-NFA-s.

\begin{code}
enfaToDFA:: (Eq a, Ord a) => ENFA a -> DFA [a]
enfaToDFA (ENFA sts alph del eps strt ac) = let nf = ENFA sts alph del eps strt ac in 
  cutDFA $ DFA sortedStates alph del' (rtClose nf (sort [strt])) [st | st <- sortedStates,  intersect st ac /= []] where
    del' sy ls = let nf = ENFA sts alph del eps strt ac in rtClose nf ( unionL [del sy l | l <- ls] )
    sortedStates = map sort $ subsequences sts
                                                              
\end{code}

Now we implement transition from RegExp to ENFA.
\begin{code}
-- RegExp to ENFA
regExpToENFA :: RegExp -> ENFA Int
regExpToENFA Empty   = ENFA [1] [] (\_ _ -> []) [] 1 []
regExpToENFA Epsilon = ENFA [1] [] (\_ _ -> []) [] 1 [1]
regExpToENFA (R [])  = ENFA [1] [] (\_ _ -> []) [] 1 [1]
regExpToENFA (R xs)  = ENFA [0..length xs] (nub xs) delta2 [] 0 [length xs] where
  delta2 symbol state | state >= length xs || state < 0 = []
                      | xs !! state == symbol = [state + 1]
                      | otherwise = []
regExpToENFA (Union r1 r2) = regExpToENFA r1 `unionENFA` makeDisjoint (regExpToENFA r1) (regExpToENFA r2)
regExpToENFA (Star r) = starENFA (regExpToENFA r)
regExpToENFA (Con r1 r2) = regExpToENFA r1 `concatENFA` makeDisjoint (regExpToENFA r1) (regExpToENFA r2)
regExpToENFA (Plus r) = regExpToENFA r `concatENFA` makeDisjoint (regExpToENFA r) (starENFA (regExpToENFA r))


-- function that takes two ENFAs and outputs a relabeling of the second ENFA such that the states of both become disjoint
makeDisjoint :: ENFA Int -> ENFA Int -> ENFA Int
makeDisjoint n1 n2 = ENFA states' alphabet' delta' epT start' accept where
  add | minimum (statesENF n2) <= 0 = negate (minimum (statesENF n2)) + (maximum (statesENF n1 ++ statesENF n2) + 1)
      | otherwise = maximum (statesENF n1 ++ statesENF n2) + 1
  states' = map (+ add) (statesENF n2)
  alphabet' = alphabetENF n2
  delta' sym state = map (+ add) (deltaENF n2 sym (state - add))
  epT = [(s + add, t + add) | (s,t) <- epTrans n2 ] 
  start' = startENF n2 + add
  accept = map (+ add) (acceptstateENF n2)


unionENFA :: ENFA Int -> ENFA Int -> ENFA Int-- Use only if states are disjoint
unionENFA n1 n2 = ENFA states'' alphabet2 delta'' epT start'' accept where
  states'' = start'' : statesENF n1 ++ statesENF n2
  alphabet2 = alphabetENF n1 `union` alphabetENF n2
  delta'' sym st = deltaENF n1 sym st ++ deltaENF n2 sym st
  epT = epTrans n1 ++ epTrans n2 ++ [(start'', startENF n1), (start'', startENF n2)]
  start'' = maximum (statesENF n2 ++ statesENF n1) + 1 
  accept = acceptstateENF n1 ++ acceptstateENF n2

starENFA :: ENFA Int -> ENFA Int
starENFA n = ENFA (statesENF n) (alphabetENF n) (deltaENF n) ep (startENF n) (acceptstateENF n)  where
  ep = epTrans n ++ [(startENF n, s)| s <- acceptstateENF n] ++ [(s, startENF n) | s <- acceptstateENF n]

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
For that we use the following function:
\begin{code}

makeIntDFA :: (Eq a, Ord a) => DFA a -> DFA Int
makeIntDFA dfa = DFA sts alph delt strt acceptst 
  where sts = [1..length (states dfa)]
        alph = alphabet dfa
        delt sym i  = indX $ delta dfa sym (states dfa !! (i-1))
        strt = indX (start dfa)
        acceptst = [indX s | s <- acceptstate dfa]
        indX s = fromJust (elemIndex s (states dfa)) + 1

-- simplify to make the result a bit more readable 
dfaToRegExp :: (Eq a, Ord a) => DFA a -> RegExp
dfaToRegExp dfa = simplify $ regExpUnion [rijk dfaInt (start dfaInt) f (length $ states dfa) | f <- acceptstate dfaInt ]
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


Brzozowski's algorithm for comparing DFA's

\begin{code}



minimizeDFA :: Ord a => DFA a -> DFA [Int]
minimizeDFA df | all (`elem` acceptstate df) (reachables df)  = DFA [[0]] (alphabet df) (const (const [0])) [0] [[0]] 
               |  otherwise = (reverseDFA . reverseDFA) df 

  
reverseDFA :: Ord a => DFA a -> DFA [Int]
reverseDFA d = enfaToDFA' $ ENFA sts alph delt ep st acc where
    e = singleAccept $ cutDFA d
    sts = statesENF e
    alph = alphabetENF e
    delt sym s = [t | t <- statesENF e, s `elem` deltaENF e sym t] 
    ep = [(s, t) | (t,s) <- epTrans e ]
    st = head $ acceptstateENF e
    acc = [startENF e]
    enfaToDFA' nf@(ENFA sts' alph' del eps strt ac) = 
      cutDFA $ DFA sortedStates alph' del' (rtClose nf (sort [ t | (s, t) <- eps, s == strt])) [st' | st' <- sortedStates,  intersect st' ac /= []] where
      del' sy ls = rtClose nf ( unionL [del sy l | l <- ls] )
      sortedStates = map sort $ subsequences sts'
  
singleAccept :: Ord a => DFA a -> ENFA Int
singleAccept d = ENFA sts alph delt ep st acc where 
    d' = makeIntDFA d 
    newAccept = maximum (states d') + 1
    sts = newAccept : states d'
    alph = alphabet d'
    delt sym t | t == newAccept = []
               | otherwise =  [delta d' sym t]
    ep = [(f, newAccept) | f <- acceptstate d']
    st = start d'
    acc = [newAccept]

-- Checks whether the reachable states of two DFA's are Isomorphic
brzozowski :: (Ord a, Ord b) => DFA a -> DFA b -> Bool
brzozowski d d' = alphabet d == alphabet d' && 
  (null (acceptstate m) && null (acceptstate m')) ||
   isIsomorphTo (start m) (start m') [] where
    m = minimizeDFA d
    m' = minimizeDFA d'
    isIsomorphTo s s' is = 
      (s,s') `elem` is || (s `elem` acceptstate m) == (s' `elem` acceptstate m') &&
      all (\sym -> isIsomorphTo (delta m sym s) (delta m' sym s') ((s,s'):is)) (alphabet m) 
\end{code}