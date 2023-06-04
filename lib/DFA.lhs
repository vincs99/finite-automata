\section{DFA Implementation}\label{sec:DFA}


This section describes our implementation of DFAs, together with some helpful functions to work with them. 

\subsection{Data type and basics}
As in the formal definition, a DFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. Throughout, we chose to represent sets as lists, as working with lists is generally easier and faster. 

The parameter of the DFA constructor stands for what type the states are. By default, we will use DFAs with integer states, 
but occasionally we will need ones that have lists of integers as their states. 

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module DFA where
import Data.List ( (\\), intersect, nub )
import Test.QuickCheck ( elements, sublistOf, Arbitrary(arbitrary), Gen ) 

type Symbol = Char
data DFA a = DFA { states :: [a] 
                , alphabet:: [Symbol]
                , delta :: Symbol -> a -> a
                , start:: a
                , acceptstate:: [a]}
\end{code}
We implemented an evaluation function that, given a string, determines if the string is in the 
language. For this, we use a helper function \texttt{stateArr} that specifies the current state after reading a list of symbols.

\begin{code}
evaluate:: (Eq a, Ord a) => DFA a -> String -> Bool
evaluate df st = all (`elem` alphabet df) st && -- captures that all element of string in alphabet
                stateArr (start df) st `elem` acceptstate df where 
                    stateArr  q [] = q
                    stateArr q (x:xs) = stateArr (delta df x q) xs
\end{code}

Below is an example DFA on the alphabet $\Sigma = \{0, 1\}$, accepting the language of words starting with a $0$.
\begin{code}
zeroStart:: DFA Int
zeroStart = DFA [0,1,2] ['0', '1'] deltazero 0 [1] where
    deltazero:: Symbol -> Int -> Int
    deltazero char st
     | st == 0 && char == '0' = 1
     | st == 0 && char == '1' = 2
     | st == 1 = 1
     | st == 2 = 2
     | otherwise = -1
\end{code}
We add a basic \texttt{show} instance for DFAs. The transition function $\delta$ is shown as a list of strings 
specifying its value on the state space and the alphabet.
\begin{code}
instance Show a => Show (DFA a) where
    show (DFA sts alph del strt acc) 
      = "DFA" ++ "("++ show sts ++ "," ++ show alph ++ "," ++ show1 del ++ "," ++ show strt ++ "," ++ show acc ++")" where
      show1 f = show ["d" ++ "(" ++ show sym ++ "," ++ show st ++ ")" ++ " = " ++ show (f sym st) | sym <- alph, st <- sts ]
\end{code}
On the DFA zeroStart from above, this yields the following string representation. 
\begin{showCode}
ghci> zeroStart
DFA([0,1,2],"01",["d('0',0) = 1","d('0',1) = 1","d('0',2) = 2","d('1',0) = 2","d('1',1) = 1","d('1',2) = 2"],0,[1])
\end{showCode}


\subsection{Helpful functions}
Below we defined some helpful functions. 

The \texttt{flipDFA} function takes a DFA and outputs that DFA, but with its accepting states now rejecting and vice versa.
The \texttt{cutDFA} function creates a DFA whose state 
space consists of just those states that are reachable from the start state via the $\delta$-transitions. The  \texttt{reachables} 
function finds those reachable states. Using the \texttt{reachables}, we wrote a function
that determines whether a given DFA has an empty language. 
\begin{code}
flipDFA :: (Eq a, Ord a) => DFA a -> DFA a
flipDFA (DFA sts alp del st acc) = DFA sts alp del st (sts \\ acc)

reachables :: Eq a => DFA a -> [a] 
reachables dfa = reachableInSteps [start dfa] where 
  reachableInSteps ts | all (`elem` ts) (concatMap allSuccessors ts) = ts -- if all successors are in the set then
  --no more reachables
                     | otherwise = reachableInSteps (nub $ ts ++ concatMap allSuccessors ts) --else add successors
  allSuccessors st = nub (st : [delta dfa sym st | sym <- alphabet dfa])

cutDFA :: Eq a => DFA a -> DFA a  
cutDFA d = DFA sts alp delt strt acc where
  sts = reachables d
  alp = alphabet d
  strt = start d
  acc = reachables d `intersect` acceptstate d
  delt = delta d 

languageIsEmpty :: Eq a => DFA a -> Bool
languageIsEmpty d = null $ reachables d `intersect` acceptstate d     
\end{code}

\subsection{Arbitrary generation}
For testing purposes, we added the functionality to arbitrarily generate DFAs. For this we make 
DFAs an instance of the \texttt{Arbitrary} class. To generate arbitrary delta functions, we use two helper functions. The first
generates arbitrary functions on the state space, the second adds the \texttt{Symbol} argument to the function. In the 
construction, we make sure that the state space and set of accept states are non-empty, by adding $0$ to both.
For running time purposes, we only generate DFAs that have at most $4$ states, but that can be adjusted as 
described in the code. 
\begin{code}
-- inspired by arbitrary instance in HW2
randomFunFromTo :: (Eq a, Ord a, Arbitrary a) => [a] -> [a] -> Gen (a -> a)
randomFunFromTo [] _ = return (const undefined)
randomFunFromTo (w:ws) ps = do
    f <- randomFunFromTo ws ps
    wResult <- elements ps
    return $ \v -> if v == w then wResult else f v

randomDelta :: (Eq a, Ord a, Arbitrary a) => [Symbol] -> [a] -> [a] -> Gen (Symbol -> a -> a)
randomDelta [] _ _ = return (const undefined)
randomDelta (sym:syms) ds ps = do 
    f <- randomDelta syms ds ps
    wResult <- randomFunFromTo ds ps
    return $ \sy -> if sy == sym then wResult else f sy

instance Arbitrary (DFA Int) where
    arbitrary = do
        sts <- (0 :) <$> sublistOf [1..3] --choose a set up to 4 states. to change this, set 3 to some larger number
        let sym = ['0', '1']
        delt <- randomDelta sym sts sts
        strt <- elements sts
        accraw <-  sublistOf sts
        let acc = nub (0:accraw)
        return $ DFA sts sym delt strt acc    
\end{code}

