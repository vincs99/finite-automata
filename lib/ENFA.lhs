\section{Implementation of $\epsilon$-NFA-s}\label{sec:ENFA}

Here we modify our implementation of NFA-s to account for epsilon transitions. This requires 
some technical apparatus. First we give the type structure and 
define some helper functions to calculate $\epsilon$-closure of states. For this we use the blog answer
\cite{CS:30073}.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module ENFA where

import Data.List
import DFA
import NFA
import Test.QuickCheck
data ENFA a = ENFA { statesENF :: [a]
                , alphabetENF:: [Symbol]
                , deltaENF :: Symbol -> a -> [a]
                , epTrans:: [(a, a)]
                , startENF:: a
                , acceptstateENF:: [a]}

instance Show a => Show (ENFA a) where
    show (ENFA sts alph del eps strt acc) 
      = "ENFA" ++ "("++ show sts ++ "," ++ show alph ++ "," ++ show1 del ++ "," ++ show eps ++ "," ++ show strt ++ "," ++ show acc ++")" where
      show1 f = show ["d" ++ "(" ++ show sym ++ "," ++ show st ++ ")" ++ " = " ++ show (f sym st) | sym <- alph, st <- sts ]
-- Taken from https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
trClose :: Eq a => [(a, a)] -> [(a, a)]
trClose closure 
  | closure == closureUntilNow = closure
  | otherwise                  = trClose closureUntilNow
  where closureUntilNow = 
          nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, b == b']


refClose:: Eq a => [a] -> [(a,a)] -> [(a,a)]
refClose as ps = nub (ps ++ [(x,x) | x <- as])

rtClose:: (Eq a, Ord a) => ENFA a -> [a] -> [a]
rtClose nf qs = sort (unionL [[p | p <- statesENF nf , (q, p) `elem` trClose rcl ] | q<- qs] ) where
                                            rcl = refClose (statesENF nf) (epTrans nf)
\end{code}

Finally, we modify the the evaluation function. 

\begin{code}
unionL:: Eq a => [[a]] -> [a]
unionL = foldr union []

evaluateENF:: (Eq a, Ord a) => ENFA a -> String -> Bool
evaluateENF nf st = all (`elem` alphabetENF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateENF nf) (stateArrENF' [startENF nf] st)  where
                stateArrENF' qs [] = rtClose nf qs
                stateArrENF' [] _ = []
                stateArrENF' [q] (x:xs)  = rtClose nf (stateArrENF' (unionL (map (deltaENF nf x) (rtClose nf [q]))) xs )
                stateArrENF' (q:qs) (x:xs) = stateArrENF' [q] (x : xs) `union` stateArrENF' qs (x : xs) --recursion on statespace
\end{code}

We make ENFA -s instance of Arbitrary by slightly modifying the relevant code for NFA-s.
\begin{code}


instance Arbitrary (ENFA Int) where
    arbitrary = do
        -- choose a set of up to 10 worlds:
        sts <- (0:) <$> sublistOf [1..5]
        let sym = ['0', '1']
        delt <- randomDeltaNF sym sts sts
        eps <- sublistOf [(x, y) | x <- sts, y <- sts]
        strt <- elements sts
        accraw <- sublistOf sts
        let acc = nub (0:accraw)
        return $ ENFA sts sym delt eps strt acc
\end{code}