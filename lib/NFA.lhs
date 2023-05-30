\section{NFA Implementation}\label{sec:NFA}

This describes our implementation of Nondeterministic Finite State Automata. As in the definition, an NFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. 

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module NFA where

import Data.List
import DFA
import Test.QuickCheck
data NFA a= NFA { statesNF :: [a]
                , alphabetNF:: [Symbol]
                , deltaNF :: Symbol -> a -> [a]
                , startNF:: a
                , acceptstateNF:: [a]}

instance Show a => Show (NFA a) where
    show (NFA sts alph del strt acc) 
      = "NFA" ++ "("++ show sts ++ "," ++ show alph ++ "," ++ show1 del ++ "," ++ show strt ++ "," ++ show acc ++")" where
      show1 f = show ["d" ++ "(" ++ show sym ++ "," ++ show st ++ ")" ++ " = " ++ show (f sym st) | sym <- alph, st <- sts ]
\end{code}

We also implement a similar, but more complicated transition function suitable for NFA-s.

\begin{code}
evaluateNF:: Eq a => NFA a-> String -> Bool
evaluateNF nf st = all (`elem` alphabetNF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateNF nf) (stateArrNF' [startNF nf] st)  where
                stateArrNF'  qs [] = qs
                stateArrNF'  [] _ = []
                stateArrNF'  [q] (x:xs) = stateArrNF' (deltaNF nf x q) xs --recursion on string
                stateArrNF'  (q:qs) (x:xs) = stateArrNF' [q] (x : xs) `union` stateArrNF' qs (x : xs) --recursion on statespace
\end{code}

We make NFA -s instance of Arbitrary by slightly modifying the relevant code for DFA-s.
\begin{code}
-- recursively make a valuation function for these worlds:
randomFunFromTolist :: (Eq a, Arbitrary a) => [a] -> [a] -> Gen (a -> [a])
randomFunFromTolist [] _ = return (const undefined)
randomFunFromTolist (w:ws) ps = do
    f <- randomFunFromTolist ws ps
    wResult <- sublistOf ps
    return $ \v -> if v == w then wResult else f v

randomDeltaNF :: (Eq a, Arbitrary a) => [Symbol] -> [a] -> [a] -> Gen (Symbol -> a -> [a])
randomDeltaNF [] _ _ = return (const undefined)
randomDeltaNF (sym:syms) ds ps = do 
    f <- randomDeltaNF syms ds ps
    wResult <- randomFunFromTolist ds ps
    return $ \sy -> if sy == sym then wResult else f sy

instance Arbitrary (NFA Int) where
    arbitrary = do
        -- choose a set of up to 10 worlds:
        sts <- (0:) <$> sublistOf [1..5]
        let sym = ['0', '1']
        delt <- randomDeltaNF sym sts sts
        strt <- elements sts
        accraw <- sublistOf sts
        let acc = nub (0:accraw)
        return $ NFA sts sym delt strt acc
\end{code}


