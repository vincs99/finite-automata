\section{NFA Implementation}\label{sec:NFA}

This section describes our implementation of NFAs. In the initial design, the only (natural) difference to DFA-s is that 
the transition function maps to a list. The show instance of NFA-s is analogous to that of DFA-s.
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module NFA where
import Data.List ( nub, union )
import DFA ( Symbol )
import Test.QuickCheck
    ( elements, sublistOf, Arbitrary(arbitrary), Gen )

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

We also implement a similar, but more complicated evaluation function suitable for NFA-s. We have to keep track of 
all possible path we can be in via a helper function stateArrNF'. We define the unionL helper function that takes
the set union of elements of a list of lists. We make use of this functions on several other occasion throughout 
the code. 

\begin{code}
unionL:: Eq a => [[a]] -> [a]
unionL = foldr union []

evaluateNF:: Eq a => NFA a-> String -> Bool
evaluateNF nf st = all (`elem` alphabetNF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateNF nf) (stateArrNF' [startNF nf] st)  where
                stateArrNF'  qs [] = qs -- type signature: [a]->Symbol-> [a]
                stateArrNF'  [] _ = []
                stateArrNF'  qs (x:xs) = unionL [stateArrNF' (deltaNF nf x q) xs | q <- qs] --recursion on string               
\end{code}

We make NFA -s instance of Arbitrary by slightly modifying the relevant code for DFA-s. The difference is that the
arbitrary functions we generate take lists as values.
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
        sts <- (0 :) <$> sublistOf [1..5]
        let sym = ['0', '1']
        delt <- randomDeltaNF sym sts sts
        strt <- elements sts
        accraw <- sublistOf sts
        let acc = nub (0:accraw)
        return $ NFA sts sym delt strt acc
\end{code}


