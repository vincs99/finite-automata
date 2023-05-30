
\section{DFA Implementation}\label{sec:DFA}

This describes our implementation of Deterministic Finite State Automata. As in the definition, a DFA consists
of a tuple $(Q, \Sigma, \delta, q_{start}, A)$ representing the set of states, the alphabet, the transition 
function, the start state and the accept states. 

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module DFA where

import Data.List
import Test.QuickCheck

type Symbol = Char
data DFA a = DFA { states :: [a] 
                , alphabet:: [Symbol]
                , delta :: Symbol -> a -> a
                ,  start:: a
                , acceptstate:: [a]}
instance Show a => Show (DFA a) where
    show (DFA sts alph del strt acc) 
      = "DFA" ++ "("++ show sts ++ "," ++ show alph ++ "," ++ show1 del ++ "," ++ show strt ++ "," ++ show acc ++")" where
      show1 f = show ["d" ++ "(" ++ show sym ++ "," ++ show st ++ ")" ++ " = " ++ show (f sym st) | sym <- alph, st <- sts ]


\end{code}

We implement a basic evaluation function that upon input from the string, evaluates if the string is in the 
language.

\begin{code}
evaluate:: (Eq a, Ord a) => DFA a -> String -> Bool
evaluate df st = all (`elem` alphabet df) st && -- captures that all element of string in alphabet
                stateArr (start df) st `elem` acceptstate df where 
                    stateArr  q [] = q
                    stateArr q (x:xs) = stateArr (delta df x q) xs
\end{code}

We write a toy example DFA on the alphabet $\Sigma = \{0, 1\}$ computing the language of words starting with a $0$.

\begin{code}
zeroStart:: DFA Int
zeroStart = DFA [0,1] ['0', '1'] deltazero 0 [1] where
    deltazero:: Symbol -> Int -> Int
    deltazero char st
     | st == 0 && char == '0' = 1
     | st == 0 && char == '1' = 0
     | st == 1 = 1
     | otherwise = -1
\end{code}

Some basic transform of a DFA. 
\begin{code}
flipDFA :: (Eq a, Ord a) => DFA a -> DFA a
flipDFA (DFA sts alp del st acc) = DFA sts alp del st (sts \\ acc)
\end{code}

We make DFA-s instance of Arbitrary as follows. We use solution to Homework 2.
\begin{code}
-- recursively make a valuation function for these worlds:
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
        -- choose a set of up to 10 worlds:
        sts <- (0:) <$> sublistOf [1]
        let sym = ['0', '1']
        delt <- randomDelta sym sts sts
        strt <- elements sts
        accraw <-  sublistOf sts
        let acc = nub (0:accraw)
        return $ DFA sts sym delt strt acc
\end{code}