\section{Regular expressions}\label{sec:RegExp}
Here we implement regular expressions and a toolset to generate words that the regexp accepts.

\begin{code}
module RegExp where

import System.Random
import DFA
import NFA()
import ENFA()

data RegExp = Epsilon | R [Symbol] | Union RegExp RegExp | Star RegExp | Con RegExp RegExp | Plus RegExp

instance Show RegExp where
  show Epsilon = "R e"
  show (R xs) = "R " ++ show xs
  show (Union r1 r2) = show r1 ++ " u " ++ show r2
  show (Star r) = "(" ++  show r ++ ")*"
  show (Con r1 r2) = show r1 ++ show r2
  show (Plus r) = "(" ++ show r ++ ")+"


generateWord :: RegExp -> IO String
generateWord Epsilon = pure ""
generateWord (R xs) = pure xs
generateWord (Union r1 r2) = do
  i <- getStdRandom (randomR (0,1::Int))
  [generateWord r1, generateWord r2] !! i
generateWord (Con r1 r2) = do
  one <- generateWord r1
  two <- generateWord r2
  return (one ++ two)
generateWord (Star r) = do
  i <- getStdRandom (randomR (0,1::Int))
  [generateWord Epsilon, generateWord (Con r (Star r))] !! i
generateWord (Plus r) = generateWord (Con r (Star r)) 
\end{code}

A function to use for testing: 
\begin{code}
qc :: Eq a => DFA a -> RegExp -> Int -> IO ()
qc _ _ 0 = print "No counterexample found"
qc dfa ex n =  do
  s <- generateWord ex
  if evaluate dfa s
    then qc dfa ex (n-1)
    else 
      if s == "" 
      then print "The DFA rejected the empty string"
      else print ("the DFA rejected " ++ s) 
\end{code}