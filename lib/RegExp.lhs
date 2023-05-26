\section{Regular expressions}\label{sec:RegExp}
Here we implement regular expressions and a toolset to generate words that the regexp accepts.

\begin{code}
module RegExp where

import System.Random
import DFA
import NFA()
import ENFA()
import Test.QuickCheck

data RegExp = Epsilon | R [Symbol] | Union RegExp RegExp | Star RegExp | Con RegExp RegExp | Plus RegExp
  deriving (Eq, Show)

ppt:: RegExp -> String
ppt Epsilon = "R e"
ppt (R xs) = "R " ++ xs
ppt (Union r1 r2) = "(" ++ ppt r1 ++ ")" ++ "u" ++ "(" ++ ppt r2 ++ ")"
ppt (Star r) = "(" ++  ppt r ++ ")*"
ppt (Con r1 r2) = "(" ++ ppt r1 ++ ")" ++ "(" ++ ppt r2 ++ ")"
ppt (Plus r) = "(" ++ ppt r ++ ")+"


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



\begin{code}
generateString :: RegExp -> Gen String
generateString Epsilon = return ""
generateString (R xs) = return xs
generateString (Union r1 r2) = oneof [generateString r1, generateString r2]
generateString (Con r1 r2) = do
  w <- generateString r1
  v <- generateString r2
  return (w ++ v)
generateString (Star r) = oneof [generateString Epsilon, generateString (Con r (Star r)), generateString (Con r (Star r))]
generateString (Plus r) = generateString (Con r (Star r))


-- test
test1 :: IO ()
test1 = quickCheck (forAll (generateString (Plus (R "0"))) (\w -> zeroStart `evaluate` w))
\end{code}