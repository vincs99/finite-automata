
\section{Simple Tests}
\label{sec:simpletests}

We create some tests, using QuickCheck to test whether some automata are equal, whether some regular expression 
generates exactly the language of some automaton, and we will add tests that show that our definitions work.


\begin{code}
module Main where
import Test.QuickCheck
import RegExp
import DFA
import ENFA
import NFA
import Transitions()

main :: IO()
main = do
    test1DF
    test2DF
    test1NF
    test2NF
    test1ENF
    test2ENF
\end{code}
The first test whether the example DFA, called zeroStart indeed accepts the strings 
starging with $0$. Note the regular expression for this language is $0(0+1)*$. 
We do the same for slightly modified NFA and ENFA versions of zeroStart. 

\begin{code}
zeroStartNF:: NFA Int
zeroStartNF = NFA [0,1,2, 3] ['0', '1'] deltazeroNF 0 [1] where
    deltazeroNF:: Symbol-> Int -> [Int]
    deltazeroNF char st
     | st == 0 && char == '0' = [1]
     | st == 0 && char == '1' = [2, 3]
     | st == 1 = [1]
     | st == 2 = [2, 3]
     | st == 3 = [3]
     | otherwise = [-1]

zeroStartENF:: ENFA Int
zeroStartENF = ENFA [0,1,2, 3] ['0', '1'] deltazeroNF [(3,2), (2,3)] 0 [1] where
    deltazeroNF:: Symbol-> Int -> [Int]
    deltazeroNF char st
     | st == 0 && char == '0' = [1]
     | st == 0 && char == '1' = [2, 3]
     | st == 1 = [1]
     | st == 2 = [2, 3]
     | st == 3 = [3]
     | otherwise = [-1]
-- test
test1DF :: IO ()
test1DF = quickCheck (forAll (generateString (Con (R "0") (Star (Union (R "0") (R "1"))))) (\w -> zeroStart `evaluate` w))

test1NF :: IO ()
test1NF = quickCheck (forAll (generateString (Con (R "0") (Star (Union (R "0") (R "1"))))) (\w -> zeroStartNF `evaluateNF` w ))

test1ENF :: IO ()
test1ENF = quickCheck (forAll (generateString (Con (R "0") (Star (Union (R "0") (R "1"))))) (\w -> zeroStartENF `evaluateENF` w ))

\end{code}

We now test that words generated from the RegExp for the complement are not accepted. 
Note the RegExp for the complement language is $\epsilon + 1(0+1)*$

\begin{code}
test2DF :: IO ()
test2DF = quickCheck (forAll (generateString (Union Epsilon (Con (R "1") (Star (Union (R "0") (R "1")))))) (not . evaluate zeroStart))

test2NF :: IO ()
test2NF = quickCheck (forAll (generateString (Union Epsilon (Con (R "1") (Star (Union (R "0") (R "1")))))) (not . evaluateNF zeroStartNF))

test2ENF :: IO ()
test2ENF = quickCheck (forAll (generateString (Union Epsilon (Con (R "1") (Star (Union (R "0") (R "1")))))) (not . evaluateENF zeroStartENF))
\end{code}


%To also find out which part of your program is actually used for these tests,
%run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
%report for ... is available at ... .html'' and open this file in your browser.
%See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
