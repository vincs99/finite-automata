
\section{Simple Tests}
\label{sec:simpletests}

We now use the library QuickCheck to randomly generate input for our functions
and test some properties.

We will later add stuff to (pseudo)test whether some automata are equal, whether some regular expression generates exactly the language of some automaton, and we will add tests that show that our definitions work.


\begin{code}
module Main where

--import Test.QuickCheck
\end{code}

%The following uses the HSpec library to define different tests.
Note that the first test is a specific test with fixed inputs.
The second and third test use QuickCheck.

\begin{code}
main :: IO ()
main = print ":)"
\end{code}

%To run the tests, use \verb|stack test|.

%To also find out which part of your program is actually used for these tests,
%run \verb|stack clean && stack test --coverage|. Then look for ``The coverage
%report for ... is available at ... .html'' and open this file in your browser.
%See also: \url{https://wiki.haskell.org/Haskell_program_coverage}.
