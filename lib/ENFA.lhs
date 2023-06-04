\section{Implementation of $\epsilon$-NFAs}\label{sec:ENFA}

Here we modify our implementation of NFAs to account for epsilon transitions. Even though for every $\epsilon$-NFA there is an equivalent NFA, it is useful to have these transitions for representational purposes of some automata, and for understandability of some constructions. 

\subsection{Data type and basics}
We chose to represent
$\epsilon$ transitions via a list of tuples of $\epsilon$-related states. This is for convenience when
taking the $\epsilon$-closure and to highlight the different role $\epsilon$-transitions have compared to standard transitions. The \texttt{show} instance is similar to its DFA counterpart, with the $\epsilon$-relations also listed.
\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module ENFA where
import Data.List ( nub, sort )
import DFA ( Symbol )
import NFA ( randomDeltaNF, unionL )
import Test.QuickCheck ( elements, sublistOf, Arbitrary(arbitrary) )
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
\end{code}
To define $\epsilon$-closure, we need some machinery to calculate the reflexive and transitive closure of some relation. This work is inspired by \cite{CS:30073}.
\begin{code}
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
We use the above to modify the evaluation function to account for $\epsilon$-transitions.
\begin{code}
evaluateENF:: (Eq a, Ord a) => ENFA a -> String -> Bool
evaluateENF nf st = all (`elem` alphabetENF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateENF nf) (stateArrENF' [startENF nf] st)  where
                stateArrENF' qs [] = rtClose nf qs
                stateArrENF' [] _ = []
                stateArrENF' qs (x:xs)  =
                 unionL ( [rtClose nf (stateArrENF' (unionL (map (deltaENF nf x) (rtClose nf [q]))) xs ) | q <-qs ])
  -- this first does transitive closure on the input list, then transitive closure on output list and takes union
\end{code}

\subsection{Arbitrary Generation}
We make ENFAs instance of \texttt{Arbitrary} by slightly modifying the relevant code for NFAs, to the effect of adding 
an arbitrary $\epsilon$-relation list.
\begin{code}
instance Arbitrary (ENFA Int) where
    arbitrary = do
        -- choose a set of up to 10 worlds:
        sts <- (0 :) <$> sublistOf [1..5]
        let sym = ['0', '1']
        delt <- randomDeltaNF sym sts sts
        eps <- sublistOf [(x, y) | x <- sts, y <- sts]
        strt <- elements sts
        accraw <- sublistOf sts
        let acc = nub (0:accraw)
        return $ ENFA sts sym delt eps strt acc
\end{code}