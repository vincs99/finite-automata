\section{Regular expressions}\label{sec:RegExp}
In this section, we discuss the implementation of regular expressions, as well as a small toolbox to manipulate them. 


\begin{code}
 
module RegExp where
import DFA
import NFA()
import ENFA()
import Test.QuickCheck
\end{code}

\subsection{Definition}

Recall that a regular expression is defined to be either empty, the emptystring, a single symbol, the union or concatenation of two expressions, or the closure of a language (with or without the emptystring). The datatype \texttt{RegExp} we used closely follows this definition, but also allows for some more flexibility.

\begin{code}
data RegExp = Empty | Epsilon | R [Symbol] | Union RegExp RegExp | Con RegExp RegExp | Star RegExp | Plus RegExp
  deriving (Show, Eq)
\end{code}

The main difference between \texttt{RegExp} and regular expressions, is in the base case for single symbols in the original definition. We chose to allow a list of symbol to increase user-friendliness. This means that instead of \texttt{Con (R "a") (R "b")}, one can write \texttt{R "ab"} to denote the same expression. \texttt{R []} is also equivalent to \texttt{Epsilon}. 


For convenience, we also added a function that takes a list of regular expressions and outputs the expression corresponding to the union of all its members.

\begin{code}
regExpUnion :: [RegExp] -> RegExp
regExpUnion = simplify . foldr Union Empty   
\end{code}

\subsection{Pretty printing and simplifying}
As larger expressions tend to become unreadable, we implemented a function for pretty showing and pretty printing regular expressions.

\begin{code}
pRegExp :: RegExp -> String
pRegExp Empty = ""
pRegExp Epsilon = "R e"
pRegExp (R []) = "R e"
pRegExp (R xs) = show xs
pRegExp (Union r1 r2) = "(" ++ pRegExp r1 ++ "|" ++ pRegExp r2 ++ ")"
pRegExp (Star r) = "(" ++  pRegExp r ++ ")*"
pRegExp (Con r1 r2) = pRegExp r1 ++ pRegExp r2
pRegExp (Plus r) = "(" ++ pRegExp r ++ ")+"

ppRegExp :: RegExp -> IO ()
ppRegExp = print . pRegExp
\end{code}

Here is an example to illustrate the improvement on the readability.\\
\texttt{ppRegExp (Con (Union (Con (R "5") (R"3")) (R [])) (Star (R "19")))} yields\\
 \texttt{"("53"|R e)("19")*"}.
 
Another way to improve readability is to simplify regular expressions. With simplifying we mean to remove unnecessary parts of an expression. E.g.\ any expression \texttt{r} concatenated with \texttt{Empty} is equivalent to \texttt{Empty}, and \texttt{Con r Empty} and \texttt{Union r Epsilon} are equivalent to just \texttt{r}. The function \texttt{simplify} takes a \texttt{RegExp} and outputs an equivalent one with less clutter. As minimising a regular expression is computationally demanding \cite{gramlich2007minimizing}, this function only does basic simplifications, but runs in polynomial time.     
\begin{code}
simplify :: RegExp -> RegExp
simplify r | r == simplify' r = r
 | otherwise = simplify $ simplify' r where
  simplify' Empty = Empty
  simplify' Epsilon = Epsilon
  simplify' (R xs) = R xs
  simplify' (Con Epsilon Epsilon) = Epsilon
  simplify' (Con _ Empty) = Empty
  simplify' (Con Empty _) = Empty
  simplify' (Con r' Epsilon) = simplify' r'
  simplify' (Con Epsilon r') = simplify' r'
  simplify' (Con r1 r2) = Con (simplify' r1) (simplify' r2)
  simplify' (Union Empty r') = simplify' r'
  simplify' (Union r' Empty) = simplify' r'
  simplify' (Union r1 r2) | r1 /= r2 = Union (simplify' r1) (simplify' r2)
                          | otherwise = simplify' r1
  simplify' (Plus Epsilon) = Epsilon
  simplify' (Star Epsilon) = Epsilon
  simplify' (Plus Empty) = Empty
  simplify' (Star Empty) = Epsilon
  simplify' (Star r') = Star (simplify' r')
  simplify' (Plus r') = Plus (simplify' r')
\end{code}

\subsection{Arbitrary generation of regular expressions}
We create an Arbitrary instance for RegExp in order to be able to generate regular expressions. The main purpose for this is testing of properties. Note that we exclude the Empty expression from the generation, as empty expressions cannot generate words. The parameter for \texttt{sized} decreases swiftly in order to keep the generated expressions of a reasonable and readable size. This is necessary for feasible runtimes in the translation process from regular expressions to DFAs later on.
\begin{code}
instance Arbitrary RegExp where
  arbitrary = sized randomReg where
    randomReg :: Int -> Gen RegExp
    randomReg 0 = R <$> elements ["0", "1"]
    randomReg n = oneof [ R <$> elements ["0", "1"]
                          , Star <$> randomReg (n `div` 8)
                          , Plus <$> randomReg (n `div` 8)
                          , Con <$> randomReg (n `div` 8)
                          <*> randomReg (n `div` 8) 
                          , Union <$> randomReg (n `div` 8)
                          <*> randomReg (n `div` 8)
                          , return Epsilon ]

\end{code}

\subsection{Generating words from regular expressions}
The following code is used to generate words from expressions. The intended use is to combine it with \texttt{quickCheck} to generate the strings. Note that an \texttt{error} is thrown when generating from the \texttt{Empty} expression. This is because it is not possible to generate words from nothing. Moreover, the output needs to be distinguished from the empty string. To make sure this error is only thrown when explicitly running \texttt{generateString Empty} and not when, for example, executing \texttt{generateString (Union (R "0") Empty)}, we use \texttt{simplify}. 

\begin{code}
generateString :: RegExp -> Gen String
generateString = (`generateString'` 5) . simplify
  where generateString' :: RegExp -> Int -> Gen String
        generateString' Empty _ = error "cannot generate from empty language" 
        generateString' Epsilon _ = return ""
        generateString' (R xs) _ = return xs
        generateString' (Union r1 r2) n = oneof [generateString' r1 n, generateString' r2 n]
        generateString' (Con r1 r2) n = do
            w <- generateString' r1 n
            v <- generateString' r2 n
            return (w ++ v)
        generateString' (Star _) 0 = return ""    
        generateString' (Star r) n = oneof [generateString' Epsilon n, generateString' (Con r (Star r)) (n - 1)]
        generateString' (Plus r) n = generateString' (Con r (Star r)) n
\end{code}