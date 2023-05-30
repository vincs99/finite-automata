\section{Regular expressions}\label{sec:RegExp}
Here we implement regular expressions and a toolset to generate words that the regexp accepts.

\begin{code}
module RegExp where

import System.Random
import DFA
import NFA()
import ENFA()
import Test.QuickCheck


data RegExp = Empty | Epsilon | R [Symbol] | Union RegExp RegExp | Star RegExp | Con RegExp RegExp | Plus RegExp
  deriving (Show, Eq)

pRegExp :: RegExp -> String
pRegExp Empty = ""
pRegExp Epsilon = "R e"
pRegExp (R xs) = show xs
pRegExp (Union r1 r2) = "(" ++ pRegExp r1 ++ " u " ++ pRegExp r2 ++ ")"
pRegExp (Star r) = "(" ++  pRegExp r ++ ")*"
pRegExp (Con r1 r2) = pRegExp r1 ++ pRegExp r2
pRegExp (Plus r) = "(" ++ pRegExp r ++ ")+"

regExpUnion :: [RegExp] -> RegExp
regExpUnion [] = Empty
regExpUnion [r] = r
regExpUnion (r:rs) =  foldr Union r rs


generateWord :: RegExp -> IO String
generateWord = generateWord' . simplify
  where 
  generateWord' Empty = error "cannot generate from empty language"
  generateWord' Epsilon = pure ""
  generateWord' (R xs) = pure xs
  generateWord' (Union r1 r2) = do
    i <- getStdRandom (randomR (0,1::Int))
    [generateWord' r1, generateWord' r2] !! i
  generateWord' (Con r1 r2) = do
    one <- generateWord' r1
    two <- generateWord' r2
    return (one ++ two)
  generateWord' (Star r) = do
    i <- getStdRandom (randomR (0,1::Int))
    [generateWord' Epsilon, generateWord' (Con r (Star r))] !! i
  generateWord' (Plus r) = generateWord' (Con r (Star r)) 
\end{code}

We create an Arbitrary instance for RegExp to be able to generate regular expressions. 
Note we exclude the Empty expression from the generation, as we cannot generate words from that.
\begin{code}
instance Arbitrary RegExp where
  arbitrary = sized randomReg where
    randomReg :: Int -> Gen RegExp
    randomReg 0 = return Epsilon
    randomReg n = oneof [ R <$> elements ["0", "1"]
                          , Star <$> randomReg (n `div` 8)
                          , Plus <$> randomReg (n `div` 8)
                          , Con <$> randomReg (n `div` 8)
                          <*> randomReg (n `div` 8) 
                          , Union <$> randomReg (n `div` 8)
                          <*> randomReg (n `div`8)]

\end{code}


\begin{code}

generateString :: RegExp -> Gen String
generateString = generateString' . simplify
  where generateString' :: RegExp -> Gen String
        generateString' Empty = error "cannot generate from empty language" 
        generateString' Epsilon = return ""
        generateString' (R xs) = return xs
        generateString' (Union r1 r2) = oneof [generateString' r1, generateString' r2]
        generateString' (Con r1 r2) = do
            w <- generateString' r1
            v <- generateString' r2
            return (w ++ v)
        generateString' (Star r) = oneof [generateString' Epsilon, generateString' (Con r (Star r)), generateString' (Con r (Star r))]
        generateString' (Plus r) = generateString' (Con r (Star r))



simplify :: RegExp -> RegExp
simplify r | r == simplify' r = r
           | otherwise = simplify $ simplify' r
  where
  simplify' :: RegExp -> RegExp
  simplify' Empty = Empty
  simplify' Epsilon = Epsilon
  simplify' (R xs) = R xs
  simplify' (Con Empty Empty) = Empty
  simplify' (Con Epsilon Epsilon) = Epsilon
  simplify' (Con _ Empty) = Empty
  simplify' (Con Empty _) = Empty
  simplify' (Con r' Epsilon) = simplify' r'
  simplify' (Con Epsilon r') = simplify' r'
  simplify' (Con r1 r2) = Con (simplify' r1) (simplify' r2)
  simplify' (Union Empty Empty) = Empty
  simplify' (Union Empty r') = simplify' r'
  simplify' (Union r' Empty) = simplify' r'
  simplify' (Union Epsilon Epsilon) = Epsilon
  simplify' (Union r1 r2) | r1 /= r2 = Union (simplify' r1) (simplify' r2)
                          | otherwise = simplify' r1
  simplify' (Plus Epsilon) = Epsilon
  simplify' (Star Epsilon) = Epsilon
  simplify' (Plus Empty) = Empty
  simplify' (Star Empty) = Epsilon
  simplify' (Star r') = Star (simplify' r')
  simplify' (Plus r') = Plus (simplify' r')


\end{code}