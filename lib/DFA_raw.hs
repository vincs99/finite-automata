module DFA_raw where

import Data.List
import System.Random

type State = Int
type Symbol = Char
data DFA = DFA { states :: [State]
                , alpabet:: [Symbol]
                , delta :: Symbol -> State -> State
                ,  start:: State
                , acceptstate:: [State]}

evaluate:: DFA -> String -> Bool
evaluate df st = and (map ((flip $ elem) (alpabet df)) st) && -- captures that all element of string in alphabet
                (stateArr (start df) st) `elem` acceptstate df where 
                    stateArr:: State -> String -> State -- recursively go to state at end of string
                    stateArr q [] = q
                    stateArr q (x:xs) = stateArr (delta df x q) xs

-- Toy example: accepts all strings starting with 0
zeroStart:: DFA
zeroStart = DFA [0,1,2] ['0', '1'] deltazero 0 [1] where
    deltazero:: Symbol -> State -> State
    deltazero char st
     | st == 0 && char == '0' = 1
     | st == 0 && char == '1' = 2
     | st == 1 = 1
     | st == 2 = 2
     | otherwise = -1

data NFA = NFA { statesNF :: [State]
                , alpabetNF:: [Symbol]
                , deltaNF :: Symbol -> State -> [State]
                , startNF:: State
                , acceptstateNF:: [State]}

evaluateNF:: NFA -> String -> Bool
evaluateNF nf st = and (map ((flip $ elem) (alpabetNF nf)) st) && -- captures elements of string in alphabet
             or (map ((flip $ elem) (acceptstateNF nf)) (stateArrNF' ([startNF nf]) st))  where
                stateArrNF':: [State] -> String -> [State]  
                stateArrNF' qs [] = qs
                stateArrNF' [] _ = []
                stateArrNF' [q] (x:xs) | q == -1 = []
                                       | otherwise = stateArrNF' (deltaNF nf x q) xs --recursion on string
                stateArrNF' (q:qs) (x:xs) = union (stateArrNF' [q] (x:xs)) (stateArrNF' qs (x:xs)) --recursion on statespace


-- Toy example: accepts all strings starting with 0
zeroStartNF:: NFA
zeroStartNF = NFA [0,1,2, 3] ['0', '1'] deltazeroNF 0 [1] where
    deltazeroNF:: Symbol-> State -> [State]
    deltazeroNF char st
     | st == 0 && char == '0' = [1]
     | st == 0 && char == '1' = [2, 3]
     | st == 1 = [1]
     | st == 2 = [2, 3]
     | st == 3 = [3]
     | otherwise = [-1]

unionL:: Eq a => [[a]] -> [a]
unionL [] = []
unionL (li:lis) = union li (unionL lis)

data ENFA = ENFA { statesENF :: [State]
                , alpabetENF:: [Symbol]
                , deltaENF :: Symbol -> State -> [State]
                , epTrans:: [(State, State)]
                , startENF:: State
                , acceptstateENF:: [State]}

-- Taken from https://stackoverflow.com/questions/19212558/transitive-closure-from-a-list-using-haskell
trClose :: Eq a => [(a, a)] -> [(a, a)]
trClose closure 
  | closure == closureUntilNow = closure
  | otherwise                  = trClose closureUntilNow
  where closureUntilNow = 
          nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, b == b']


refClose:: Eq a => [a] -> [(a,a)] -> [(a,a)]
refClose as ps = nub (ps ++ [(x,x) | x <- as])


evaluateENF:: ENFA -> String -> Bool
evaluateENF nf st = and (map ((flip $ elem) (alpabetENF nf)) st) && -- captures elements of string in alphabet
             or (map ((flip $ elem) (acceptstateENF nf)) (stateArrENF' ([startENF nf]) st))  where
                stateArrENF':: [State] -> String -> [State]  
                stateArrENF' qs [] = qs
                stateArrENF' [] _ = []
                stateArrENF' [q] (x:xs) | q == -1 = []
                                        | otherwise = stateArrENF' (unionL (map (deltaENF nf x) lis)) xs where
                                          lis = [p | p <- statesENF nf , (q, p) `elem` (trClose (rcl)) ] where
                                            rcl = refClose (statesENF nf) (epTrans nf) 
                stateArrENF' (q:qs) (x:xs) = union (stateArrENF' [q] (x:xs)) (stateArrENF' qs (x:xs)) --recursion on statespace

-- Toy example: accepts all strings starting with 0
zeroStartENF:: ENFA
zeroStartENF = ENFA [0,1,2, 3] ['0', '1'] deltazeroNF [(0,1), (2,3)] 0 [1] where
    deltazeroNF:: Symbol-> State -> [State]
    deltazeroNF char st
     | st == 0 && char == '0' = [1]
     | st == 0 && char == '1' = [2, 3]
     | st == 1 = [1]
     | st == 2 = [2, 3]
     | st == 3 = [3]
     | otherwise = [-1]


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


-- Toy example: check if the word generated by (01)* gets accepted by DFA zeroStart
qcToy :: IO ()
qcToy = do
  s <- generateWord (Star (Con (R "0") (R "1")))
  if evaluate zeroStart s 
    then print ("zeroStart accepts " ++ s) 
    else print ("zeroStart rejects " ++ s)

qc :: DFA -> RegExp -> Int -> IO ()
qc _ _ 0 = print "No counterexample found"
qc dfa ex n =  do
  s <- generateWord ex
  if evaluate dfa s
    then qc dfa ex (n-1)
    else 
      if s == "" 
      then print "The DFA rejected the empty string"
      else print ("the DFA rejected " ++ s) 


-- More toys
qc1 :: IO ()
qc1 = qc zeroStart (Con (R "0") (Star (R "1"))) 100
qc2 :: IO ()
qc2 = qc zeroStart (Plus (Con (R "0") (R "1") )) 100
qc3 :: IO ()
qc3 =  qc zeroStart (Star (Con (R "0") (R "1") )) 100


  
  