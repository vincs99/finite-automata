module DFA_raw where

import Data.List
import System.Random

type State = Int
type Symbol = Char
data DFA = DFA { states :: [[State]] -- For ease with subset construct, represent by set of states.
                , alphabet:: [Symbol]
                , delta :: Symbol -> [State] -> [State]
                ,  start:: [State]
                , acceptstate:: [[State]]}

evaluate:: DFA -> String -> Bool
evaluate df st = all (`elem` alphabet df) st && -- captures that all element of string in alphabet
                stateArr (start df) st `elem` acceptstate df where 
                    stateArr:: [State] -> String -> [State] -- recursively go to state at end of string
                    stateArr q [] = q
                    stateArr q (x:xs) = stateArr (delta df x q) xs

-- Toy example: accepts all strings starting with 0
zeroStart:: DFA
zeroStart = DFA [[0],[1],[2]] ['0', '1'] deltazero [0] [[1]] where
    deltazero:: Symbol -> [State] -> [State]
    deltazero char st
     | st == [0] && char == '0' = [1]
     | st == [0] && char == '1' = [2]
     | st == [1] = [1]
     | st == [2] = [2]
     | otherwise = [-1]

data NFA = NFA { statesNF :: [State]
                , alphabetNF:: [Symbol]
                , deltaNF :: Symbol -> State -> [State]
                , startNF:: State
                , acceptstateNF:: [State]}

evaluateNF:: NFA -> String -> Bool
evaluateNF nf st = all (`elem` alphabetNF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateNF nf) (stateArrNF' [startNF nf] st)  where
                stateArrNF':: [State] -> String -> [State]  
                stateArrNF' qs [] = qs
                stateArrNF' [] _ = []
                stateArrNF' [q] (x:xs) | q == -1 = []
                                       | otherwise = stateArrNF' (deltaNF nf x q) xs --recursion on string
                stateArrNF' (q:qs) (x:xs) = stateArrNF' [q] (x : xs) `union` stateArrNF' qs (x : xs) --recursion on statespace


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
unionL = foldr union []


data ENFA = ENFA { statesENF :: [State]
                , alphabetENF:: [Symbol]
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

rtClose:: ENFA -> State -> [State]
rtClose nf q = [p | p <- statesENF nf , (q, p) `elem` trClose rcl ] where
                                            rcl = refClose (statesENF nf) (epTrans nf)


evaluateENF:: ENFA -> String -> Bool
evaluateENF nf st = all (`elem` alphabetENF nf) st && -- captures elements of string in alphabet
             any (`elem` acceptstateENF nf) (stateArrENF' [startENF nf] st)  where
                stateArrENF':: [State] -> String -> [State]  
                stateArrENF' qs [] = qs
                stateArrENF' [] _ = []
                stateArrENF' [q] (x:xs) | q == -1 = []
                                        | otherwise = stateArrENF' (unionL (map (deltaENF nf x) (rtClose nf q))) xs
                stateArrENF' (q:qs) (x:xs) = stateArrENF' [q] (x : xs) `union` stateArrENF' qs (x : xs) --recursion on statespace

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

 -- Powerset construction

transNtoD:: NFA -> DFA
transNtoD (NFA sts alph del strt ac) = 
  DFA (subsequences sts) alph del' [strt] [st | st <- subsequences sts,  intersect st ac /= []] where
    del':: Symbol -> [State] -> [State]
    del' sy ls = unionL [del sy l | l <- ls] 

transENtoD:: ENFA -> DFA
transENtoD (ENFA sts alph del eps strt ac) = let nf = ENFA sts alph del eps strt ac in 
  DFA (subsequences sts) alph del' (rtClose nf strt) [st | st <- subsequences sts,  intersect st ac /= []] where
    del':: Symbol -> [State] -> [State]
    del' sy ls = let nf = ENFA sts alph del eps strt ac in  unionL (map (rtClose nf) lis) where
                                                              lis = unionL [del sy l | l <- ls] 

-- Regexp implementation


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
qc4 :: IO ()
qc4 = qc zeroStart (Star (Union (R "a") (R "1"))) 100



-- RegExp to ENFA
regExpToENFA :: RegExp -> ENFA
regExpToENFA Epsilon = ENFA [0] [] (\_ _ -> []) [] 0 [0]
regExpToENFA (R xs) = ENFA [0..length xs -1] (nub xs) delta [] 0 [length xs -1] where
  delta symbol state | xs !! state == symbol = [state + 1]
                     | otherwise = []
regExpToENFA (Union r1 r2) = regExpToENFA r1 `unionENFA` makeDisjoint (regExpToENFA r1) (regExpToENFA r2)
regExpToENFA (Star r) = starENFA (regExpToENFA r)
regExpToENFA (Con r1 r2) = regExpToENFA r1 `concatENFA` makeDisjoint (regExpToENFA r1) (regExpToENFA r2)
regExpToENFA (Plus r) = regExpToENFA r `concatENFA` starENFA (makeDisjoint (regExpToENFA r) (regExpToENFA r))


-- function that takes two ENFAs and outputs a relabeling of the second ENFA such that the states of both become disjoint
makeDisjoint :: ENFA -> ENFA -> ENFA
makeDisjoint n1 n2 = ENFA states alphabet delta epT start accept where
  add = maximum (statesENF n1)
  states = map (+ add) (statesENF n2)
  alphabet = alphabetENF n2
  delta sym state = deltaENF n2 sym (state + add)
  epT = [(s + add, t + add) | (s,t) <- epTrans n2 ] 
  start = start + add
  accept = map (+ add) (acceptstateENF n2)


unionENFA :: ENFA -> ENFA -> ENFA -- Use only if states are disjoint
unionENFA n1 n2 = ENFA states alphabet delta epT start accept where
  states = -1 : statesENF n1 ++ statesENF n2
  alphabet = alphabetENF n1 `union` alphabetENF n2
  delta sym st = deltaENF n1 sym st ++ deltaENF n2 sym st
  epT = epTrans n1 ++ epTrans n2 ++ [(-1, startENF n1), (-1, startENF n2)]
  start = -1
  accept = acceptstateENF n1 ++ acceptstateENF n2

starENFA :: ENFA -> ENFA
starENFA n = ENFA (statesENF n) (alphabetENF n) (deltaENF n) ep (startENF n) (acceptstateENF n)  where
  ep = epTrans n ++ [(s, startENF n) | s <- acceptstateENF n]

concatENFA :: ENFA -> ENFA -> ENFA -- Use only if states are disjoint again
concatENFA n1 n2 = ENFA states alphabet delta epT start accept where
  states = statesENF n1 ++ statesENF n2
  alphabet = alphabetENF n1 `union` alphabetENF n2
  delta sym st = deltaENF n1 sym st ++ deltaENF n2 sym st
  epT = epTrans n1 ++ epTrans n2 ++ [(s, startENF n2) | s <- acceptstateENF n1]
  start = startENF n1
  accept = acceptstateENF n2

  --an alteration


 
