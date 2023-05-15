module DFA_raw where

import Data.List
import Data.Maybe()

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

-- just to play around, have it defined outside previous:
stateArrNF:: NFA -> [State] -> String -> [State]  
stateArrNF _ qs [] = qs
stateArrNF _ [] _ = []
stateArrNF nf [q] (x:xs) =  stateArrNF nf (deltaNF nf x q) xs   --recursion on string
stateArrNF nf (q:qs) (x:xs) = union (stateArrNF nf [q] (x:xs)) (stateArrNF nf qs (x:xs)) --recursion on statespace


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

     -- this is a modification

unionL:: Eq a => [[a]] -> [a]
unionL [] = []
unionL (li:lis) = union li (unionL lis)

data ENFA = ENFA { statesENF :: [State]
                , alpabetENF:: [Symbol]
                , deltaENF :: Symbol -> State -> [State]
                , epTrans:: State -> [State]
                , startENF:: State
                , acceptstateENF:: [State]}

trClose:: (State -> [State]) -> [State] -> State -> [State]
trClose func vis st 
    |sort (nub vis) == sort (nub ( unionL [trClose func (st:vis) q | q <- (st: (func st))])) 
     = sort (nub (st: unionL [trClose func (st:vis) q | q <- (st: (func st))]))
    | otherwise = sort.nub $ vis
-- Toy example: 
funcy:: State -> [State]
funcy num
    | num == 0 = [1,2]
    | num == 2 = [3,4]
    | otherwise = [-1]

evaluateENF:: ENFA -> String -> Bool
evaluateENF nf st = and (map ((flip $ elem) (alpabetENF nf)) st) && -- captures elements of string in alphabet
             or (map ((flip $ elem) (acceptstateENF nf)) (stateArrENF' ([startENF nf]) st))  where
                stateArrENF':: [State] -> String -> [State]  
                stateArrENF' qs [] = qs
                stateArrENF' [] _ = []
                stateArrENF' [q] (x:xs) | q == -1 = []
                                        | otherwise = stateArrENF' (unionL (map (deltaENF nf x) lis)) xs where
                                          lis = trClose (epTrans nf) [] q
                stateArrENF' (q:qs) (x:xs) = union (stateArrENF' [q] (x:xs)) (stateArrENF' qs (x:xs)) --recursion on statespace



