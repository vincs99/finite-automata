module DFA_raw where

import Data.List
import Data.Maybe()

type State = Int
type Symbol = Char
data DFA = DFA { states :: [State]
                , alpabet:: [Symbol]
                , delta :: State -> Symbol -> State
                ,  start:: State
                , acceptstate:: [State]}

evaluate:: DFA -> String -> Bool
evaluate df st = and (map ((flip $ elem) (alpabet df)) st) && -- captures that all element of string in alphabet
                (stateArr (start df) st) `elem` acceptstate df where 
                    stateArr:: State -> String -> State -- recursively go to state at end of string
                    stateArr q [] = q
                    stateArr q (x:xs) = stateArr (delta df q x) xs

-- Toy example: accepts all strings starting with 0
zeroStart:: DFA
zeroStart = DFA [0,1,2] ['0', '1'] deltazero 0 [1] where
    deltazero:: State -> Symbol -> State
    deltazero st char
     | st == 0 && char == '0' = 1
     | st == 0 && char == '1' = 2
     | st == 1 = 1
     | st == 2 = 2
     | otherwise = -1

data NFA = NFA { statesNF :: [State]
                , alpabetNF:: [Symbol]
                , deltaNF :: State -> Symbol -> [State]
                ,  startNF:: State
                , acceptstateNF:: [State]}

evaluateNF:: NFA -> String -> Bool
evaluateNF nf st = and (map ((flip $ elem) (alpabetNF nf)) st) && -- captures elements of string in alphabet
             or (map ((flip $ elem) (acceptstateNF nf)) (stateArrNF' ([startNF nf]) st))  where
                stateArrNF':: [State] -> String -> [State]  
                stateArrNF' qs [] = qs
                stateArrNF' [] _ = []
                stateArrNF' [q] (x:xs) = stateArrNF' (deltaNF nf q x) xs --recursion on string
                stateArrNF' (q:qs) (x:xs) = union (deltaNF nf q x) (stateArrNF' qs (x:xs)) --recursion on statespace

-- just to play around, have it defined outside previous:
stateArrNF:: NFA -> [State] -> String -> [State]  
stateArrNF _ qs [] = qs
stateArrNF _ [] _ = []
stateArrNF nf [q] (x:xs) =  stateArrNF nf (deltaNF nf q x) xs   --recursion on string
stateArrNF nf (q:qs) (x:xs) = union (deltaNF nf q x) (stateArrNF nf qs (x:xs)) --recursion on statespace


-- Toy example: accepts all strings starting with 0
zeroStartNF:: NFA
zeroStartNF = NFA [0,1,2, 3] ['0', '1'] deltazeroNF 0 [1] where
    deltazeroNF:: State -> Symbol -> [State]
    deltazeroNF st char
     | st == 0 && char == '0' = [1]
     | st == 0 && char == '1' = [2, 3]
     | st == 1 = [1]
     | st == 2 = [2, 3]
     | st == 3 = [3]
     | otherwise = [-1]

