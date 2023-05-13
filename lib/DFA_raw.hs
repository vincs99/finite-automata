module DFA_raw where

import Data.List()
import Data.Maybe()

type State = Int
type Symbol = Char
data DFA = DFA { states :: [State]
                , alpabet:: [Symbol]
                , delta :: State -> Symbol -> State
                ,  start:: State
                , acceptstate:: [State]}

evaluate:: DFA -> String -> Bool
evaluate df st = (stateArr (start df) st) `elem` acceptstate df where
    stateArr:: State -> String -> State
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





