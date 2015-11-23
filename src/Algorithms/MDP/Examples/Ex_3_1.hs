-- | The problem described by Bertsekas p. 22.
module Algorithms.MDP.Examples.Ex_3_1 where

import Algorithms.MDP

-- | There are two distinct states
data State = A | B
           deriving (Show, Ord, Eq)

-- | There are two distinct actions we can take in each state
data Control = U1 | U2
             deriving (Show, Ord, Eq)

-- | The transition matrix
transition :: Control -> State -> State -> Double
transition U1 A A = 3 / 4
transition U1 A B = 1 / 4
transition U1 B A = 3 / 4
transition U1 B B = 1 / 4
transition U2 A A = 1 / 4
transition U2 A B = 3 / 4
transition U2 B A = 1 / 4
transition U2 B B = 3 / 4

-- | The costs associated with each state and action
costs :: Control -> State -> Double
costs U1 A = 2
costs U2 A = 1 / 2
costs U1 B = 1
costs U2 B = 3

-- | The discount factor
alpha :: Double
alpha = 9 / 10

-- | The available states
states :: [State]
states = [A, B]

-- | The available actions
controls :: [Control]
controls = [U1, U2]

-- | The MDP representing the problem.
mdp :: MDP State Control Double
mdp = mkDiscountedMDP states controls transition costs (\_ -> controls) alpha
