module Algorithms.MDP.Examples.Ex_3_1 where

import Algorithms.MDP

data State = A | B
           deriving (Show, Ord, Eq)
data Control = U1 | U2
             deriving (Show, Ord, Eq)

transition U1 A A = 3 / 4
transition U1 A B = 1 / 4
transition U1 B A = 3 / 4
transition U1 B B = 1 / 4
transition U2 A A = 1 / 4
transition U2 A B = 3 / 4
transition U2 B A = 1 / 4
transition U2 B B = 3 / 4

costs U1 A = 2
costs U2 A = 1 / 2
costs U1 B = 1
costs U2 B = 3

alpha = 9 / 10

states = [A, B]
controls = [U1, U2]

mdp = mkDiscountedMDP states controls transition costs (\_ -> controls) alpha
