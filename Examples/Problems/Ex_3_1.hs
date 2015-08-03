-- | The example from Bertsekas, p. 22.
module Examples.Problems.Ex_3_1 where

import Data.Array

import MDP.MDP

data States = A | B
            deriving (Show, Ord, Eq)
data Controls = U1 | U2
              deriving (Show, Ord, Eq)

transition U1 A A = 3 / 4
transition U1 A B = 1 / 4
transition U1 B A = 3 / 4
transition U1 B B = 1 / 4
transition U2 A A = 1 / 4
transition U2 A B = 3 / 4
transition U2 B A = 1 / 4
transition U2 B B = 3 / 4

cost U1 A = 2
cost U2 A = 1 / 2
cost U1 B = 1
cost U2 B = 3

alpha = 9 / 10

states = [A, B]
controls = [U1, U2]

mdp = mkMDP states controls transition cost (\_ -> controls) alpha
