-- | The problem described by Bertsekas p. 210.
module Algorithms.MDP.Examples.Ex_3_2 where

import Algorithms.MDP.Examples.Ex_3_1 hiding (mdp)

import Algorithms.MDP

-- | The MDP representing the problem.
mdp :: MDP State Control Double
mdp = mkUndiscountedMDP states controls transition costs (\_ -> controls)
