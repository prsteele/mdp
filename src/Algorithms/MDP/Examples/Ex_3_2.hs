module Algorithms.MDP.Examples.Ex_3_2 where

import Algorithms.MDP.Examples.Ex_3_1 hiding (mdp)

import Algorithms.MDP.UndiscountedMDP

mdp = mkUndiscountedMDP states controls transition cost (\_ -> controls)
