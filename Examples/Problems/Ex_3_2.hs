module Examples.Problems.Ex_3_2 (
  States (..)
  , Controls (..)
  , transition
  , cost
  , states
  , controls
  , alpha
  , mdp
  ) where

import Examples.Problems.Ex_3_1 hiding (alpha, mdp)

import MDP.MDP

alpha = 1.0

mdp = mkMDP states controls transition cost (\_ -> controls) alpha
