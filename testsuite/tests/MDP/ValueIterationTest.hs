import Algorithms.MDP.MDP as MDP
import Algorithms.MDP.ValueIteration

import MDPArbitrary
import TestingBase

import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad
import System.Exit


increasing :: (Ord a) => [a] -> Bool
increasing (p:q:qs) = if p <= q
                      then increasing (q:qs)
                      else False
increasing _        = True

-- | We know that value iteration produces larger costs for each state
-- over each iteration.
prop_increasing :: (Ord a, Ord b) => (MDP.MDP a b) -> Bool
prop_increasing mdp = let
  iterations = valueIteration mdp
  valuesFor s = map snd (map ($ s) iterations)

  increasingFor s = increasing (take 10 (valuesFor s))
  in all increasingFor (unStates mdp)

tests :: [TestProperty]
tests = [ ("prop_increasing"
          , quickCheckResult (prop_increasing :: (MDP.MDP Int Int) -> Bool))
        ]

main = testingMain tests
