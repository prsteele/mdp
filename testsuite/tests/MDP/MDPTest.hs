import Algorithms.MDP.MDP as MDP
import Algorithms.MDP.ValueIteration

import MDPArbitrary
import TestingBase

import Test.QuickCheck
import Test.QuickCheck.Test
import Control.Monad
import System.Exit


prop_zeroCostFunction :: (Ord a, Ord b) => MDP.MDP a b -> Bool
prop_zeroCostFunction mdp = let
  zero = mkZeroCostFunction mdp
  in and [snd (zero s) == 0 | s <- MDP.unStates mdp]

prop_isStochastic :: (Ord a, Ord b) => (MDP.MDP a b) -> Bool
prop_isStochastic mdp = isStochastic mdp 1e-3

tests :: [TestProperty]
tests = [ ("prop_zerocostfunction"
          , quickCheckResult (prop_zeroCostFunction :: (MDP.MDP Int Int) -> Bool))
        , ("prop_isStochastic"
          , quickCheckResult (prop_isStochastic :: (MDP.MDP Int Int) -> Bool))
        ]

main = testingMain tests

