{-# OPTIONS_GHC -F -pgmF htfpp #-}

module MDPTest where

-- Library imports
import Algorithms.MDP.MDP as MDP
import Algorithms.MDP.ValueIteration

-- Testing imports
import MDPArbitrary

-- Remaining imports
import Test.Framework

prop_zeroCostFunction :: MDP.MDP Int Int -> Bool
prop_zeroCostFunction mdp = let
  zero = mkZeroCostFunction mdp
  in and [snd (zero s) == 0 | s <- MDP.unStates mdp]

prop_isStochastic :: MDP.MDP Int Int -> Bool
prop_isStochastic mdp = isStochastic mdp 1e-3

--main = htfMain htf_thisModulesTests
    
