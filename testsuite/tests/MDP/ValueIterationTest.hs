{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ValueIterationTest where

-- Library imports
import Algorithms.MDP.MDP as MDP
import Algorithms.MDP.ValueIteration

-- Testing imports
import MDPArbitrary

-- Remaining imports
import Test.Framework

increasing :: (Ord a) => [a] -> Bool
increasing (p:q:qs) = if p <= q
                      then increasing (q:qs)
                      else False
increasing _        = True

-- | We know that value iteration produces larger costs for each state
-- over each iteration.
prop_increasing :: MDP.MDP Int Int -> Bool
prop_increasing mdp = let
  iterations = valueIteration mdp
  valuesFor s = map snd (map ($ s) iterations)

  increasingFor s = increasing (take 10 (valuesFor s))
  in all increasingFor (unStates mdp)

--main = htfMain htf_thisModulesTests
