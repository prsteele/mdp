import Algorithms.MDP.MDP as MDP
import Algorithms.MDP.ValueIteration

import MDPArbitrary

import Test.QuickCheck

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

-- -- prop_isStochastic :: ValidTrans -> Bool
-- -- prop_isStochastic (ValidTrans trans) = let
-- --   mdp = mkMDP states actions trans (\a b -> 0) (\a -> actions) 1.0
-- --   in isStochastic mdp 0

example = mkMDP [1] [1, 2] (\a _ _ -> if a == 2 then 1 else 0) (\_ _ -> 2.67) (\_ -> [2]) 0.5

main = quickCheck (prop_increasing :: (MDP.MDP Int Int) -> Bool)
