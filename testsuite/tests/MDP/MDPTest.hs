import Algorithms.MDP.MDP as MDP
import Algorithms.MDP.ValueIteration

import MDPArbitrary

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

prop_zeroCostFunction :: (Ord a, Ord b) => MDP.MDP a b -> Bool
prop_zeroCostFunction mdp = let
  zero = mkZeroCostFunction mdp
  in and [snd (zero s) == 0 | s <- MDP.unStates mdp]

prop_isStochastic :: (Ord a, Ord b) => (MDP.MDP a b) -> Bool
prop_isStochastic mdp = isStochastic mdp 1e-3

 tests = [ ("prop_zerocostfunction"
           , quickCheckResult (prop_zeroCostFunction :: (MDP.MDP Int Int) -> Bool))
         , ("prop_increasing"
           , quickCheckResult (prop_increasing :: (MDP.MDP Int Int) -> Bool))
         , ("prop_isStochastic"
           , quickCheckResult (prop_isStochastic :: (MDP.MDP Int Int) -> Bool))]

printFailures results = let
  failedTests = filter (isSuccess . fst) (zip results tests)
  failedNames = map (fst . snd) failedTests
  in if null failedTests
     then return ()
     else do
       putStrLn "Failed tests:"
       mapM_ putStrLn failedNames

main = do
  results <- mapM snd tests
  printFailures results
  unless (all isSuccess results) exitFailure 

