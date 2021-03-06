{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | This module tests the standard value iteration algorithm for
-- discounted problems by comparing its iterations to known iterations
-- from "Dynamic Programming and Optimal Control", Dimitri
-- P. Bertsekas, p. 23.
module Algorithms.MDP.Ex_3_1_Test where

import Test.Framework

import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP
import Algorithms.MDP.ValueIteration

almostEqual eps (x, y) | x == y    = True
                       | otherwise = abs (x - y) <= eps

iterations = take 16 (valueIteration mdp)

correctValuesA =
  [ 0
  , 0.5
  , 1.287
  , 1.844
  , 2.414
  , 2.896
  , 3.343
  , 3.740
  , 4.099
  , 4.422
  , 4.713
  , 4.974
  , 5.209
  , 5.421
  , 5.612
  , 5.783
  ]

correctValuesB =
  [ 0
  , 1
  , 1.562
  , 2.220
  , 2.745
  , 3.247
  , 3.686
  , 4.086
  , 4.444
  , 4.767
  , 5.057
  , 5.319
  , 5.554
  , 5.766
  , 5.957
  , 6.128
  ]

actualValuesA = map (cost A) iterations
actualValuesB = map (cost B) iterations

pairsA = zip actualValuesA correctValuesA
pairsB = zip actualValuesB correctValuesB

badPairsA = filter (not . almostEqual 1e-3) pairsA
badPairsB = filter (not . almostEqual 1e-3) pairsB

test_AValues = assertBoolVerbose (unlines (map show badPairsA)) (null badPairsA)
test_BValues = assertBoolVerbose (unlines (map show badPairsB)) (null badPairsB)
