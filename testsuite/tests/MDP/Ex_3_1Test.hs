{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Ex_3_1Test where

import Algorithms.MDP.ValueIteration
import Algorithms.MDP.Examples.Ex_3_1

import Test.HUnit
import Test.Framework

iterations = valueIteration mdp

computed state = map (snd . ($ state)) iterations

expected A = [ 0
             , 0.500
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
             , 5.412
             , 5.612
             , 5.783
             ]

expected B = [ 0
             , 1.000
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

data Iter = Iter Int States Double Double

almostEq eps (Iter _ _ a b) = abs (a - b) <= eps

itersFor state =
  [Iter i state a b | (i, a, b) <- zip3 [0..] (computed state) (expected state)]

iters = itersFor A ++ itersFor B

failedIters = filter (not . (almostEq 1e3)) iters

messageFor (Iter iter state actualValue expectedValue)
  = unwords [ "Iteration"
            , show iter
            , "state"
            , show state
            , show actualValue
            , "/="
            , show expectedValue]

test_correct = assertBoolVerbose (unlines (map messageFor failedIters)) (null failedIters)
