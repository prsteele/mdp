{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | This module tests the standard value iteration algorithm for
-- discounted problems by comparing its iterations to known iterations
-- from "Dynamic Programming and Optimal Control", Dimitri
-- P. Bertsekas, p. 23.
module Algorithms.MDP.Ex_3_1_RelativeTest where

import Test.Framework
import Data.Maybe

import Algorithms.MDP.Ex_3_1_Test (correctValuesA, correctValuesB, almostEqual)
import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP.MDP
import Algorithms.MDP.ValueIteration

lowerValuesA =
  [ read "-Infinity"
  , 5.000
  , 6.350
  , 6.856
  , 7.129
  , 7.232
  , 7.287
  , 7.308
  , 7.319
  , 7.324
  , 7.326
  , 7.327
  , 7.327
  , 7.327
  , 7.328
  , 7.328
  ]

upperValuesA =
  [ read "Infinity"
  , 9.500
  , 8.375
  , 7.767
  , 7.540
  , 7.417
  , 7.371
  , 7.345
  , 7.336
  , 7.331
  , 7.329
  , 7.328
  , 7.328
  , 7.328
  , 7.328
  , 7.328
  ]

lowerValuesB =
  [ read "-Infinity"
  , 5.500
  , 6.625
  , 7.232
  , 7.460
  , 7.583
  , 7.629
  , 7.654
  , 7.663
  , 7.669
  , 7.671
  , 7.672
  , 7.672
  , 7.672
  , 7.672
  , 7.672
  ]

upperValuesB =
  [ read "Infinity"
  , 10.000
  , 8.650
  , 8.144
  , 7.870
  , 7.768
  , 7.712
  , 7.692
  , 7.680
  , 7.676
  , 7.674
  , 7.673
  , 7.673
  , 7.673
  , 7.672
  , 7.672
  ]

iterations = take 16 (relativeValueIteration mdp)

lower s (CFBounds cf lb _)  = pure (+ lb) <*> cost s cf
upper s (CFBounds cf _  ub) = pure (+ ub) <*> cost s cf

actualValuesA = catMaybes $ map (cost A . _CF) iterations
actualValuesB = catMaybes $ map (cost B . _CF) iterations

actualLowerA = catMaybes $ map (lower A) iterations
actualUpperA = catMaybes $ map (upper A) iterations
actualLowerB = catMaybes $ map (lower B) iterations
actualUpperB = catMaybes $ map (upper B) iterations

badActualA = filter (not . almostEqual 1e-3) $ zip actualValuesA correctValuesA
badActualB = filter (not . almostEqual 1e-3) $ zip actualValuesB correctValuesB

badLBA = filter (not . almostEqual 1e-3) $ zip actualLowerA lowerValuesA
badUBA = filter (not . almostEqual 1e-3) $ zip actualUpperA upperValuesA
badLBB = filter (not . almostEqual 1e-3) $ zip actualLowerB lowerValuesB
badUBB = filter (not . almostEqual 1e-3) $ zip actualUpperB upperValuesB

test_AValues = assertBoolVerbose (unlines (map show badActualA)) (null badActualA)
test_BValues = assertBoolVerbose (unlines (map show badActualB)) (null badActualB)
test_LBA = assertBoolVerbose (unlines (map show badLBA)) (null badLBA)
test_UBA = assertBoolVerbose (unlines (map show badUBA)) (null badUBA)
test_LBB = assertBoolVerbose (unlines (map show badLBB)) (null badLBB)
test_UBB = assertBoolVerbose (unlines (map show badUBB)) (null badUBB)

