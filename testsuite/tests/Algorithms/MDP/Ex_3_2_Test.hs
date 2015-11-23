{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | This module tests the undiscountedRelativeValueIteration function
-- for undiscounted problems by comparing its tierations to known
-- iteratinos from "Dynamic Programming and Optimal Control", Dimitri
-- P. Bertsekas, p. 210.
--
-- We actually implement a slightly different technique to solve this
-- problem than is reported in Bertsekas; however, our solutions
-- should converge to the same value. Thus we simply ensure that the
-- error bounds we report properly contain the solution reported by
-- Bertsekas.
module Algorithms.MDP.Ex_3_2_Test where

import Test.Framework

import Algorithms.MDP
import Algorithms.MDP.ValueIteration
import Algorithms.MDP.Examples.Ex_3_2

value = 0.750

iterations = take 11 (undiscountedRelativeValueIteration mdp)

estimate (CFBounds _ lb ub) = (lb, ub)

proper (lb, ub) = lb <= value && value <= ub

badPairs = filter (not . proper) (map estimate iterations)

test_values = assertBoolVerbose (unlines (map show badPairs)) (null badPairs)
