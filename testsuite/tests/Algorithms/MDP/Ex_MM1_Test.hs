{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | Tests for the problems discussed in section 10.4 of "Stochastic
-- Dynamic Programming and the Control of Queueing Systems", Linn
-- Sennot.
module Algorithms.MDP.Ex_MM1_Test where

import Test.Framework

import Algorithms.MDP.MDP
import Algorithms.MDP.CTMDP
import Algorithms.MDP.ValueIteration
import Algorithms.MDP.Examples.MM1

costOf (CFBounds _ lb ub) = (lb + ub) / 2

gap :: (Num t) => CFBounds a b t -> t
gap (CFBounds _ lb ub) = ub - lb

solution :: Double -> MDP State Action Double -> CFBounds State Action Double
solution tol =
  head . dropWhile ((> tol) . gap) . undiscountedRelativeValueIteration

test_scenario1Cost = assertBoolVerbose msg (abs (c - cc) < 1e3) 
  where
    ctmdp = uniformize (mkInstance scenario1)
    sol   = solution (1e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario1
    msg   = unwords [show c, "/=", show cc]

test_scenario2Cost = assertBoolVerbose msg (abs (c - cc) < 2e3) 
  where
    ctmdp = uniformize (mkInstance scenario2)
    sol   = solution (2e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario2
    msg   = unwords [show c, "/=", show cc]

test_scenario3Cost = assertBoolVerbose msg (abs (c - cc) < 3e3) 
  where
    ctmdp = uniformize (mkInstance scenario3)
    sol   = solution (3e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario3
    msg   = unwords [show c, "/=", show cc]

test_scenario4Cost = assertBoolVerbose msg (abs (c - cc) < 4e3) 
  where
    ctmdp = uniformize (mkInstance scenario4)
    sol   = solution (4e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario4
    msg   = unwords [show c, "/=", show cc]

test_scenario5Cost = assertBoolVerbose msg (abs (c - cc) < 5e3) 
  where
    ctmdp = uniformize (mkInstance scenario5)
    sol   = solution (5e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario5
    msg   = unwords [show c, "/=", show cc]

test_scenario6Cost = assertBoolVerbose msg (abs (c - cc) < 6e3) 
  where
    ctmdp = uniformize (mkInstance scenario6)
    sol   = solution (6e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario6
    msg   = unwords [show c, "/=", show cc]

test_scenario7Cost = assertBoolVerbose msg (abs (c - cc) < 7e3) 
  where
    ctmdp = uniformize (mkInstance scenario7)
    sol   = solution (7e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario7
    msg   = unwords [show c, "/=", show cc]

test_scenario8Cost = assertBoolVerbose msg (abs (c - cc) < 8e3) 
  where
    ctmdp = uniformize (mkInstance scenario8)
    sol   = solution (8e-4) ctmdp
    c     = costOf sol
    cc    = _scenarioCost scenario8
    msg   = unwords [show c, "/=", show cc]
