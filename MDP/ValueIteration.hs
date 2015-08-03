-- | This module defines functions to solve an MDP via value iteration.
module MDP.ValueIteration where

import qualified Data.Map as Map
import Data.List (minimumBy)

import MDP.MDP

-- | Given an MDP and a CostFunction, compute an improved estimate of
-- the cost function.
--
-- The fixed point of this iteration is the optimal solution to the
-- problem, but this may not be achieved in finite time.
valueIterate :: (Ord a, Ord b) => MDP a b -> CostFunction a b -> CostFunction a b
valueIterate mdp cf =
  let
    cost = unCosts mdp
    stateSpace = unStates mdp
    actionSet = unActionSet mdp
    alpha = unDiscountFactor mdp
    neighbors = unNeighbors mdp

    costFor action state =
      let
        transitionCost = sum [p * (snd . cf) s' | (s', p) <- neighbors action state]
        fixedCost = cost action state
      in fixedCost + alpha * transitionCost
    
    choiceFor s = minimumBy cmp ([(ac, costFor ac s) | ac <- actionSet s])
      where
        cmp (_, c) (_, c') = compare c c'

    -- Find the best choice in each state
    choices = map choiceFor stateSpace
  in ((Map.fromList $ zip stateSpace choices) Map.!)

-- | Runs value iteration on the given problem.
--
-- Returns an infinite sequence of 'CostFunction's that approach an
-- optimal 'CostFunction'.
valueIteration :: (Ord a, Ord b) => MDP a b -> [CostFunction a b]
valueIteration mdp = iterate (valueIterate mdp) (mkZeroCostFunction mdp)

relativeValueIterate ::
  (Ord a, Ord b) =>
  MDP a b             -- ^ The MDP to solve
  -> a                -- ^ A distinguished state
  -> DCFAndBounds a b -- ^ The current estimate of the differential
                      -- cost vector
  -> DCFAndBounds a b -- ^ The resulting bounds and differential cost
                      -- vector
relativeValueIterate mdp distinguished (cf, _, _, _ ) =
  let
    stateSpace = unStates mdp
    
    cf'    = valueIterate mdp cf
    cf'' s = (action, cost - cost')
      where
        (action, cost)  = cf' s
        (_     , cost') = cf' distinguished

    bounds = (f minimum, f maximum)
      where
        f sense = sense $ map (\s -> snd (cf' s) - snd (cf s)) stateSpace
    (lb, ub) = bounds

    opt = snd (cf' distinguished)
        
  in (cf'', opt, lb, ub)

-- | Relative value iteration is used to compute long-run average-cost
-- optimal policies.
--
-- In particular, relative value iteration computes a long-run average
-- cost and an associated differential cost vector. The convergence of
-- this method depends on stationary policies being unichain.
--
-- This method has a tuning parameter tau, which must be in (0, 1). If
-- you are unsure how to utilize this parameter, a choice of 0.5 is
-- acceptable.
--
-- This method also utilizes a distinguished state @s@. Given this state, 
relativeValueIteration
  :: (Ord a, Eq a, Ord b, Eq b) =>
     MDP a b               -- ^ The MDP to solve
     -> a                  -- ^ A distinguished state
     -> Double             -- ^ The value of tau
     -> [DCFAndBounds a b] -- ^ The resulting sequence of bounds and
                           -- differential cost vectors
relativeValueIteration mdp distinguished tau =
  let
    trans = unTransition mdp
    trans' action state state' = if state == state'
                                 then tau * p + (1 - tau)
                                 else tau * p
      where
        p = trans action state state'

    mdp' = mdp { unTransition = trans' }
    zero = (mkZeroCostFunction mdp', 0, 0, 0)
    
  in tail $ iterate (relativeValueIterate mdp' distinguished) zero

-- | A differential cost vector is a function that maps each state to
-- the difference in cost between that state and some distinguished
-- state and the action needed to achieve that cost.
type DifferentialCostVector a b = CostFunction a b

-- | A differential cost vector paired with an estimate, lower-bound,
-- and upper-bound on the optimal average cost per stage.
type DCFAndBounds a b =
  (DifferentialCostVector a b, Double, Double, Double)

gap :: DCFAndBounds a b -> Double
gap (_, _, lb, ub) = ub - lb

optimalCost :: DCFAndBounds a b -> Double
optimalCost (_, opt, _, _ ) = opt
