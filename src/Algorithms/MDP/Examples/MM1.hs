-- | We model an M/M/1 queue, i.e. a single-server queue with Poisson
-- arrivals and service times.
--
-- See "Stochastic Dynamic Programming and the Control of Queueing
-- Systems", Linn I. Sennot,, p. 242 for details.
module Algorithms.MDP.Examples.MM1 where

import Algorithms.MDP.CTMDP

data Scenario = Scenario
                { _arrivalRate  :: Double
                , _serviceRates :: [Double]
                , _serviceCosts :: [Double]
                , _holdingCosts :: Int -> Double
                , _maxWaiting   :: Int
                }

newtype State = State Int
              deriving (Show, Eq)
                       
data Action = NullAction
            | Action Int
            deriving (Show, Eq)

mkInstance :: Scenario -> CTMDP State Action Double
mkInstance scenario =
  let
    -- (State i) represents i customers waiting in the queue.
    states  = map State  [0..(_maxWaiting scenario)]

    -- (Action i) represents serving a customer with the ith service
    -- profile, while the NullAction represents what we do in the
    -- empty queue (wait).
    actions = NullAction : map Action [0..length (_serviceRates scenario) - 1]

    -- All actions but the null action have an associated cost
    rateCost (Action ac) (State i) = hc + sc
      where
        hc = _holdingCosts scenario i
        sc = _serviceCosts scenario !! ac
    rateCost NullAction  _         = 0

    -- There can always be an arrival, and if we don't take the null
    -- action there can be a departure.
    rates (Action ac) _  = _arrivalRate scenario + _serviceRates scenario !! ac
    rates NullAction  _  = _arrivalRate scenario

    -- There are no fixed costs.
    fixedCost _ _= 0

    -- We can only take the null action in state 0, and can take any
    -- other action in all other states.
    actionSet (State 0) = [NullAction]
    actionSet _         = (tail actions)

    -- If we take the null action, we wait for an arrival. Otherwise,
    -- we can increase or decrease the length of the queue by 1.
    --
    -- Note that since we cannot transition about the maximum state,
    -- we instead allow a self-transition.
    trans NullAction  (State 0) (State 1) = 1
    trans NullAction  _         _         = 0
    trans (Action ac) (State i) (State j) 
      | j == i + 1          = lambda / (lambda + a)
      | j == i && i == maxN = lambda / (lambda + a)
      | j == i - 1          = a / (lambda + a)
      | otherwise           = 0
      where
        maxN = _maxWaiting scenario
        lambda = _arrivalRate scenario
        a = _serviceRates scenario !! ac
  in
    mkCTMDP states actions trans rates fixedCost rateCost actionSet 1.0

scenario1 = Scenario
            { _arrivalRate  = 3
            , _serviceRates = [2, 4, 8]
            , _serviceCosts = [9, 13, 21]
            , _holdingCosts = \i -> fromIntegral i--const 5
            , _maxWaiting   = 48
            }

scenario2 = Scenario
            { _arrivalRate  = 2.0
            , _serviceRates = [1, 4, 7]
            , _serviceCosts = [1, 50, 500]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            }

scenario3 = Scenario
            { _arrivalRate  = 2.0
            , _serviceRates = [1, 4, 7]
            , _serviceCosts = [1, 50, 150]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            }