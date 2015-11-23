-- | We model an M/M/1 queue, i.e. a single-server queue with Poisson
-- arrivals and service times.
--
-- See "Stochastic Dynamic Programming and the Control of Queueing
-- Systems", Linn I. Sennot,, p. 242 for details.
module Algorithms.MDP.Examples.MM1 where

import qualified Algorithms.MDP.CTMDP as CTMDP

data Scenario = Scenario
                { _arrivalRate  :: Double
                , _serviceRates :: [Double]
                , _serviceCosts :: [Double]
                , _holdingCosts :: Int -> Double
                , _maxWaiting   :: Int
                , _scenarioCost :: Double
                }

newtype State = State Int
              deriving (Show, Eq)
                       
data Action = NullAction
            | Action Int
            deriving (Show, Eq)

mkInstance :: Scenario -> CTMDP.CTMDP State Action Double
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
    CTMDP.mkCTMDP states actions trans rates fixedCost rateCost actionSet 1.0

scenario1 :: Scenario
scenario1 = Scenario
            { _arrivalRate  = 3
            , _serviceRates = [2, 4, 8]
            , _serviceCosts = [9, 13, 21]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 48
            , _scenarioCost = 8.475
            }

scenario2 :: Scenario
scenario2 = Scenario
            { _arrivalRate  = 2.0
            , _serviceRates = [1, 4, 7]
            , _serviceCosts = [1, 50, 500]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 21.091
            }
            
scenario3 :: Scenario
scenario3 = Scenario
            { _arrivalRate  = 2.0
            , _serviceRates = [1, 4, 7]
            , _serviceCosts = [1, 50, 150]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 21.091
            }

scenario4 :: Scenario
scenario4 = Scenario
            { _arrivalRate  = 2.0
            , _serviceRates = [1, 4, 7]
            , _serviceCosts = [1, 50, 100]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 21.971
            }

scenario5 :: Scenario
scenario5 = Scenario
            { _arrivalRate  = 2.0
            , _serviceRates = [5.0, 5.5, 5.8]
            , _serviceCosts = [0, 10, 100]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 17.043
            }

scenario6 :: Scenario
scenario6 = Scenario
            { _arrivalRate  = 5.0
            , _serviceRates = [5.1, 5.3, 6.0]
            , _serviceCosts = [0, 10, 25]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 15.193
            }

scenario7 :: Scenario
scenario7 = Scenario
            { _arrivalRate  = 10.0
            , _serviceRates = [10.2, 10.6, 12]
            , _serviceCosts = [0, 10, 25]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 15.193
            }

scenario8 :: Scenario
scenario8 = Scenario
            { _arrivalRate  = 20.0
            , _serviceRates = [24, 27, 30]
            , _serviceCosts = [1, 1.5, 5.0]
            , _holdingCosts = \i -> fromIntegral i
            , _maxWaiting   = 84
            , _scenarioCost = 3.902
            }
