module Algorithms.MDP.Examples.LineClient where

import Text.Printf
import Data.List

import Algorithms.MDP
import Algorithms.MDP.UndiscountedMDP

data Action = Depart | Stay
            deriving (Show, Ord, Eq)

type State = (Int, Double)

factorial = fromIntegral . (factorial' 1)
  where
    factorial' result 0 = result
    factorial' result i = factorial' (result * i) (i - 1)

poisson :: Double -> Int -> Double
poisson r i = exp (- r) * (r ^ i) / (factorial i)

-- | Make an instance of the single-client, single-depot problem on
-- the line. We truncate the number of arrivals that can occur before
-- a delivery, and allow only a discrete set of distances on the line.
mkInstance maxWaiting numDistances arrivalRate serviceTime = let

  distances :: [Int]
  distances = [1..numDistances]

  distance :: Int -> Double
  distance d = (fromIntegral d) / (fromIntegral numDistances) * serviceTime

  states = [(i, d) | i <- [0..maxWaiting], d <- distances]

  -- | Given the current maximum distance d, what is the probability
  -- of moving to a new maximum distance d' when Staying?
  distProb :: Int -> Int -> Double
  distProb d d' = if d <= d'
                  then if d == d'
                       then (fromIntegral d) / (fromIntegral numDistances)
                       else 1 / (fromIntegral numDistances)
                  else 0

  expectedDistance = sum (map distance distances) / (fromIntegral numDistances)

  f d = poisson (2 * d * arrivalRate)

  tau = min (1 / arrivalRate) (2 * serviceTime)

  controls = [Stay, Depart]

  -- The normalizing constant for truncating the state space
  norm d = sum [f (distance d) s | (s, d', j) <- states, j == True, d' == d] 

  trans Stay (i, d) (i', d')
    | i == i' && d == i' = 1 - tau * (nu Stay)
    | i + 1 == i'        = tau * (nu Stay) * (distProb d d')
    | otherwise          = 0

  trans Depart (i, d) (i', d')
    | i == i' && d == d' = 1 - tau * (nu Depart) + tau * (nu Depart) * 

  transition Stay (i, d, j) (i', d', j') =
    if (i, d, j) == (i', d', j')
    then 1 - tau * arrivalRate
    else if (i + 1, j) == (i', j')
         then (distProb d d') * tau * arrivalRate
         else 0
  transition Depart (i, d, j) (i', d', j') =
    if (i, d, j) == (i', d', j')
    then 1 - tau / (2 * serviceTime)
    else if j /= j'
         then (distProb d d') * (tau / (2 * serviceTime) * (f (distance d) i') / (norm d))
         else 0

  costs Stay (i, d, j)   = (fromIntegral i) + expectedDistance * arrivalRate
  costs Depart (i, d, j) = arrivalRate * (serviceTime + expectedDistance)

  actions (i, d, j) = if i == maxWaiting
                      then [Depart]
                      else [Stay, Depart]
  
  in mkUndiscountedMDP states controls transition costs actions

-- takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
-- takeWhile2 _ [] = []
-- takeWhile2 p as = map fst $ takeWhile (uncurry p) (zip as (tail as))

-- -- | Find a threshold value, or an inversion.
-- --
-- -- A threshold value is a State (i, _) such that we always choose
-- -- Depart if we are in (i', _) for i' >= i, and always Stay otherwise.
-- --
-- -- An inversion is a pair of states (i, _) and (j, _) with i < j where
-- -- we choose Depart at (i, _) and Stay at (j, _).
-- thresholdOrInversion mdp (cf, _, _, _ ) =
--   let
--     stateSpace         = sort (_states mdp)
--     distances          = sort . nub $ map (\(_, d, _ ) -> d) stateSpace
--     choices            = map (fst . cf) stateSpace
--     pairs              = zip stateSpace choices
--     pairsForDistance d = filter (\((_, d', _ ), _ ) -> d' == d) pairs
--     firstDeparture d   = find ((== Depart) . snd) (pairsForDistance d)
--     remaining d s      = dropWhile (/= s) (pairsForDistance d)
--     firstStay d s      = find ((== Stay) . snd) (remaining d s)

--     tOI' d = case firstDeparture d
--              of Nothing -> "-"
--                 Just s  -> case firstStay d s
--                            of Nothing -> (\((s, _, _ ), _) -> show s) s
--                               Just t  -> show (s, t)
--   in unwords $ map tOI' distances
