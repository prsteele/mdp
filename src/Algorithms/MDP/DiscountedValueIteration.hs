module Algorithms.MDP.DiscountedValueIteration where

import qualified Data.Vector as V
import Data.List (minimumBy)

import Algorithms.MDP.MDP
import Algorithms.MDP.Internal

inner :: (Num t) => V.Vector t -> V.Vector t -> t
inner u v = V.sum (V.zipWith (*) u v)

valueIteration :: (Ord t, Num t) => 
                  MDP a b t   -- ^ The DiscountedMDP to solve
               -> [CF a b t]  -- ^ An converging sequence of
                              -- cost functions
valueIteration mdp =
  let
    states = _states mdp
    actions = _actions mdp

    zero = V.map (\s -> (s, V.head actions, 0)) states
  in
    iterate (valueIterate mdp) zero

valueIterate :: (Ord t, Num t) => 
                MDP a b t -- ^ The DiscountedMDP we are solving
             -> CF a b t  -- ^ The current cost function estimate
             -> CF a b t  -- ^ The next cost function estimate
valueIterate mdp cf = V.zipWith (choiceFor mdp cf) (_states' mdp) (_states mdp)

-- | Finds the action that minimizes the one-step payoff using the
-- given cost function.
choiceFor :: (Ord t, Num t) =>
             MDP a b t -- ^ The DiscountedMDP we are solving
          -> CF a b t  -- ^ The current cost function
          -> State     -- ^ The state for which we choose an action
          -> a         -- ^ The state for which we choose an action
          -> (a, b, t) -- ^ The choice of action and associated cost
choiceFor mdp cf (State st) s =
  let
    cmp (_, x) (_, y) = compare x y
    costs = V.map (costForAction mdp cf (State st)) (_actionSet mdp V.! st)
    pairs = V.zip (_actions mdp) costs
    (ac, c) = V.minimumBy cmp pairs
  in
    (s, ac, c)

costForAction :: (Num t) => MDP a b t -> CF a b t -> State -> Action -> t
costForAction mdp cf (State st) (Action ac) =
  let
    alpha = _discount mdp
    fixedCost = (_costs mdp) V.! ac V.! st
    transCost = inner (_trans mdp V.! ac V.! st) (V.map (\(_, _, c) -> c) cf)
  in
    fixedCost + alpha * transCost

relativeValueIteration mdp =
  let
    states = _states mdp
    actions = _actions mdp

    zero = V.map (\s -> (s, V.head actions, 0)) states

    cf = CFBounds zero (read "-Infinity") (read "Infinity")
  in
    iterate (relativeValueIterate mdp) cf

relativeValueIterate :: (Ord t, Fractional t) => 
                        MDP a b t 
                        -> CFBounds a b t 
                        -> CFBounds a b t
relativeValueIterate mdp (CFBounds cf _ _) =
  let
    alpha = _discount mdp
    cf' = valueIterate mdp cf
    (lb, ub) = (V.minimum diffs, V.maximum diffs)
      where
        diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b) cf' cf
    scale = alpha / (1 - alpha)
  in 
    CFBounds
    { _CF = cf'
    , _lb = scale * lb
    , _ub = scale * ub
    }

