module Algorithms.MDP.ValueIteration where

import qualified Data.Vector as V

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

    actions = V.fromList [(_actions mdp) V.! ac' | Action ac' <- V.toList ((_actionSet mdp) V.! st)]
    
    cmp (_, x) (_, y) = compare x y
    costs = V.map (costForAction mdp cf (State st)) (_actionSet mdp V.! st)
    pairs = V.zip actions costs
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

relativeValueIteration :: (Read t, Ord t, Fractional t) => 
                          MDP a b t 
                       -> [CFBounds a b t]
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

undiscountedRVI :: (Ord t, Fractional t) =>
                                    MDP a b t
                                 -> State
                                 -> CFBounds a b t
                                 -> CFBounds a b t
undiscountedRVI mdp (State distinguished) (CFBounds h _ _) =
  let
    th = valueIterate mdp h
    (_, _, distinguishedCost) = th V.! distinguished
    h' = V.map (\(s, ac, z) -> (s, ac, z - distinguishedCost)) th

    (lb, ub) = (V.minimum diffs, V.maximum diffs)
      where
        diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b) th h
  in
    CFBounds h' lb ub

undiscountedRelativeValueIteration :: (Ord t, Fractional t, Read t) =>
                                      MDP a b t 
                                   -> [CFBounds a b t]
undiscountedRelativeValueIteration mdp =
  let
    states' = _states' mdp
    states = _states mdp
    actions = _actions mdp

    trans  = _trans mdp
    update s v = V.imap (\i z -> tau * z + if i == s then (1 - tau) else 0) v

    trans' = V.map (\vv -> V.imap (\s v -> update s v) vv) trans

    tau = 0.5
    mdp' = mdp {_trans = trans'}
    zeroV = V.map (\s -> (s, V.head actions, 0)) states
    zero = CFBounds zeroV (read "-Infinity") (read "Infinity")
    distinguished = V.head states'
  in
    iterate (undiscountedRVI mdp' distinguished) zero
