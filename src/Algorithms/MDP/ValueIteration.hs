module Algorithms.MDP.ValueIteration where

import qualified Data.Vector as V

import Algorithms.MDP

-- | Compute the inner product between two vectors.
inner :: (Num t) => V.Vector t -> V.Vector t -> t
inner u v = V.sum (V.zipWith (*) u v)

-- | Compute an infinite sequence of estimates of cost functions
-- converging to the true cost function.
--
-- This method should only be used on discounted MDPs (e.g. an MDP
-- with a discount factor less than one).
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

-- | Computes the next estimate of the cost function.
valueIterate :: (Ord t, Num t) => 
                MDP a b t -- ^ The DiscountedMDP we are solving
             -> CF a b t  -- ^ The current cost function estimate
             -> CF a b t  -- ^ The next cost function estimate
valueIterate mdp cf = V.imap (choiceFor mdp cf) (_states mdp)

-- | Finds the action that minimizes the one-step payoff using the
-- given cost function.
choiceFor :: (Ord t, Num t) =>
             MDP a b t -- ^ The MDP we are solving
          -> CF a b t  -- ^ The current cost function
          -> Int       -- ^ The state for which we choose an action
          -> a         -- ^ The state for which we choose an action
          -> (a, b, t) -- ^ The choice of action and associated cost
choiceFor mdp cf sIndex s =
  let

    actions = V.fromList [(_actions mdp) V.! ac' | ac' <- V.toList ((_actionSet mdp) V.! sIndex)]
    
    cmp (_, x) (_, y) = compare x y
    costs = V.map (costForAction mdp cf sIndex) (_actionSet mdp V.! sIndex)
    pairs = V.zip actions costs
    (ac, c) = V.minimumBy cmp pairs
  in
    (s, ac, c)

-- | Computes the cost implied by choosing an action in the given
-- state.
costForAction :: (Num t) => 
                 MDP a b t -- ^ The MDP we are solving.
              -> CF a b t  -- ^ The current cost function.
              -> Int       -- ^ The index of the state.
              -> Int       -- ^ The index of the action.
              -> t         -- ^ The estimated cost.
costForAction mdp cf sIndex ac =
  let
    alpha = _discount mdp
    fixedCost = (_costs mdp) V.! ac V.! sIndex
    transCost = inner (_trans mdp V.! ac V.! sIndex) (V.map (\(_, _, c) -> c) cf)
  in
    fixedCost + alpha * transCost

-- | An implementation of value iteration that computes monotonic
-- error bounds.
--
-- The error bounds provided at each iteration are additive in each
-- state. That is, given a cost estimate 'c' for a given state and
-- lower and upper bounds 'lb' and 'ub', the true cost is guaranteed
-- to be in the interval [c + lb, c + ub].
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

-- | Computes the next estimate of the cost function and associated
-- error bounds.
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

-- | Relative value iteration for undiscounted MDPs.
undiscountedRelativeValueIteration :: (Ord t, Fractional t, Read t) =>
                                      MDP a b t 
                                   -> [CFBounds a b t]
undiscountedRelativeValueIteration mdp =
  let
    states = _states mdp
    actions = _actions mdp

    trans  = _trans mdp
    update s v = V.imap (\i z -> tau * z + if i == s then (1 - tau) else 0) v

    trans' = V.map (\vv -> V.imap (\s v -> update s v) vv) trans

    tau = 0.5
    mdp' = mdp {_trans = trans'}
    zeroV = V.map (\s -> (s, V.head actions, 0)) states
    zero = CFBounds zeroV (read "-Infinity") (read "Infinity")
    distinguished = 0
  in
    iterate (undiscountedRVI mdp' distinguished) zero

-- | Performs a single iterate of relative value iteration for the
-- undiscounted problem.
undiscountedRVI :: (Ord t, Fractional t) =>
                   MDP a b t
                -> Int
                -> CFBounds a b t
                -> CFBounds a b t
undiscountedRVI mdp distinguished (CFBounds h _ _) =
  let
    th = valueIterate mdp h
    (_, _, distinguishedCost) = th V.! distinguished

    th' = V.map (\(s, ac, z) -> (s, ac, z - distinguishedCost)) th

    (lb, ub) = (V.minimum diffs, V.maximum diffs)
      where
        diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b) th h

  in
    CFBounds th' lb ub
