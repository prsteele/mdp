module Algorithms.MDP.UndiscountedValueIteration where

import Debug.Trace

import qualified Data.Vector as V
import Algorithms.MDP.MDP
import qualified Algorithms.MDP.DiscountedValueIteration as D-- (relativeValueIterate)

relativeValueIterate :: (Ord t, Fractional t, Show t, Show a, Show b) =>
                        MDP a b t
                     -> t
                     -> State
                     -> CFBounds a b t
                     -> CFBounds a b t
relativeValueIterate mdp tau (State distinguished) (CFBounds h _ _) =
  let
    -- th = D.valueIterate mdp h
    -- (_, _, distinguishedCost) = th V.! distinguished
    -- (_, _, distinguishedCost') = h V.! distinguished

    -- merge (_, _, z) (s, ac, z') = (s, ac, (1 - tau) * z + z' - distinguishedCost)

    -- (lb, ub) = (V.minimum diffs, V.maximum diffs)
    --   where
    --     diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b + distinguishedCost) th h
    --     --diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b + distinguishedCost) (D.valueIterate mdp h') th

    -- h' = V.zipWith merge h th
    th = D.valueIterate mdp h
    (_, _, distinguishedCost) = th V.! distinguished
    h' = V.map (\(s, ac, z) -> (s, ac, z - distinguishedCost)) th

    (lb, ub) = (V.minimum diffs, V.maximum diffs)
      where
        diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b) th h
  in
    CFBounds h' lb ub

-- relativeValueIteration :: (Show a, Show b, Show t, Ord t, Fractional t) =>
--                           MDP a b t 
--                        -> [CFBounds a b t]
relativeValueIteration mdp =
  let
    states' = _states' mdp
    states = _states mdp
    actions = _actions mdp

    selfTransProb a i = trans V.! a V.! i V.! i
    selfTransProb' a i = tau * (selfTransProb a i) + (1 - tau)
    trans  = _trans mdp
    update a s v = V.imap (\i z -> tau * z + if i == s then (1 - tau) else 0) v
    --update a s v = v V.// [(s, selfTransProb' a s)]

    trans' = V.imap (\a vv -> V.imap (\s v -> update a s v) vv) trans

    tau = 1.0
    mdp' = mdp { _discount = tau, _trans = trans'}
    zeroV = V.map (\s -> (s, V.head actions, 0)) states
    zero = CFBounds zeroV (read "-Infinity") (read "Infinity")
    distinguished = V.head states'
  in
    trace (show trans') $ iterate (relativeValueIterate mdp' tau distinguished) zero
