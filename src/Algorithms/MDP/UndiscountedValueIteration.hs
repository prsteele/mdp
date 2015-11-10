module Algorithms.MDP.UndiscountedValueIteration where

import qualified Data.Vector as V

import Algorithms.MDP.MDP
import Algorithms.MDP.Internal
import qualified Algorithms.MDP.DiscountedValueIteration as D

relativeValueIterate :: (Ord t, Fractional t) =>
                        MDP a b t
                     -> t
                     -> State
                     -> CFBounds a b t
                     -> CFBounds a b t
relativeValueIterate mdp tau (State distinguished) (CFBounds h _ _) =
  let
    th = D.valueIterate mdp h
    (_, _, distinguishedCost) = th V.! distinguished
    h' = V.map (\(s, ac, z) -> (s, ac, z - distinguishedCost)) th

    (lb, ub) = (V.minimum diffs, V.maximum diffs)
      where
        diffs = V.zipWith (\(_, _, a) (_, _, b) -> a - b) th h
  in
    CFBounds h' lb ub

relativeValueIteration :: (Ord t, Fractional t, Read t) =>
                          MDP a b t 
                       -> [CFBounds a b t]
relativeValueIteration mdp =
  let
    states' = _states' mdp
    states = _states mdp
    actions = _actions mdp

    selfTransProb a i = trans V.! a V.! i V.! i
    selfTransProb' a i = tau * (selfTransProb a i) + (1 - tau)
    trans  = _trans mdp
    update a s v = V.imap (\i z -> tau * z + if i == s then (1 - tau) else 0) v

    trans' = V.imap (\a vv -> V.imap (\s v -> update a s v) vv) trans

    tau = 0.5
    mdp' = mdp {_trans = trans'}
    zeroV = V.map (\s -> (s, V.head actions, 0)) states
    zero = CFBounds zeroV (read "-Infinity") (read "Infinity")
    distinguished = V.head states'
  in
    iterate (relativeValueIterate mdp' tau distinguished) zero
