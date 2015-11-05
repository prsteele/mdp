module Algorithms.MDP.UndiscountedValueIteration where

import Debug.Trace

import qualified Data.Vector as V
import Algorithms.MDP.MDP
import Algorithms.MDP.DiscountedValueIteration (valueIterate)

relativeValueIterate :: (Ord t, Fractional t) =>
                        MDP a b t
                     -> t
                     -> State
                     -> DifferentialCF a b t
                     -> DifferentialCF a b t
relativeValueIterate mdp tau (State distinguished) (DifferentialCF _ h) =
  let
    h' = valueIterate mdp h
    (_, _, distinguishedCost) = h' V.! distinguished

    merge (s, ac, z) (_, _, z') = (s, ac, (1 - tau) * z + z' - distinguishedCost)
    
    h'' = V.zipWith merge h h'
  in
    DifferentialCF distinguishedCost h''

relativeValueIteration :: (Show a, Show b, Show t, Ord t, Fractional t) =>
                          MDP a b t 
                       -> [DifferentialCF a b t]
relativeValueIteration mdp =
  let
    states' = _states' mdp
    states = _states mdp
    actions = _actions mdp
    tau = 0.5
    mdp' = mdp { _discount = tau }
    zeroV = V.map (\s -> (s, V.head actions, 0)) states
    zero = DifferentialCF 0 zeroV
    distinguished = V.head states'
  in
    trace (show zeroV) $ iterate (relativeValueIterate mdp' tau distinguished) zero
