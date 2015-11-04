module Algorithms.MDP.FlatMDP where

import Control.Monad.ST
import qualified Data.Vector as V

import Algorithms.MDP.MDP (CostFunction)

-- | A state in the MDP.
newtype State = State Int
              deriving (Eq, Show)

-- | An action in the MDP.
newtype Action = Action Int
              deriving (Eq, Show)

-- | A cost function is a list of (State, Action, cost) triples
-- describing the cost of being in the given state and the action
-- taken to achieve that cost.
type FlatCostFunction a b t = V.Vector (a, b, t)

type FlatCostFunctionBounds a b t = (FlatCostFunction a b t, t, t)

-- | An efficient representation of an MDP.
--
-- We pack all transition probabilities and costs into vectors.
data FlatMDP a b t = FlatMDP
                     { __states  :: V.Vector a
                     , __actions :: V.Vector b
                     , __states' :: V.Vector State
                     , __actions' :: V.Vector Action
                     , __costs   :: V.Vector (V.Vector t)
                     , __trans   :: V.Vector (V.Vector (V.Vector t))
                     , __discount :: t
                     , __actionSet :: V.Vector (V.Vector Action)
                     }

mkFlatMDP :: (Eq b) =>
             [a] -> [b] -> (b -> a -> a -> t) -> (b -> a -> t) -> (a -> [b]) -> t -> FlatMDP a b t
mkFlatMDP states actions trans cost actionSet discount =
  let
    __states = V.fromList states
    __actions = V.fromList actions
    __states' = V.fromList (map State [0..length states - 1])
    __actions' = V.fromList (map Action [0..length actions - 1])
    mkProbAS a s = V.fromList $ map (trans a s) states
    mkProbA a = V.fromList $ map (mkProbAS a) states
    mkCostA a = V.fromList $ map (cost a) states

    __costs = V.fromList $ map mkCostA actions
    __trans = V.fromList $ map mkProbA actions

    actionPairs = zip (map Action [0..]) actions
    actionSet' st = V.fromList $ map fst $ filter ((`elem` acs) . snd) actionPairs
      where
        acs = actionSet st
    
    __actionSet = V.fromList $ map actionSet' states
  in
    FlatMDP
    { __states = __states
    , __actions = __actions
    , __states' = __states'
    , __actions' = __actions'
    , __costs = __costs
    , __trans = __trans
    , __discount = discount
    , __actionSet = __actionSet
    }
