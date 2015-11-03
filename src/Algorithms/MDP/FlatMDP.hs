module Algorithms.MDP.FlatMDP where

import Control.Monad.ST
import qualified Data.Vector as V

-- | A state in the MDP.
newtype State = State Int
              deriving (Eq, Show)

-- | An action in the MDP.
newtype Action = Action Int
              deriving (Eq, Show)

-- | A description of an MDP with an efficient representation.
--
-- Both the state space and action space are assumed to be a
-- sequential sequence of non-negative integers, and so we need only
-- store the maximum index for each. Transition probabilities and
-- costs are stored in Vectors.
--
-- The numeric types used in computations is 't'.
data FlatMDP a b t = FlatMDP
                     { _stateSpace     :: State
                     , _actionSpace    :: Action
                     , _probabilities  :: V.Vector t
                     , _costs          :: V.Vector t
                     , _discountFactor :: t
                     , _actionSet      :: V.Vector [Action]
                     , _fromOriginalS  :: a -> State
                     , _fromOriginalA  :: b -> Action
                     , _toOriginalS    :: State -> a
                     , _toOriginalA    :: Action -> b
                     }

-- | Get the probability of transitioning to state t from state s when
-- taking action a.
getProbability mdp (Action a) (State s) (State t) =
  let
    State nStates = _stateSpace mdp
    index = a * (nStates * nStates) + s * nStates + t
  in 
    (_probabilities mdp) V.! index

getCost mdp (Action a) (State s) =
  let
    State nStates = _stateSpace mdp
    index = a * (nStates) + s
  in 
    (_costs mdp) V.! index

mkFlatMDP :: (Eq a, Eq b) => [a] -> [b] -> (b -> a -> a -> t) -> (b -> a -> t) -> (a -> [b]) -> t -> FlatMDP a b t
mkFlatMDP states actions trans cost actionSet discount =
  let
    nStates  = length states
    nActions = length actions
    states'  = map State  [0..nStates - 1]
    actions' = map Action [0..nActions - 1]
    
    statePairs  = zip states  states'
    actionPairs = zip actions actions'
    revState  = zip (map State  [0..]) states
    revAction = zip (map Action [0..]) actions
    
    _fromOriginalS s  = case lookup s  statePairs  of Just s'  -> s'
    _fromOriginalA ac = case lookup ac actionPairs of Just ac' -> ac'
    _toOriginalS s'  = case lookup s'  revState of Just s  -> s
    _toOriginalA ac' = case lookup ac' revAction of Just ac -> ac

    -- Create a transition probability vector consistent with
    -- 'getProbability'
    trans' = V.fromList [trans a s t | a <- actions, s <- states, t <- states]

    -- Create a costs vector consistent with `getCost`
    cost' = V.fromList [cost a s | a <- actions, s <- states]
    
    actionSet' = V.fromList $ map (map (_fromOriginalA)) $ map (actionSet . _toOriginalS) states'

  in FlatMDP
     { _stateSpace     = State nStates
     , _actionSpace    = Action nActions
     , _probabilities  = trans'
     , _costs          = cost'
     , _discountFactor = discount
     , _actionSet      = actionSet'
     , _toOriginalS    = _toOriginalS
     , _toOriginalA    = _toOriginalA
     , _fromOriginalS  = _fromOriginalS
     , _fromOriginalA  = _fromOriginalA
     }

-- | A CostFunction is a mapping from states to the payoff in that
-- state.
type CostFunction t = V.Vector t
