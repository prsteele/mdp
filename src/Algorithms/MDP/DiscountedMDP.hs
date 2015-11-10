module Algorithms.MDP.DiscountedMDP where

import qualified Data.Vector as V

import Algorithms.MDP.MDP
import Algorithms.MDP.Internal

mkDiscountedMDP :: (Eq b) =>
             [a]                -- ^ The state space
          -> [b]                -- ^ The action space
          -> Transitions a b t  -- ^ The transition probabilities
          -> Costs a b t        -- ^ The action-dependent costs
          -> ActionSet a b      -- ^ The state-dependent actions
          -> t                  -- ^ The discount factor
          -> MDP a b t          -- ^ The resulting DiscountedMDP
mkDiscountedMDP states actions trans cost actionSet discount =
  let
    _states      = V.fromList states
    _actions     = V.fromList actions
    _states'     = V.fromList (map State [0..length states - 1])
    _actions'    = V.fromList (map Action [0..length actions - 1])
    mkProbAS a s = V.fromList $ map (trans a s) states
    mkProbA a    = V.fromList $ map (mkProbAS a) states
    mkCostA a    = V.fromList $ map (cost a) states

    _costs = V.fromList $ map mkCostA actions
    _trans = V.fromList $ map mkProbA actions

    actionPairs   = zip (map Action [0..]) actions
    actionSet' st = V.fromList $ map fst $ filter ((`elem` acs) . snd) actionPairs
      where
        acs = actionSet st
    
    _actionSet = V.fromList $ map actionSet' states
  in
    MDP
    { _states    = _states
    , _actions   = _actions
    , _states'   = _states'
    , _actions'  = _actions'
    , _costs     = _costs
    , _trans     = _trans
    , _discount  = discount
    , _actionSet = _actionSet
    }
