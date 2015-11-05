module Algorithms.MDP.DiscountedMDP where

import Control.Monad.ST
import qualified Data.Vector as V

-- | A type representing an action- and state-dependent probablity
-- vector.
type Transitions a b t = b -> a -> a -> t

-- | A type representing an action- and state-dependent cost.
type Costs a b t = b -> a -> t

-- | A type representing the allowed actions in a state.
type ActionSet a b = a -> [b]

-- | A cost function is a list of (State, Action, cost) triples
-- describing the cost of being in the given state and the action
-- taken to achieve that cost.
type DiscountedCF a b t = V.Vector (a, b, t)

type DiscountedCFBounds a b t = (DiscountedCF a b t, t, t)

mkDiscountedMDP :: (Eq b) =>
             [a]               -- ^ The state space
          -> [b]               -- ^ The action space
          -> Transitions a b t -- ^ The transition probabilities
          -> Costs a b t       -- ^ The action-dependent costs
          -> ActionSet a b     -- ^ The state-dependent actions
          -> t                 -- ^ The discount factor
          -> DiscountedMDP a b t     -- ^ The resulting DiscountedMDP
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
    DiscountedMDP
    { _states    = _states
    , _actions   = _actions
    , _states'   = _states'
    , _actions'  = _actions'
    , _costs     = _costs
    , _trans     = _trans
    , _discount  = discount
    , _actionSet = _actionSet
    }

-- | A state in the MDP.
newtype State = State Int
              deriving (Eq, Show)

-- | An action in the MDP.
newtype Action = Action Int
              deriving (Eq, Show)

-- | A discounted Markov decision process.
--
-- An MDP consists of a state space, an action space, state- and
-- action-dependent costs, and state- and action-dependent transition
-- probabilities. The goal is to compute a policy -- a mapping from
-- states to actions -- which minimizes the total discounted cost of
-- the problem, assuming a given discount factor in the range (0, 1).
data DiscountedMDP a b t = DiscountedMDP
                           { _states    :: V.Vector a
                           , _actions   :: V.Vector b
                           , _states'   :: V.Vector State
                           , _actions'  :: V.Vector Action
                           , _costs     :: V.Vector (V.Vector t)
                           , _trans     :: V.Vector (V.Vector (V.Vector t))
                           , _discount  :: t
                           , _actionSet :: V.Vector (V.Vector Action)
                           }
