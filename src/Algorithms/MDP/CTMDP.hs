-- | A continuous-time Markov decision chain is a Markov decision
-- process (CTMDP) where an exponential amount of time is spent at
-- each state.
module Algorithms.MDP.CTMDP where

import Algorithms.MDP.MDP hiding (MDP)
import qualified Algorithms.MDP.MDP as MDP (MDP)


-- | A continuous-time Markov decision chain with states of type @a@
-- and actions of type @b@.
--
-- A continous-time Markov decision chain contains a state space, a
-- set of actions that can be taken at each state, action-dependent
-- transition probabilities between states, state- and
-- action-dependent costs incurred once as an action is chosen, state-
-- and action-dependent costs that are incurred at a continuous rate
-- while in that state, and action-dependent transition rates from
-- each state. We assume that all transition rates are the rate
-- parameter of an exponential distribution.
--
-- Elements of type @a@ are in the state space, while elements of type
-- @b@ are actions or controls.
--
-- The 'Neighbors' function is used to speed up computations, and can
-- be computed automatically using the 'mkCTMDP' function.
data CTMDP a b t = CTMDP
                   { _states         :: [a]
                   , _actions        :: [b]
                   , _transitions    :: Transition a b t
                   , _rateCosts      :: StateCost a b t
                   , _fixedCosts     :: StateCost a b t
                   , _rates          :: TransitionRate a b t
                   , _actionSet      :: ActionSet a b
                   , _neighbors      :: Neighbors a b t
                   , _discount       :: t
                   }

-- | A transition rate function describes the action-dependent rate at
-- which transitions out of a state occur. The rates are the rate
-- parameter of an exponential random variable.
type TransitionRate a b t = b -> a -> t

-- | Constructs a new CTMDP.
--
-- The 'Neighbor' function is computed automatically.
mkCTMDP :: (Ord a, Ord b, Ord t, Num t) =>
  [a]                       -- ^ The state space
  -> [b]                    -- ^ The action space
  -> (Transition a b t)     -- ^ The transition probabilities
  -> (StateCost a b t)      -- ^ The rate cost of each state
  -> (StateCost a b t)      -- ^ The fixed cost of each state
  -> (ActionSet a b)        -- ^ The actions available at each state
  -> (TransitionRate a b t) -- ^ The rates at which transitions occur
  -> t                      -- ^ The discount factor
  -> CTMDP a b t            -- ^ The resulting CTMDP
mkCTMDP states actions trans rateCost fixedCost actionSet transRate discount =
  let
    neighbors = mkNeighbors states trans actionSet

    in CTMDP { _states         = states
             , _actions        = actions
             , _transitions    = trans
             , _rateCosts      = rateCost
             , _fixedCosts     = fixedCost
             , _rates          = transRate
             , _actionSet      = actionSet
             , _neighbors      = neighbors
             , _discount       = discount
             }

-- | Convert a CTMDP to a MDP via uniformization.
--
-- We convert the CTMDP to an equivalent MDP by introducing artifical
-- self-transitions to allow slow transitions to occur at the the
-- fastest transition rate.
--
-- See Bertsekas p. 249.
uniformize :: (Ord a, Eq a, Ord b, Eq b, Ord t, Fractional t) => 
              CTMDP a b t -> MDP.MDP a b t
uniformize ctmdc = let
  states     = _states ctmdc
  actions    = _actions ctmdc
  trans      = _transitions ctmdc
  rateCosts  = _rateCosts ctmdc
  fixedCosts = _fixedCosts ctmdc
  rates      = _rates ctmdc
  actionSet  = _actionSet ctmdc
  discount   = _discount ctmdc

  c = maximum [ (1 - trans action s s) * rates action s
              | s <- states, action <- actionSet s]

  tau = 1 / c

  trans' action i j = if i == j
                      then 1 - coef * (1 - p)
                      else coef * p
    where
      coef = tau * rates action i
      p    = trans action i j

  lumpedCost action i = fixedCosts action i + (rateCosts action i) / (rates action i)

  costs' action i = (lumpedCost action i) * (discount + rates action i) / (discount + 1 / tau)

  discount' = (1 / tau) / (1 / tau + discount)

  in mkMDP states actions trans' costs' actionSet discount'
