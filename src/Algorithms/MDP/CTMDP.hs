-- | A continuous-time Markov decision process (CTMDP) is an MDP where
-- transitions between states take a random amount of time. Each
-- transition time is assumed to be exponentially distributed with an
-- action- and state-dependent transition rate.
--
-- The record accessors of the 'CTMDP' type conflict with those of the
-- 'MDP' type, so either import only the 'mkCTMDP' and 'uniformize'
-- functions or import this module qualified.
module Algorithms.MDP.CTMDP
       ( CTMDP (..)
       , mkCTMDP
       , Rates
       , uniformize
       ) where

import qualified Data.Vector as V

import           Algorithms.MDP (MDP(MDP))
import           Algorithms.MDP hiding (MDP (..))

-- | A Continuous-time Markov decision process.
--
-- A CTMDP is a continuous-time analog of an MDP. In a CTMDP each
-- stage takes a variable amount of time. Each stage lasts an
-- expontially distributed amount of time characterized by a state-
-- and action-dependent rate parameter. Instead of simply having costs
-- associated with a state and an action, the costs of a CTMDP are
-- broken up into fixed and rate costs. Fixed costs are incured as an
-- action are chosen, while rate costs are paid for the duration of
-- the stage.
--
-- Here the type variable 'a' represents the type of the states, 'b'
-- represents the type of the actions, and 't' represents the numeric
-- type used in computations. Generally choosing 't' to be a Double is
-- fine, although there is no reason a higher-precision type cannot be
-- used.
--
-- This type should not be constructed directly; use the 'mkCTMDP'
-- constructor instead.
data CTMDP a b t = CTMDP
                   { _states     :: V.Vector a
                   , _actions    :: V.Vector b
                   , _fixedCosts :: V.Vector (V.Vector t)
                   , _rateCosts  :: V.Vector (V.Vector t)
                   , _rates      :: V.Vector (V.Vector t)
                   , _trans      :: V.Vector (V.Vector (V.Vector t))
                   , _discount   :: t
                   , _actionSet  :: V.Vector (V.Vector Int)
                   }

-- | A function mapping an action and a state to a transition rate.
type Rates a b t = b -> a -> t

-- | Create a CTMDP.
mkCTMDP :: (Eq b) =>
           [a]                -- ^ The state space
        -> [b]                -- ^ The action space
        -> Transitions a b t  -- ^ The transition probabilities
        -> Rates a b t        -- ^ The transition rates
        -> Costs a b t        -- ^ The action-dependent fixed costs
        -> Costs a b t        -- ^ The action-dependent rate costs
        -> ActionSet a b      -- ^ The state-dependent actions
        -> t                  -- ^ The discount factor in (0, 1]
        -> CTMDP a b t        -- ^ The resulting CTMDP
mkCTMDP states actions trans rates fixedCost rateCost actionSet discount =
  let
    _states      = V.fromList states
    _actions     = V.fromList actions
    _states'     = V.fromList [0..length states - 1]
    _actions'    = V.fromList [0..length actions - 1]

    mkCostVecFor cf ac = V.fromList $ map (cf ac) states
    _fixedCosts = V.fromList $ map (mkCostVecFor fixedCost) actions
    _rateCosts  = V.fromList $ map (mkCostVecFor rateCost)  actions

    mkProbAS a s = V.fromList $ map (trans a s) states
    mkProbA a    = V.fromList $ map (mkProbAS a) states
    _trans = V.fromList $ map mkProbA actions

    mkTransVec ac = V.fromList $ map (rates ac) states
    _rates = V.fromList $ map mkTransVec actions

    actionPairs   = zip [0..] actions
    actionSet' st = V.fromList $ map fst $ filter ((`elem` acs) . snd) actionPairs
      where
        acs = actionSet st
    
    _actionSet = V.fromList $ map actionSet' states
  in
    CTMDP
    { _states     = _states
    , _actions    = _actions
    , _fixedCosts = _fixedCosts
    , _rateCosts  = _rateCosts
    , _rates      = _rates
    , _trans      = _trans
    , _discount   = discount
    , _actionSet  = _actionSet
    }

-- | Convert a CTMDP into an MDP.
uniformize :: (Ord t, Fractional t) => CTMDP a b t -> MDP a b t
uniformize ctmdc =
  let
    states     = _states ctmdc
    actions    = _actions ctmdc
    trans      = _trans ctmdc
    rateCosts  = _rateCosts ctmdc
    fixedCosts = _fixedCosts ctmdc
    rates      = _rates ctmdc
    actionSet  = _actionSet ctmdc
    discount   = _discount ctmdc

    nStates = length states
    nActions = length actions

    -- The fastest transition rate
    nu = maximum (fmap maximum rates)

    -- The discount factor for the continuous-time problem
    beta = nu * (1 / discount - 1)

    -- We rescale the probabilities by increasing the probability of a
    -- self-transition
    rescaleProb ac s v = V.imap (\t z -> newP t z) v
      where
        newP t z = if s == t
                   then (nu - r + z * r) / (beta + nu)
                   else r * z / (beta + nu)
        r = rates V.! ac V.! s
                             
    trans' = V.imap (\a vv -> V.imap (\s v -> rescaleProb a s v) vv) trans

    -- We create costs that combine fixed and rate costs
    costFor ac s = nu * ((beta + r) * f + rc) / (beta + nu)
      where
        f  = fixedCosts V.! ac V.! s
        rc = rateCosts V.! ac V.! s
        r  = rates V.! ac V.! s

    costs' = V.generate nActions (\ac -> V.generate nStates (costFor ac))

    discount' = nu / (beta + nu)
  in
    MDP states actions costs' trans' discount' actionSet
