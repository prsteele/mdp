-- | A continuous-time Markov decision chain is a Markov decision
-- process (CTMDP) where an exponential amount of time is spent at
-- each state.
module Algorithms.MDP.CTMDP where

import qualified Data.Vector as V

import Algorithms.MDP.Internal
--import Algorithms.MDP.MDP hiding (MDP (..))
--import qualified Algorithms.MDP.MDP as MDP

import Algorithms.MDP.MDP hiding (MDP (..))
import Algorithms.MDP.MDP (MDP(MDP))

type Rates a b t = b -> a -> t

mkCTMDP :: (Eq b) =>
           [a]                -- ^ The state space
        -> [b]                -- ^ The action space
        -> Transitions a b t  -- ^ The transition probabilities
        -> Rates a b t        -- ^ The transition rates
        -> Costs a b t        -- ^ The action-dependent fixed costs
        -> Costs a b t        -- ^ The action-dependent rate costs
        -> ActionSet a b      -- ^ The state-dependent actions
        -> t                  -- ^ The discount factor
        -> CTMDP a b t        -- ^ The resulting CTMDP
mkCTMDP states actions trans rates fixedCost rateCost actionSet discount =
  let
    _states      = V.fromList states
    _actions     = V.fromList actions
    _states'     = V.fromList (map State [0..length states - 1])
    _actions'    = V.fromList (map Action [0..length actions - 1])

    mkCostVecFor cf ac = V.fromList $ map (cf ac) states
    _fixedCosts = V.fromList $ map (mkCostVecFor fixedCost) actions
    _rateCosts  = V.fromList $ map (mkCostVecFor rateCost)  actions

    mkProbAS a s = V.fromList $ map (trans a s) states
    mkProbA a    = V.fromList $ map (mkProbAS a) states
    _trans = V.fromList $ map mkProbA actions

    mkTransVec ac = V.fromList $ map (rates ac) states
    _rates = V.fromList $ map mkTransVec actions

    actionPairs   = zip (map Action [0..]) actions
    actionSet' st = V.fromList $ map fst $ filter ((`elem` acs) . snd) actionPairs
      where
        acs = actionSet st
    
    _actionSet = V.fromList $ map actionSet' states
  in
    CTMDP
    { _states     = _states
    , _actions    = _actions
    , _states'    = _states'
    , _actions'   = _actions'
    , _fixedCosts = _fixedCosts
    , _rateCosts  = _rateCosts
    , _rates      = _rates
    , _trans      = _trans
    , _discount   = discount
    , _actionSet  = _actionSet
    }

data CTMDP a b t = CTMDP
                   { _states     :: V.Vector a
                   , _actions    :: V.Vector b
                   , _states'    :: V.Vector State
                   , _actions'   :: V.Vector Action
                   , _fixedCosts :: V.Vector (V.Vector t)
                   , _rateCosts  :: V.Vector (V.Vector t)
                   , _rates      :: V.Vector (V.Vector t)
                   , _trans      :: V.Vector (V.Vector (V.Vector t))
                   , _discount   :: t
                   , _actionSet  :: V.Vector (V.Vector Action)
                   }

uniformize :: (Ord t, Fractional t) => CTMDP a b t -> MDP a b t
uniformize ctmdc =
  let
    states     = _states ctmdc
    actions    = _actions ctmdc
    states'    = _states' ctmdc
    actions'   = _actions' ctmdc
    trans      = _trans ctmdc
    rateCosts  = _rateCosts ctmdc
    fixedCosts = _fixedCosts ctmdc
    rates      = _rates ctmdc
    actionSet  = _actionSet ctmdc
    discount   = _discount ctmdc

    selfTrans ac s = trans V.! ac V.! s V.! s

    -- The fastest transition rate
    nu = maximum (fmap maximum rates)

    -- The mean transition time of the fastest transition
    tau = 1 / nu

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
    costFor (Action ac) (State s) = nu * ((beta + r) * f + rc) / (beta + nu)
      where
        f  = fixedCosts V.! ac V.! s
        rc = rateCosts V.! ac V.! s
        r  = rates V.! ac V.! s

    costs' = V.map (\ac -> V.map (costFor ac) states') actions'

    discount' = (1 / tau) / (1 / tau + discount)
  in
    MDP states actions states' actions' costs' trans' discount' actionSet
