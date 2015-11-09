-- | A continuous-time Markov decision chain is a Markov decision
-- process (CTMDP) where an exponential amount of time is spent at
-- each state.
module Algorithms.MDP.CTMDP where

import qualified Data.Vector as V

import qualified Algorithms.MDP.MDP as MDP

data CTMDP a b t = CTMDP
                   { _states     :: V.Vector a
                   , _actions    :: V.Vector b
                   , _states'    :: V.Vector MDP.State
                   , _actions'   :: V.Vector MDP.Action
                   , _fixedCosts :: V.Vector (V.Vector t)
                   , _rateCosts  :: V.Vector (V.Vector t)
                   , _rates      :: V.Vector (V.Vector t)
                   , _trans      :: V.Vector (V.Vector (V.Vector t))
                   , _discount   :: t
                   , _actionSet  :: V.Vector (V.Vector MDP.Action)
                   }

uniformize :: (Ord t, Fractional t) => CTMDP' a b t -> MDP.MDP a b t
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
    nu = maximum [ (1 - selfTrans ac s) * (rates V.! ac V.! s)
                 | MDP.State s <- V.toList states'
                 , MDP.Action ac <- V.toList (actionSet V.! s)
                 ]

    -- The mean transition time of the fastest transition
    tau = 1 / nu

    -- The discount factor for the continuous-time problem
    beta = nu * (1 / discount - 1)

    -- We rescale the probabilities by increasing the probability of a
    -- self-transition
    rescaleProb ac s v = V.imap (\t z -> newP t z) v
      where
        newP t z = if s == t
                   then (nu - r + z * r) / (discount + nu)
                   else r * z / (discount + nu)
        coef = tau * (rates V.! ac V.! s)
        nu = 1 / tau
        r = rates V.! ac V.! s
                             
    trans' = V.imap (\a vv -> V.imap (\s v -> rescaleProb a s v) vv) trans

    -- We create costs that combine fixed and rate costs
    costFor (MDP.Action ac) (MDP.State s) = ((beta + r) * f  + rc) / (beta + nu)
      where
        f  = fixedCosts V.! ac V.! s
        rc = rateCosts V.! ac V.! s
        r  = rates V.! ac V.! s

    costs' = V.map (\ac -> V.map (costFor ac) states') actions'

    discount' = (1 / tau) / (1 / tau + discount)
  in
    MDP.MDP states actions states' actions' costs' trans' discount' actionSet
