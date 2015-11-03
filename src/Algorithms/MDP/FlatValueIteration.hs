module Algorithms.MDP.FlatValueIteration where

import qualified Data.Vector as V
import Data.List (minimumBy)

import Algorithms.MDP.FlatMDP

valueIteration :: (Ord t, Num t) => FlatMDP a b t -> [CostFunction t]
valueIteration mdp =
  let
    State nStates = _stateSpace mdp
    zero = V.replicate nStates 0
  in
    iterate (valueIterate mdp) zero

valueIterate :: (Ord t, Num t) => FlatMDP a b t -> CostFunction t -> CostFunction t
valueIterate mdp cf = V.fromList $ map (choiceFor mdp cf) (map State [0..nStates - 1])
  where
    State nStates = _stateSpace mdp

-- | The cost of choosing an action in a state.
costForAction :: (Num t) => FlatMDP a b t -> CostFunction t -> Action -> State -> t
costForAction mdp cf a s = 
  let
    alpha     = _discountFactor mdp
    fixedCost = getCost mdp a s

    prob = (getProbability mdp) a s

    -- This should transform to the following commented code if vector
    -- fusion is taking place.
    --   -- transitionCost = V.ifoldl' (\tot ndx c -> tot + c * prob (State ndx))
    transitionCost = V.sum scaled
      where 
        scaled = V.imap (\ndx c -> c * prob (State ndx)) cf
  in
    fixedCost + alpha * transitionCost

-- | Find the minimum cost that can be achieved in this state
choiceFor :: (Ord t, Num t) => FlatMDP a b t -> CostFunction t -> State -> t
choiceFor mdp cf s =
  let
    State s' = s
    actions = (_actionSet mdp) V.! s'
  in
    minimum [costForAction mdp cf a s | a <- actions]
