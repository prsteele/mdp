module Algorithms.MDP.UndiscountedMDP where

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V

import Algorithms.MDP.MDP
import Algorithms.MDP.DiscountedMDP

mkUndiscountedMDP :: (Eq b, Num t) =>
                     [a]                -- ^ The state space
                  -> [b]                -- ^ The action space
                  -> Transitions a b t  -- ^ The transition probabilities
                  -> Costs a b t        -- ^ The action-dependent costs
                  -> ActionSet a b      -- ^ The state-dependent actions
                  -> MDP a b t          -- ^ The resulting DiscountedMDP
mkUndiscountedMDP states actions trans cost actionSet =
  mkDiscountedMDP states actions trans cost actionSet 1
