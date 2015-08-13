-- | This module contains an instance of Show for MDP.
--
-- This instance is defined here because (in general) we probably
-- don't want a Show instance for MDP since it is so ugly. The
-- ugliness primarily comes from the constructor requiring function
-- arguments. However, reglardless of how we actually store the
-- functions, explicitly representing all values could create huge
-- strings. Take for example the transition matrix, which has size
-- (State space) x (State space) x (Action space).
module MDPShow where

import Algorithms.MDP.MDP as MDP

instance (Show a, Show b) => Show (MDP.MDP a b) where
  show mdp = unlines [ unwords ["States" , show (MDP.unStates mdp)]
                     , unwords ["Actions" , show (MDP.unActions mdp)]
                     , "Transition probabilities:"
                     , showTransitionMatrix mdp
                     , "Allowed actions:"
                     , showActionSet mdp
                     , "Costs:"
                     , showCosts mdp
                     , unwords ["Discount factor", show (MDP.unDiscountFactor mdp)]
                     ]

showTransitionMatrix :: (Show a, Show b) => MDP.MDP a b -> String
showTransitionMatrix mdp = let
  triples = [(s, t, a) | s <- unStates mdp, t <- unStates mdp, a <- unActions mdp]
  trans = unTransition mdp
  in unlines [unwords [show (s, t, a), show (trans a s t)] | (s, t, a) <- triples]

showActionSet :: (Show a, Show b) => MDP.MDP a b -> String
showActionSet mdp = let
  states = MDP.unStates mdp
  actionSet = MDP.unActionSet mdp
  in unlines [unwords [show s, show (actionSet s)] | s <- states]

showCosts :: (Show a, Show b) => MDP.MDP a b -> String
showCosts mdp = let
  costFn  = MDP.unCosts mdp
  states  = MDP.unStates mdp
  actions = MDP.unActions mdp
  actionSet = unActionSet mdp
  in unlines [unwords [show s, show (costFn a s)] | s <- states, a <- actionSet s]
