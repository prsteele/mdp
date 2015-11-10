{-# LANGUAGE GADTs, StandaloneDeriving #-}

-- | The main module for modeling Markov decision processes (MDP).
--
-- We are primarily concerned with infinite horizon MDPs, both
-- discounted and undiscounted.
module Algorithms.MDP.MDP where

import Algorithms.MDP.Internal

import Control.Monad
import qualified Data.Vector as V

-- | A type representing an action- and state-dependent probablity
-- vector.
type Transitions a b t = b -> a -> a -> t

-- | A type representing an action- and state-dependent cost.
type Costs a b t = b -> a -> t

-- | A type representing the allowed actions in a state.
type ActionSet a b = a -> [b]

-- | A cost function is a vector containing (state, action, cost)
-- triples. Each triple describes the cost of taking the action in
-- that state.
type CF a b t = V.Vector (a, b, t)

cost :: (Eq a) => a -> CF a b t -> Maybe t
cost s cf =
  do
    (_, _, c) <- V.find (\(s', _, _) -> s == s') cf
    return c

action :: (Eq a) => a -> CF a b t -> Maybe b
action s cf =
  do
    (_, ac, _) <- V.find (\(s', _, _) -> s == s') cf
    return ac

-- | A cost function with error bounds. The cost in a (state, action,
-- cost) triple is guaranteed to be in the range [cost + lb, cost + ub]
data CFBounds a b t = CFBounds
                      { _CF :: CF a b t
                      , _lb :: t
                      , _ub :: t
                      }

-- | A DifferentialCF is an estimate of the long-run optimal average
-- cost per stage along with a differential cost vector describing the
-- deviation of each cost of a given state from the long-run average
-- cost.
data DifferentialCF a b t = DifferentialCF
                            { _cost :: t
                            , _h    :: CF a b t
                            }

-- | A Markov decision process.
--
-- An MDP consists of a state space, an action space, state- and
-- action-dependent costs, and state- and action-dependent transition
-- probabilities. The goal is to compute a policy -- a mapping from
-- states to actions -- which minimizes the total discounted cost of
-- the problem, assuming a given discount factor in the range (0, 1].
data MDP a b t = MDP
                 { _states    :: V.Vector a
                 , _actions   :: V.Vector b
                 , _states'   :: V.Vector State
                 , _actions'  :: V.Vector Action
                 , _costs     :: V.Vector (V.Vector t)
                 , _trans     :: V.Vector (V.Vector (V.Vector t))
                 , _discount  :: t
                 , _actionSet :: V.Vector (V.Vector Action)
                 }

-- -- | Verifies that a 'MDP' has fully stochastic transition
-- -- probabilities.
-- --
-- -- For each state and allowable action in that state, the sum of
-- -- transition probabilities to each other state must sum to 1 (within
-- -- the given tolerance), and all transition probabilities must be
-- -- nonnegative.
-- isStochastic :: (Ord t, Num t) => MDP a b t -> t -> Bool
-- isStochastic mdp tol = null $ nonStochastic mdp tol

-- -- | Returns the non-stochastic (action, state) pairs in an 'MDP'.
-- --
-- -- An (action, state) pair is not stochastic if any transitions out of
-- -- the state occur with negative probability, or if the total
-- -- probability all possible transitions is not 1 (within the given
-- -- tolerance).
-- nonStochastic :: (Ord t, Num t) => MDP a b t -> t -> [(b, a, t)]
-- nonStochastic mdp tol =
--   let
--     stateSpace = unStates mdp
--     actionSet = unActionSet mdp
--     trans = unTransition mdp
--     isStochastic' (action, state) = all (>= 0) v && abs (1 - sum v) <= tol
--       where
--         v = map (trans action state) stateSpace

--     totalProb action state = sum $ map (trans action state) stateSpace

--     pairs = [(action, state) | state <- stateSpace, action <- actionSet state]

--     f (action, state) = (action, state, totalProb action state)

--   in map f (filter (not . isStochastic') pairs)

-- -- | Returns True if the two 'CostFunction's have a large difference
-- -- between them.
-- --
-- -- We compute the 2-norm of the difference of the two functions, and
-- -- return True if the norm is less than the given tolerance.
-- --
-- -- This function can be applied to a list of cost functions to
-- -- determine when the cost functions have converged to within some
-- -- tolerance, e.g.
-- --
-- -- > costFunctions = valueIteration mdp
-- -- > pairs = zip costFunctions (tail costFunctions)
-- -- > solution = head $ dropWhile (uncurry (converging mdp 0.01)) pairs
-- converging :: (Ord t, Floating t) => MDP a b t -> t -> CostFunction a b t -> CostFunction a b t -> Bool
-- converging mdp tol cf cf' =
--   let
--     stateSpace = unStates mdp
--     norm = sum [(snd (cf s) - snd (cf' s)) ** 2 | s <- stateSpace]
--   in norm > tol
