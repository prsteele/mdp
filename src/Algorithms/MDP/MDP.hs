{-# LANGUAGE GADTs, StandaloneDeriving #-}

-- | The main module for modeling Markov decision processes (MDP).
--
-- The primary data structure is the 'MDP', which consists of a state
-- space, an action space, transition probabilities, state costs, and
-- actions allowed at each state.
--
-- We are primarily concerned with infinite horizon MDPs, both
-- discounted and undiscounted.
module Algorithms.MDP.MDP where

import qualified Data.Map as Map

-- | A Markov decision process with states of type @a@ and actions of type @b@.
--
-- A Markov decision process contains a state space, a set of actions
-- at each state, action-dependent transition probabilities between
-- states, and action-dependent costs for each state.
--
-- Elements of type @a@ are in the state space, while elements of type
-- @b@ are actions or controls.
--
-- The 'Neighbors' function is used to speed up computations, and can
-- be computed automatically using the 'mkMDP' function.
data MDP a b = MDP
               { unStates         :: [a]
               , unActions        :: [b]
               , unTransition     :: Transition a b
               , unCosts          :: StateCost a b
               , unActionSet      :: ActionSet a b
               , unNeighbors      :: Neighbors a b
               , unDiscountFactor :: DiscountFactor
               }

-- | Constructs a new MDP.
--
-- The Neighbor function is computed automatically.
mkMDP :: (Ord a, Ord b) => 
  [a]                 -- ^ The state space
  -> [b]              -- ^ The action space
  -> (Transition a b) -- ^ The transition probabilities
  -> (StateCost a b)  -- ^ The cost of each state
  -> (ActionSet a b)  -- ^ The actions available at each state
  -> DiscountFactor   -- ^ The discount factor
  -> MDP a b          -- ^ The resulting MDP
mkMDP stateSpace actionSpace trans cost actionSet discount =
  let
    neighbors = mkNeighbors stateSpace trans actionSet
  in MDP { unStates         = stateSpace
         , unActions        = actionSpace
         , unTransition     = trans
         , unCosts          = cost
         , unActionSet      = actionSet
         , unNeighbors      = neighbors
         , unDiscountFactor = discount
         }

-- | Computes a 'Neighbors' function from the state space, transition
-- probablities, and allowed actions.
--
-- Since 'b' is a neighbor of 'a' if the transition probability from
-- 'a' to 'b' is positive for some action, the function can be
-- computed automatically from the provided information.
mkNeighbors :: (Ord a, Ord b) =>
               [a] -- ^ The state space
               -> (Transition a b) -- ^ The transition probabilities
               -> (ActionSet a b) -- ^ The actions available at each state
               -> Neighbors a b -- ^ The implied neighbors function
mkNeighbors stateSpace trans actionSet = let
  neighbors a s = [(t, trans a s t) | t <- stateSpace, trans a s t > 0]
  neighborMap = Map.fromList $ [((a, s), neighbors a s)
                               | s <- stateSpace, a <- actionSet s]
  in curry (neighborMap Map.!)

-- | A transition function describes the action-dependent transition
-- probabilities between origin and destination states.
type Transition a b = b -> a -> a -> Double

-- | A cost function describes the action-dependent cost of being in a
-- state.
type StateCost a b = b -> a -> Double

-- | An action set describes the set of actions that can be taken in a
-- given state.
type ActionSet a b = a -> [b]

-- | A neighbors function describes the action-dependent set of states
-- that can be transitioned to from an origin state, along with the
-- (positive) probabilities of each transition.
--
-- Note that this function can be inferred from a state space, an
-- action space, and a 'Transition' function; in fact, this function
-- is computed automatically when using the 'mkMDP' convenience
-- constructor.
type Neighbors a b = b -> a -> [(a, Double)]

-- | A discount factor in the interval (0, 1].
type DiscountFactor = Double

-- | A CostFunction is a mapping from states to the action to be taken
-- in that state to achieve the specified payoff.
type CostFunction a b = a -> (b, Double)

-- | Creates a cost function that maps each state to an arbitrary
-- action and zero cost.
mkZeroCostFunction :: (Ord a, Ord b) => MDP a b -> CostFunction a b
mkZeroCostFunction mdp =
  let
    stateSpace = unStates mdp
    zero       = (head (unActions mdp), 0)
    pairs      = map (\x -> (x, zero)) stateSpace
  in ((Map.fromList pairs) Map.!)

-- | Verifies that a 'MDP' has fully stochastic transition
-- probabilities.
--
-- For each state and allowable action in that state, the sum of
-- transition probabilities to each other state must sum to 1 (within
-- the given tolerance), and all transition probabilities must be
-- nonnegative.
isStochastic :: MDP a b -> Double -> Bool
isStochastic mdp tol = null $ nonStochastic mdp tol

-- | Returns the non-stochastic (action, state) pairs in an 'MDP'.
--
-- An (action, state) pair is not stochastic if any transitions out of
-- the state occur with negative probability, or if the total
-- probability all possible transitions is not 1 (within the given
-- tolerance).
nonStochastic :: MDP a b -> Double -> [(b, a, Double)]
nonStochastic mdp tol =
  let
    stateSpace = unStates mdp
    actionSet = unActionSet mdp
    trans = unTransition mdp
    isStochastic' (action, state) = all (>= 0) v && abs (1 - sum v) <= tol
      where
        v = map (trans action state) stateSpace

    totalProb action state = sum $ map (trans action state) stateSpace

    pairs = [(action, state) | state <- stateSpace, action <- actionSet state]

    f (action, state) = (action, state, totalProb action state)

  in map f (filter (not . isStochastic') pairs)

-- | Returns True if the two 'CostFunction's have a large difference
-- between them.
--
-- We compute the 2-norm of the difference of the two functions, and
-- return True if the norm is less than the given tolerance.
--
-- This function can be applied to a list of cost functions to
-- determine when the cost functions have converged to within some
-- tolerance, e.g.
--
-- > costFunctions = valueIteration mdp
-- > pairs = zip costFunctions (tail costFunctions)
-- > solution = head $ dropWhile (uncurry (converging mdp 0.01)) pairs
converging :: MDP a b -> Double -> CostFunction a b -> CostFunction a b -> Bool
converging mdp tol cf cf' =
  let
    stateSpace = unStates mdp
    norm = sum [(snd (cf s) - snd (cf' s)) ** 2 | s <- stateSpace]
  in norm > tol
