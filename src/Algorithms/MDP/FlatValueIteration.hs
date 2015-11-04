module Algorithms.MDP.FlatValueIteration where

import Control.Monad
import Control.Monad.Trans.Writer.Lazy
import qualified Data.Vector as V
import Data.List (minimumBy)

import Algorithms.MDP.MDP (CostFunction)
import Algorithms.MDP.FlatMDP

inner :: (Num t) => V.Vector t -> V.Vector t -> t
inner u v = V.sum (V.zipWith (*) u v)

valueIteration :: (Ord t, Num t) => FlatMDP a b t -> [FlatCostFunction a b t]
valueIteration mdp =
  let
    states = __states mdp
    actions = __actions mdp
    
    zero = V.replicate (V.length states) (V.head states, V.head actions, 0)
  in
    iterate (valueIterate mdp) zero

valueIterate :: (Ord t, Num t) => FlatMDP a b t -> FlatCostFunction a b t -> FlatCostFunction a b t
valueIterate mdp cf = V.zipWith (choiceFor mdp cf) (__states' mdp) (__states mdp)

choiceFor :: (Ord t, Num t) => FlatMDP a b t -> FlatCostFunction a b t -> State -> a -> (a, b, t)
choiceFor mdp cf (State st) s =
  let
    cmp (_, x) (_, y) = compare x y
    costs = V.map (costForAction mdp cf (State st)) (__actionSet mdp V.! st)
    pairs = V.zip (__actions mdp) costs
    (ac, c) = V.minimumBy cmp pairs
  in
    (s, ac, c)

costForAction :: (Num t) =>
                 FlatMDP a b t -> FlatCostFunction a b t -> State -> Action -> t
costForAction mdp cf (State st) (Action ac) =
  let
    alpha = __discount mdp
    fixedCost = (__costs mdp) V.! ac V.! st
    transCost = inner (__trans mdp V.! ac V.! st) (V.map (\(_, _, c) -> c) cf)
  in
    fixedCost + alpha * transCost
