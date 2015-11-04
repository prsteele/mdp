import Algorithms.MDP.Examples.Ex_3_1_flat
import Algorithms.MDP.FlatMDP
import Algorithms.MDP.FlatValueIteration

import qualified Data.Vector as V

converging :: Double
              -> (FlatCostFunction Double, FlatCostFunction Double)
              -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = fst (cf V.! 0)
    y = fst (cf' V.! 0)

iterations = valueIteration mdp

main = do
  mapM_ (putStrLn . showAll) $ take 100 iterations
  where
    showCosts cf = [getCost' mdp cf A, getCost' mdp cf B]
    showActions cf = [getAction' mdp cf A, getAction' mdp cf B]
    showAll cf = show (showCosts cf) ++ " " ++ show (showActions cf)
