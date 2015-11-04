import Algorithms.MDP.Examples.Ex_3_1_flat
import Algorithms.MDP.FlatMDP
import Algorithms.MDP.FlatValueIteration

import qualified Data.Vector as V

converging :: Double
              -> (FlatCostFunction' States Controls Double, FlatCostFunction' States Controls Double)
              -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = (\(_, _, c) -> c) (cf V.! 0)
    y = (\(_, _, c) -> c) (cf' V.! 0)

iterations = valueIteration' mdp

main = do
  mapM_ (putStrLn . showAll) $ take 100 iterations
  where
    showCosts cf = V.map (\(_, _, c) -> c) cf
    showActions cf = V.map (\(_, a, _) -> a) cf
    showAll cf = show (showCosts cf) ++ " " ++ show (showActions cf)
