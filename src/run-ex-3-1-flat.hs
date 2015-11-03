import Algorithms.MDP.Examples.Ex_3_1_flat
import Algorithms.MDP.FlatMDP
import Algorithms.MDP.FlatValueIteration

import qualified Data.Vector as V

converging :: Double
              -> (CostFunction Double, CostFunction Double)
              -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = cf V.! 0
    y = cf' V.! 0

iterations = valueIteration mdp

main = do
  mapM_ (putStrLn . showAll) $ take 100 iterations
  where
    showAll cf = (show (cf V.! 0)) ++ " " ++ (show (cf V.! 1))
