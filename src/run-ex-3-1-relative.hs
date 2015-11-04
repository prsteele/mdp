import Algorithms.MDP.Examples.Ex_3_1_flat
import Algorithms.MDP.FlatMDP
import Algorithms.MDP.FlatValueIteration

import qualified Data.Vector as V

type CF = FlatCostFunction States Controls Double

converging :: Double -> (CF, CF) -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = (\(_, _, c) -> c) (cf V.! 0)
    y = (\(_, _, c) -> c) (cf' V.! 0)

iterations = relativeValueIteration mdp

main = do
  mapM_ (putStrLn . showAll) $ take 100 iterations
  where
    costs (cf, _, _) = V.map (\(_, _, c) -> c) cf
    actions (cf, _, _) = V.map (\(_, a, _) -> a) cf
    bounds (_, lb, ub) = [lb, ub]
    showAll cf = unwords [show (costs cf), show (bounds cf), show (actions cf)]
