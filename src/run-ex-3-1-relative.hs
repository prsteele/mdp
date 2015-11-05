import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP.MDP
import Algorithms.MDP.DiscountedMDP
import Algorithms.MDP.DiscountedValueIteration

import qualified Data.Vector as V

converging :: Double 
           -> (CF States Controls Double, CF States Controls Double) 
           -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = (\(_, _, c) -> c) (cf V.! 0)
    y = (\(_, _, c) -> c) (cf' V.! 0)

iterations = relativeValueIteration mdp

main = do
  mapM_ (putStrLn . showAll) $ take 100 iterations
  where
    costs (CFBounds cf _ _) = V.map (\(_, _, c) -> c) cf
    actions (CFBounds cf _ _) = V.map (\(_, a, _) -> a) cf
    bounds (CFBounds _ lb ub) = [lb, ub]
    showAll cf = unwords [show (costs cf), show (bounds cf), show (actions cf)]
