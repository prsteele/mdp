import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP
import Algorithms.MDP.ValueIteration

import qualified Data.Vector as V

converging :: Double 
           -> (CF State Control Double, CF State Control Double) 
           -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = (\(_, _, c) -> c) (cf V.! 0)
    y = (\(_, _, c) -> c) (cf' V.! 0)

iterations = valueIteration mdp

main = do
  mapM_ (putStrLn . showAll) $ take 100 iterations
  where
    showCosts cf = V.map (\(_, _, c) -> c) cf
    showActions cf = V.map (\(_, a, _) -> a) cf
    showAll cf = show (showCosts cf) ++ " " ++ show (showActions cf)
