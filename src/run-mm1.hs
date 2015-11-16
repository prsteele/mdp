import Text.Printf

import Algorithms.MDP.MDP
import Algorithms.MDP.CTMDP
import Algorithms.MDP.ValueIteration
import Algorithms.MDP.Examples.MM1

mdp = uniformize (mkInstance scenario3)

iterations = undiscountedRelativeValueIteration mdp

printErrors mdp tol = case verifyStochastic mdp tol of
  Left er -> do
    mapM_ (putStrLn . show) (_negativeProbability er)
    mapM_ (putStrLn . show) (_notOneProbability er)
  Right _ -> return ()

maxRate = max (_arrivalRate scenario3) (maximum (_serviceRates scenario3))

nIters = 1200

(CFBounds finalCF _ _) = head (drop (nIters - 1) iterations)

main = do
  printErrors mdp 1e-5
  mapM_ (putStrLn . showBounds) $ take nIters iterations
  mapM_ (putStrLn . show) $ finalCF
    where
      showBounds (CFBounds _ lb ub) = printf "%.3f\t%.3f" (maxRate * lb) (maxRate * ub)
