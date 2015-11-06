import Algorithms.MDP.Examples.LineClient
import Algorithms.MDP.MDP
import Algorithms.MDP.UndiscountedValueIteration

mdp = mkInstance 40 10 1 1

iterations = relativeValueIteration mdp

showAll (CFBounds h lb ub) = unwords [show lb, show ub]

main = do
  mapM_ (putStrLn . showAll) $ take 20 iterations
