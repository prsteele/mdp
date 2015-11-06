import Text.Printf
import qualified Data.Vector as V

import Algorithms.MDP.Examples.LineClient
import Algorithms.MDP.MDP
import Algorithms.MDP.UndiscountedValueIteration

lambdas = enumFromThenTo 0.1 0.2 2

gap (CFBounds h lb ub) = ub - lb

getMDP lam = mkInstance 40 1 lam 1

getCost eps mdp =
  let
    iterations = relativeValueIteration mdp
    (CFBounds _ lb ub) = head . (dropWhile ((> eps) . gap)) $ iterations
  in
    (ub + lb) / 2

getThresholds eps mdp =
  let
    iterations = relativeValueIteration mdp
    (CFBounds h _ _) = head . (dropWhile ((> eps) . gap)) $ iterations
  in
    h

showMDP eps lam =
  let
    c = getCost eps (getMDP lam)
    ts = getThresholds eps (getMDP lam)
  in
    unwords [show lam, printf "%.3f" c]

main = do
  mapM_ (putStrLn . (showMDP 1e-3)) $ lambdas
  putStrLn . show $ getThresholds 1e-3 (getMDP 1.0)
