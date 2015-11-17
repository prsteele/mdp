import Text.Printf
import Control.Monad

import Algorithms.MDP.MDP
import Algorithms.MDP.CTMDP
import Algorithms.MDP.ValueIteration
import Algorithms.MDP.Examples.MM1

printErrors :: MDP State Action Double -> Double -> IO ()
printErrors mdp tol = case verifyStochastic mdp tol of
  Left er -> do
    mapM_ (putStrLn . show) (_negativeProbability er)
    mapM_ (putStrLn . show) (_notOneProbability er)
  Right _ -> return ()

names :: [String]
names =
  [ "Scenario 1"
  , "Scenario 2"
  , "Scenario 3"
  ]

scenarios :: [MDP State Action Double]
scenarios = 
  [ uniformize (mkInstance scenario1)
  , uniformize (mkInstance scenario2)
  , uniformize (mkInstance scenario3)
  ]

costs :: [Double]
costs =
  [ 8.475
  , 21.091
  , 21.091
  ]

gap :: (Num t) => CFBounds a b t -> t
gap (CFBounds _ lb ub) = ub - lb

solution :: Double -> MDP State Action Double -> CFBounds State Action Double
solution tol =
  head . dropWhile ((> tol) . gap) . undiscountedRelativeValueIteration

printSolution :: MDP State Action Double -> Double -> Double -> IO ()
printSolution scenario tol c =
  let
    (CFBounds _ lb ub) = solution tol scenario
    result = if lb <= c && c <= ub
             then printf "  %.3f in [%.3f, %.3f]" c lb ub
             else printf "  %.3f not in [%.3f, %.3f]" c lb ub
  in
    putStrLn result

main :: IO ()
main = do
  forM_ (zip3 names scenarios costs) $ \(name, scenario, c) ->
    do
      putStrLn name
      printErrors scenario 1e-5
      printSolution scenario 1e-3 c
