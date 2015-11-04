import Algorithms.MDP.Examples.Ex_3_1
import Algorithms.MDP.MDP
import Algorithms.MDP.ValueIteration

converging :: Double
              -> (CostFunction States Controls Double, CostFunction States Controls Double)
              -> Bool
converging tol (cf, cf') = abs (x - y) > tol
  where
    x = snd (cf A)
    y = snd (cf' A)

iterations = valueIteration mdp

main = do
  mapM_ (putStrLn . showAll) $ map fst $ take 100 pairs
  where
    pairs = zip iterations (tail iterations)
    showCosts cf = show $ map (snd . cf) states
    showChoices cf = show $ map (fst . cf) states
    showAll cf = (showCosts cf) ++ " " ++ (showChoices cf)
      
    
    
-- takeWhile (converging 0.01)
