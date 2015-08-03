import Algorithms.MDP.Examples.Ex_3_2

import Algorithms.MDP.MDP
import Algorithms.MDP.ValueIteration

iterations = relativeValueIteration mdp distinguished 0.25
pairs = zip iterations (tail iterations)

-- | Takes elements from a list while each adjacent pair of elements
-- satisfies the given predicate.
takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p as = map fst $ takeWhile (uncurry p) (zip as (tail as))

distinguished = A

showAll :: DCFAndBounds States Controls -> String
showAll (cf, opt, lb, ub) = unwords [show (cf A), show (cf B), show opt, show lb, show ub]

main = do
  if isStochastic mdp 0
    then mapM_ (putStrLn . showAll) $ map fst $ take 11 pairs
    else do
         putStrLn "Non-stochastic states"
         mapM_ (putStrLn . show) $ nonStochastic mdp 0
