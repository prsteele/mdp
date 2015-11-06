import Data.Maybe (fromJust)
import qualified Data.Vector as V

import Algorithms.MDP.Examples.Ex_3_1 hiding (mdp, cost)
import Algorithms.MDP.Examples.Ex_3_2
import Algorithms.MDP.MDP
import Algorithms.MDP.UndiscountedValueIteration

iterations = relativeValueIteration mdp
pairs = zip iterations (tail iterations)

-- | Takes elements from a list while each adjacent pair of elements
-- satisfies the given predicate.
takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 _ [] = []
takeWhile2 p as = map fst $ takeWhile (uncurry p) (zip as (tail as))

distinguished = A

showAll (CFBounds h lb ub) = unwords [show h, show lb, show ub]

main = do
  mapM_ (putStrLn . showAll) $ take 20 iterations

