import Algorithms.MDP.MDP

import Control.Monad
import qualified Data.Map as Map
import Test.QuickCheck

import Debug.Trace

-- | Returns an arbitrary non-empty subset of the input.
arbitraryNonEmptySubset :: [a] -> Gen [a]
arbitraryNonEmptySubset xs = let
  narrow flips | or flips = return $ map fst $ filter snd $ zip xs flips
               | otherwise = arbitraryNonEmptySubset xs
  in do
    flips <- vector (length xs) :: Gen [Bool]
    narrow flips

-- | Returns a sequence of the form [1..n], for an arbitrary n in the
-- input range.
arbitraryIntegerSequence :: Int -> Int -> Gen [Int]
arbitraryIntegerSequence lower upper = do
  num <- choose (lower, upper)
  return [1..num]

-- | Returns an arbitrary mapping from the state space to a subset of
-- the action space.
arbitraryActionSet :: (Ord a) => [a] -> [b] -> Gen (a -> [b])
arbitraryActionSet states actions = do
    choices <- replicateM (length states) (arbitraryNonEmptySubset actions)
    return $ ((Map.fromList (zip states choices)) Map.!)

-- | Returns an arbitrary PDF (as a vector of values) over the input
-- vector.
arbitraryPDF :: [a] -> Gen [Double]
arbitraryPDF xs = do
  probs <- (vector (length xs)) `suchThat` (all (>= 0)) :: Gen [Double]
  return [p / (sum probs) | p <- probs]

test :: (Num a, Show a) => [a] -> IO ()
test = putStrLn . show . sum

-- instance Arbitrary (MPD Int Int) where
--   arbitrary = do
--     states <- arbitraryIntegerSequence 1 20
--     actions <- arbitraryIntegerSequence 1 10
--     actionSet <- arbitraryActionSet states actions
--     return $ mkMDP states actions trans cost a
    
    

-- actionSet :: Int -> Int -> Gen (Int -> [Int])
-- actionSet nStates nActions = let
--   states  = [1..nStates]
--   actions = [1..nActions]

--   narrow 

-- instance Arbitrary (MDP Int Int) where
--   arbitrary = let
--     narrow actions flips | any flips = map fst $ filter snd $ zip actions flips
--                          | otherwise = actions

--     actionSet nStates nActions flips = let
--       states  = [1..nStates]
--       actions = [1..nActions]
--       allowed = map (narrow actions) flips
--       in Map.fromList (zip states allowed)

--     in do
--       nStates <- choose (1, 20)
--       nActions <- choose (1, 10)
--       flips <- replicateM nStates (vector nActions :: Gen [Bool])
      
          


-- instance Arbitrary ActionSet' where
--   arbitrary = do
--     nStates <- choose (1, 20)
--     states <- vector nStates :: Gen [Int]
--     nActions <- choose (1, 10)
--     actions <- vector nActions :: Gen [Int]
--     allowed <- replicateM nStates (vector nActions :: Gen [Bool]
--     return (ActionSet' (StateSpace (nStates - 1)) (ActionSpace (nActions - 1)) (\_ -> actions))
    

-- data ValidTrans = ValidTrans (Transition Int Bool)

-- instance Arbitrary ValidTrans where
--    arbitrary = let
--      pairs = [(a, s) | s <- states, a <- actions]
--      size = length states * length actions

--      transMap :: [Double] -> Map.Map (Bool, Int) Double
--      transMap ps = Map.fromList (zip pairs ps)

--      trans :: [Double] -> ValidTrans
--      trans ps = ValidTrans (curry ((transMap ps) Map.!))
     
--      in do
--        ps <- fmap (take size) arbitrary
--        return (trans ps)

-- prop_idempotent :: Int -> Bool
-- prop_idempotent x = id x == id x

-- -- prop_isStochastic :: ValidTrans -> Bool
-- -- prop_isStochastic (ValidTrans trans) = let
-- --   mdp = mkMDP states actions trans (\a b -> 0) (\a -> actions) 1.0
-- --   in isStochastic mdp 0

-- main = quickCheck prop_idempotent
