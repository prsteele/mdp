{-# Language FlexibleInstances #-}

import Algorithms.MDP.MDP as Q

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

arbitraryTransitionMatrix :: (Ord a, Ord b) =>
                             [a] -> (a -> [b]) -> Gen (a -> a -> b -> Double)
arbitraryTransitionMatrix states actionSet = let
  pairs = [(s, a) | s <- states, a <- actionSet s]
  count = length pairs
  pdfToMap pdf = Map.fromList (zip states pdf)
  masterMap pdfs = Map.fromList (zip pairs (map pdfToMap pdfs))
  in do
    pdfs <- replicateM count (arbitraryPDF states)
    return $ (\s t a -> ((masterMap pdfs) Map.! (s, a)) Map.! t)

arbitraryCostFunction :: (Ord a, Ord b) =>
                         [a] -> (a -> [b]) -> Gen (a -> b -> Double)
arbitraryCostFunction states actionSet = let
  pairs = [(s, a) | s <- states, a <- actionSet s]
  in do
    costs <- vector (length pairs) `suchThat` (all (>= 0))
    return $ curry ((Map.fromList (zip pairs costs)) Map.!)

arbitraryDiscount :: Gen Double
arbitraryDiscount =
  sized $ \n -> let
    n' = max 2 n
    in do
      val <- choose (1, n')
      return $ (fromIntegral val) / (fromIntegral n')

instance Arbitrary (Q.MDP Int Int) where
  arbitrary = do
    states    <- arbitraryIntegerSequence 1 20
    actions   <- arbitraryIntegerSequence 1 10
    actionSet <- arbitraryActionSet states actions
    costFn    <- arbitraryCostFunction actions actionSet
    transMat  <- arbitraryTransitionMatrix actions actionSet
    discount  <- arbitrary
    return $ mkMDP states actions transMat costFn actionSet discount

prop_idempotent :: Int -> Bool
prop_idempotent x = id x == id x

increasing :: (Ord a) => [a] -> 

prop_increasing :: (Q.MDP a b) -> Bool
prop_increasing mdp = let
  iterations = valueIteration mdp

  increasingFor s = take 10 (map snd (map ($ s) iterations))
  
  values = map 

-- -- prop_isStochastic :: ValidTrans -> Bool
-- -- prop_isStochastic (ValidTrans trans) = let
-- --   mdp = mkMDP states actions trans (\a b -> 0) (\a -> actions) 1.0
-- --   in isStochastic mdp 0

main = quickCheck prop_idempotent
