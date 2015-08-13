{-# Language FlexibleInstances #-}

-- | This module contains an instance declaration for Arbitrary (MDP Int Int).
module MDPArbitrary where

import Algorithms.MDP.MDP as MDP

-- | Pull in a Show instance to satisfy QuickCheck
import MDPShow

import Test.QuickCheck
import qualified Data.Map as Map
import Control.Monad

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
  probs <- (vector (length xs)) :: Gen [Double]
  return [abs p / (sum (map abs probs)) | p <- probs]

arbitraryTransitionMatrix :: (Ord a, Ord b) =>
                             [a] -> (a -> [b]) -> Gen (b -> a -> a -> Double)
arbitraryTransitionMatrix states actionSet = let
  pairs = [(s, a) | s <- states, a <- actionSet s]
  count = length pairs
  pdfToMap pdf = Map.fromList (zip states pdf)
  masterMap pdfs = Map.fromList (zip pairs (map pdfToMap pdfs))

  trans master a s t = let
    p = Map.lookup (s, a) master >>= Map.lookup t
    in (case p of
          Just z -> z
          Nothing -> 0)
  in do
    pdfs <- replicateM count (arbitraryPDF states)
    return (trans (masterMap pdfs))

arbitraryCostFunction :: (Ord a, Ord b) =>
                         [a] -> (a -> [b]) -> Gen (b -> a -> Double)
arbitraryCostFunction states actionSet = let
  pairs = [(s, a) | s <- states, a <- actionSet s]

  costFn costMap a s = case Map.lookup (s, a) costMap of
    Just z -> z
    Nothing -> 0

  in do
    costs <- vector (length pairs) `suchThat` (all (>= 0))
    --return $ curry ((Map.fromList (zip pairs costs)) Map.!)
    return (costFn (Map.fromList (zip pairs costs)))

arbitraryDiscount :: Gen Double
arbitraryDiscount =
  sized $ \n -> let
    n' = max 2 n
    in do
      val <- choose (1, n')
      return $ (fromIntegral val) / (fromIntegral n')

instance Arbitrary (MDP.MDP Int Int) where
  arbitrary = do
    states    <- arbitraryIntegerSequence 1 10
    actions   <- arbitraryIntegerSequence 1 3
    actionSet <- arbitraryActionSet states actions
    costFn    <- arbitraryCostFunction states actionSet
    transMat  <- arbitraryTransitionMatrix states actionSet
    discount  <- arbitraryDiscount
    return $ mkMDP states actions transMat costFn actionSet discount

  --shrink mdp = let
    
