{-# Language FlexibleInstances #-}

import Debug.Trace

import Algorithms.MDP.MDP as Q
import Algorithms.MDP.ValueIteration

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

instance Arbitrary (Q.MDP Int Int) where
  arbitrary = do
    states    <- arbitraryIntegerSequence 1 10
    actions   <- arbitraryIntegerSequence 1 3
    actionSet <- arbitraryActionSet states actions
    costFn    <- arbitraryCostFunction states actionSet
    transMat  <- arbitraryTransitionMatrix states actionSet
    discount  <- arbitraryDiscount
    return $ mkMDP states actions transMat costFn actionSet discount

prop_idempotent :: Int -> Bool
prop_idempotent x = id x == id x

increasing :: (Ord a) => [a] -> Bool
increasing (p:q:qs) = if p <= q
                      then increasing (q:qs)
                      else False
increasing _        = True

prop_increasing :: (Ord a, Ord b) => (Q.MDP a b) -> Bool
prop_increasing mdp = let
  iterations = valueIteration mdp
  valuesFor s = map snd (map ($ s) iterations)

  increasingFor s = increasing (take 10 (valuesFor s))
  in all increasingFor (unStates mdp)

-- -- prop_isStochastic :: ValidTrans -> Bool
-- -- prop_isStochastic (ValidTrans trans) = let
-- --   mdp = mkMDP states actions trans (\a b -> 0) (\a -> actions) 1.0
-- --   in isStochastic mdp 0

example = mkMDP [1] [1, 2] (\a _ _ -> if a == 2 then 1 else 0) (\_ _ -> 2.67) (\_ -> [2]) 0.5

main = quickCheck (prop_increasing :: (Q.MDP Int Int) -> Bool)
