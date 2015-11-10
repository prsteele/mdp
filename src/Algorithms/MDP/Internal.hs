module Algorithms.MDP.Internal where

-- | A state in the MDP.
newtype State = State Int
              deriving (Eq, Show)

-- | An action in the MDP.
newtype Action = Action Int
              deriving (Eq, Show)
